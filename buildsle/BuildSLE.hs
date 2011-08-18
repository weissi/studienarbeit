{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Prelude
import Control.Monad (foldM, liftM)
import Control.Monad.Error (MonadError(), throwError)
import Data.List ( nub, isSuffixOf, foldl', groupBy, sort, (\\)
                 , intercalate, isPrefixOf)
import Data.Map (Map)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Math.Statistics (mean, stddev)
import Safe (readMay)
import System.Console.CmdLib
import System.Exit (exitFailure)
import System.FilePath.Posix (takeFileName)
import System.IO (hPutStrLn, stderr)
import Text.ProtocolBuffers.Basic (uToString)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Foldable as F
import qualified Data.Map as Map

import Text.ProtocolBuffers.WireMessage (messageGet)
import qualified HsPerfCounters.CounterData as Pb
import qualified HsPerfCounters.Timestamp as Pb
import HsPerfCounters.CounterValue(CounterValue(..))

{-
import Debug.Trace
strace a = trace (show a) a
-}

type ShotGroupId = String
data ShotId = ShotId { si_shotGroupId :: ShotGroupId
                     , si_subShotId :: Maybe String
                     , si_groupChar :: Maybe Char
                     } deriving (Eq, Ord)
type CtrName = String
type CtrVal = Integer
type TimeNanosecs = Double

instance Show ShotId where
    show (ShotId shotGroupId mSubShotId mGroupChar) =
        showString shotGroupId $
        showString (maybe "" (:[]) mGroupChar) $
        fromMaybe "" mSubShotId

type CounterMap = Map CtrName CtrVal
type ShotCounterMap = Map ShotId (CounterMap, TimeNanosecs)
type ShotWorkMap = Map ShotId Double
type ShotDataMap = Map ShotId (CounterMap, TimeNanosecs, Double)
type ShotGroupDataMap = Map ShotGroupId (CounterMap, TimeNanosecs, Double)

type FilePath' = String
type WorkFileMap = Map ShotId FilePath'

_NaN_ :: Fractional a => a
_NaN_ = 0/0

_REF_COL_ :: String
_REF_COL_ = "CPU_CLK_UNHALTED"

_ERROR_REL_STDDEV_ :: Double
_ERROR_REL_STDDEV_ = 0.005

relstddev :: Floating a => [a] -> a
relstddev as = stddev as / mean as

tr :: Eq a => a -> a -> [a] -> [a]
tr a b = map f
         where f c = if c == a then b else c

rCompatibleCounterName :: CtrName -> String
rCompatibleCounterName = tr ':' '.'

internalCounterName :: String -> CtrName
internalCounterName = tr '.' ':'

tt_fst :: (a, b, c) -> a
tt_fst (a, _, _) = a

tt_snd :: (a, b, c) -> b
tt_snd (_, b, _) = b

tt_trd :: (a, b, c) -> c
tt_trd (_, _, c) = c

loadCounterDataProto :: FilePath' -> IO (Maybe Pb.CounterData)
loadCounterDataProto fn =
    do pbs <- BSL.readFile fn
       case messageGet pbs of
         Right (cd, _) ->
             return (Just cd)
         Left err ->
             do hPutStrLn stderr $ "ERROR on file `" ++ fn ++ "': " ++ err
                return Nothing
runTime :: Pb.CounterData -> TimeNanosecs
runTime cd = fromIntegral $ stopNs - startNs
    where calcNs :: Pb.Timestamp -> Integer
          calcNs ts = (1000000000 * (fromIntegral $ Pb.sec ts))
                      + (fromIntegral $ Pb.nsec ts)
          stopNs :: Integer
          stopNs = calcNs $ Pb.stop_time cd
          startNs :: Integer
          startNs = calcNs $ Pb.start_time cd

addShotData :: MonadError String m
            => (String -> ShotId)
            -> ShotCounterMap
            -> Pb.CounterData
            -> m ShotCounterMap
addShotData shotIdFun shotMap cd =
   do if shotId `Map.member` shotMap
        then throwError $ "Duuplicate shotID: " ++ show shotId
        else return $ Map.insert shotId (ctrMap', runTime cd) shotMap
    where extractVal :: CounterMap -> CounterValue -> CounterMap
          extractVal cmap cv =
             Map.insert (uToString $ counter_name cv)
                        (F.sum $ fmap toInteger $ counter_value_per_cpu cv)
                        cmap
          ctrMap' :: CounterMap
          ctrMap' = F.foldl' extractVal Map.empty (Pb.counters cd)
          shotId :: ShotId
          shotId = shotIdFun $ uToString $ Pb.shot_id cd

shotCounterMap :: MonadError String m
               => (String -> ShotId)
               -> [Pb.CounterData]
               -> m ShotCounterMap
shotCounterMap shotIdFun cds = foldM (addShotData shotIdFun) Map.empty cds

allCounters :: ShotDataMap -> [CtrName]
allCounters sm = nub $ concat $ map (Map.keys . tt_fst) $ Map.elems sm

printRTAB :: Bool -> ShotDataMap -> IO ()
printRTAB ratios sm =
    do putStr "SHOT_ID\t"
       putStr "WORK\t"
       putStr "TIME_NANO_DIFF\t"
       mapM_ (\c -> putStr $ c ++ "\t") allCtrs
       if ratios
          then mapM_ (\c -> putStr $ "RATIO_" ++ c ++ "\t") allCtrs
          else return ()
       putStr "\n"
       mapM_ printShot (Map.keys sm)
       where allCtrs = allCounters sm
             valueMap :: ShotId -> Map CtrName CtrVal
             valueMap sid =
                 Map.findWithDefault Map.empty sid $ Map.map tt_fst sm

             strValueMap :: ShotId -> Map CtrName String
             strValueMap sid =
                 Map.map show $ valueMap sid

             referenceValue :: ShotId -> Double
             referenceValue sid =
                 Map.findWithDefault _NaN_
                                     _REF_COL_
                                     (Map.map fromIntegral (valueMap sid))

             strRatioMap :: ShotId -> Map CtrName String
             strRatioMap sid =
                 Map.map (\v -> show (fromIntegral v / referenceValue sid))
                         (valueMap sid)

             printCounter :: ShotId -> String -> IO ()
             printCounter sid c =
                 putStr ((Map.findWithDefault "NA\t" c (strValueMap sid))
                                    ++ "\t")

             printCounterRatio :: ShotId -> String -> IO ()
             printCounterRatio sid c =
                 putStr ((Map.findWithDefault "NA\t" c (strRatioMap sid))
                                    ++ "\t")

             printShot :: ShotId -> IO ()
             printShot sid =
                 do putStr $ (show sid) ++ "\t"
                    putStr (Map.findWithDefault "NA" sid
                                                (Map.map (show.tt_trd) sm))
                    putStr "\t"
                    putStr (Map.findWithDefault "NA" sid
                                                (Map.map (show.tt_snd) sm))
                    putStr "\t"
                    mapM_ (printCounter sid) allCtrs
                    if ratios
                       then mapM_ (printCounterRatio sid) allCtrs
                       else return ()
                    putStr "\n"

calcWorkFile :: ShotId -> FilePath'
calcWorkFile sid = showString "work_" $ shows sid ".work"

isCounterFile :: FilePath' -> Bool
isCounterFile f = ".ctrs" `isSuffixOf` takeFileName f

findWorkFile :: ShotId -> WorkFileMap -> Maybe FilePath'
findWorkFile sid files = Map.lookup sid files

getWork :: WorkFileMap -> ShotId -> IO (Maybe Double)
getWork files sid =
    case findWorkFile sid files of
      Just file ->
          do val <- readFile file
             return $ readMay val
      Nothing -> return Nothing

shotDataMap :: ShotCounterMap -> WorkFileMap -> IO ShotDataMap
shotDataMap scmap files =
    do mWorks <- mapM (getWork files) sids
       return $ Map.intersectionWith joinData scmap (swmap mWorks)
       where sids = Map.keys scmap
             check :: (ShotId, Maybe Double) -> Maybe (ShotId, Double)
             check (f, mS) =
                 case mS of
                   Nothing -> Nothing
                   Just s -> Just (f, s)
             swmap :: [Maybe Double] -> ShotWorkMap
             swmap w = Map.fromList $ mapMaybe check (zip sids w)
             joinData :: (a, b) -> c -> (a, b, c)
             joinData (a, b) c = (a, b, c)

shotIdFromString :: Maybe Char -> String -> ShotId
shotIdFromString mGroupChar s =
    case mGroupChar of
      Nothing -> ShotId s Nothing Nothing
      Just gc ->
         if gc `elem` s
           then ShotId { si_shotGroupId = (takeWhile ((/=) gc) s)
                       , si_subShotId = Just (drop 1 (dropWhile ((/=) gc) s))
                       , si_groupChar = mGroupChar
                       }
           else ShotId s Nothing Nothing

shotGroupIds :: [ShotId] -> [[ShotId]]
shotGroupIds sids = groupBy (\l r -> si_shotGroupId l == si_shotGroupId r)
                            (sort sids)

groupShotDataMap :: MonadError String m
                 => ShotDataMap
                 -> Double
                 -> m ShotGroupDataMap
groupShotDataMap sdm maxRelErr =
    do groupNames <- sequence $ map shotGroupname groupedShotIds
       shotGroupData <- sequence $ map accumShotDataMap groupedShotIds
       return $ Map.fromList $ zip groupNames shotGroupData
    where
    groupedShotIds :: [[ShotId]]
    groupedShotIds = shotGroupIds $ Map.keys sdm
    accumCounterMaps :: [CounterMap] -> CounterMap
    accumCounterMaps cms = foldl' Map.union Map.empty cms
    accumDouble :: MonadError String m => [ShotId] -> [Double] -> m Double
    accumDouble sids ws =
        if relstddev ws > maxRelErr
           then fail $ "relative standard deviation (" ++ (show $ relstddev ws)
                       ++ ") too high, shot ids: " ++ (show sids)
                       ++ ", values: " ++ (show ws)
           else return $ mean ws
    shotGroupname :: MonadError String m => [ShotId] -> m ShotGroupId
    shotGroupname sids =
        case sids of
          [] -> throwError "empty shot group --> cannot generate group id"
          firstShot:_ -> return $ si_shotGroupId firstShot
    shotListData :: [ShotId] -> ([CounterMap], [TimeNanosecs], [Double])
    shotListData sids = unzip3 $ mapMaybe (flip Map.lookup sdm) sids
    accumShotDataMap :: MonadError String m
                     => [ShotId]
                     -> m (CounterMap, TimeNanosecs, Double)
    accumShotDataMap sids =
       do let counters = accumCounterMaps $ tt_fst (shotListData sids)
          time <- accumDouble sids $ tt_snd (shotListData sids)
          work <- accumDouble sids $ tt_trd (shotListData sids)
          return (counters, time, work)

data MainOptions =
     MainOptions { mo_groupChar :: String
                 , mo_maxRelStdDev :: String
                 , mo_printRefCols :: Bool
                 , mo_counterFile :: String
                 , mo_fileArgs :: [String]
                 } deriving (Typeable, Data, Eq)
_EMPTY_MAIN_OPTIONS_ :: MainOptions
_EMPTY_MAIN_OPTIONS_ = MainOptions "" "" False "" []

instance Attributes MainOptions where
    attributes _ =
        group "Options"
            [ mo_groupChar    %> [ Help "Group shot ids by common prefix"
                                 , ArgHelp "GROUP-CHAR"
                                 , Default ("" :: String)
                                 , Long ["group-char"]
                                 , Short ['g']
                                 ]
            , mo_maxRelStdDev %> [ Help $ "Maximal relative standard deviation "
                                          ++ "when accumulating work"
                                 , Default _ERROR_REL_STDDEV_
                                 , ArgHelp "MAX-REL-STDDEV"
                                 , Long [ "maximimal-relative-stddev"
                                        , "max-rel-stddev"
                                        ]
                                 , Short ['m']
                                 ]
            , mo_printRefCols %> [ Help "Print reference columns"
                                 , Default True
                                 , Long ["print-ref-columns", "print-ref-cols"]
                                 , Short ['p']
                                 ]
            , mo_counterFile %>  [ Help "Only repect counters listed in file"
                                 , Default ("" :: String)
                                 , Long ["counter-file"]
                                 , Short ['c']
                                 ]
            , mo_fileArgs     %> Extra True
            ]

instance RecordCommand MainOptions where
    mode_summary _ = "BuildSLE"

readGoodCounters :: FilePath' -> IO [CtrName]
readGoodCounters f =
    do contents <- readFile f
       return $ map internalCounterName $ lines contents

delBadCounters :: [CtrName] -> CounterMap -> CounterMap
delBadCounters ctrs cmap = foldl' (flip Map.delete) cmap badCtrs
    where badCtrs = Map.keys cmap \\ ctrs

workFilesMap :: Maybe Char -> [FilePath'] -> WorkFileMap
workFilesMap gc fps =
    foldl' addFile Map.empty fps
    where
        addFile :: WorkFileMap -> FilePath' -> WorkFileMap
        addFile fm fp =
            if isWorkFile
               then Map.insert shotId fp fm
               else fm
            where
                shotId = {-# SCC "sI" #-} shotIdFromString gc sShotId
                sShotId = {-# SCC "eSI" #-} takeWhile (/= '.') $
                              drop 1 $ dropWhile (/= '_') fn
                isWorkFile = {-# SCC "wfm:isWF" #-} "work_" `isPrefixOf` fn
                fn = {-# SCC "wfm:takeFileName" #-} takeFileName fp

main :: IO ()
main = getArgs >>= executeR _EMPTY_MAIN_OPTIONS_ >>= \opts ->
    do let opt_files = mo_fileArgs opts
           opt_counterFile =
               case mo_counterFile opts of
                 "" -> Nothing
                 s -> Just s
       mGroupChar <-
           case mo_groupChar opts of
             g : [] -> return $ Just g
             [] -> return Nothing
             _ ->
                 do hPutStrLn stderr "WARNING: group char ignored (length)"
                    return Nothing
       let workFiles :: WorkFileMap
           workFiles = workFilesMap mGroupChar opt_files
       hPutStrLn stderr $ "Processed work files: " ++ show (Map.size workFiles)
       maxRelStdDev <-
           case readMay $ mo_maxRelStdDev opts of
             Just v -> return v
             Nothing ->
                 do hPutStrLn stderr "WARNING: max rel stddev param ignored"
                    return _ERROR_REL_STDDEV_
       mCDs <- mapM loadCounterDataProto $ filter isCounterFile opt_files
       let eSmap = shotCounterMap (shotIdFromString mGroupChar) (catMaybes mCDs)
       smapRaw <- case eSmap of
                 Right r -> return r
                 Left err -> putStrLn err >> exitFailure
       goodCounters <-
           case opt_counterFile of
             Just f -> liftM Just $ readGoodCounters f
             Nothing -> return Nothing
       let smap = case goodCounters of
             Just gcs -> Map.map (\(a,b) -> (delBadCounters gcs a, b)) smapRaw
             Nothing -> smapRaw
           all_arg_files_basenames = map takeFileName opt_files
           all_sids = Map.keys smap
           work_file_available :: ShotId -> Bool
           work_file_available sid = calcWorkFile sid `elem`
                                     all_arg_files_basenames
           good_sids = filter work_file_available all_sids
           bad_sids = all_sids \\ good_sids
           smap' = foldr Map.delete smap bad_sids
       sdmap <- shotDataMap smap' workFiles
       hPutStrLn stderr $ "group char: " ++ (show mGroupChar)
       --hPutStrLn stderr $ "GOOD sids: " ++ (show $ Map.keys sdmap)
       hPutStrLn stderr $ "Ignored sids: " ++ (show bad_sids)
       --hPutStrLn stderr $ "leaps <- regsubsets(unlist(t['WORK'])~" ++
       --                   (intercalate "+" (map rCompatibleCounterName
       --                   (allCounters sdmap))) ++
       --                   ", data=t, force.in=c('CPU_CLK_UNHALTED', " ++
       --                   "'INST_RETIRED'), nbest=10)"
       case groupShotDataMap sdmap maxRelStdDev of
         Left err -> putStrLn err
         Right gsdmap ->
             do printRTAB (mo_printRefCols opts) $
                 case mGroupChar of
                   Nothing -> sdmap
                   Just _ -> Map.mapKeys (\k -> ShotId k Nothing Nothing) gsdmap
       return ()
