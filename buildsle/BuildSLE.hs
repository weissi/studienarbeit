{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Prelude
import System.Console.CmdLib ((%>))
import Control.Monad (foldM, liftM, when, replicateM_)
import Control.Monad.Error (MonadError(), throwError)
import Data.List ( nub, isSuffixOf, foldl', groupBy, sort, (\\)
                 , intercalate, isPrefixOf, concatMap, group)
import Data.Map (Map)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Math.Statistics (mean, stddev)
import Safe (readMay)
import System.Exit (exitFailure)
import System.FilePath.Posix (takeFileName)
import System.IO (hPutStrLn, stderr)
import Text.ProtocolBuffers.Basic (uToString)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Foldable as F
import qualified Data.Map as Map
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T
import qualified System.Console.CmdLib as CL

import Text.ProtocolBuffers.WireMessage (messageGet)
import qualified HsPerfCounters.CounterData as Pb
import qualified HsPerfCounters.Timestamp as Pb
import qualified HsPerfCounters.CounterValue as Pb

{-
import Debug.Trace
strace a = trace (show a) a
-}

type ShotGroupId = String
data ShotId = ShotId { si_shotGroupId :: ! ShotGroupId
                     , si_subShotId :: ! (Maybe String)
                     , si_groupChar :: ! (Maybe Char)
                     } deriving (Eq, Ord)
type CtrName = String
type CtrVal = Integer
type TimeNanosecs = Double

instance Show ShotId where
    show (ShotId shotGroupId mSubShotId mGroupChar) =
        showString shotGroupId $
        showString (maybe "" (:[]) mGroupChar) $
        fromMaybe "" mSubShotId

data ShotData = ShotData
    { sd_counterMap :: ! CounterMap
    , sd_time :: ! TimeNanosecs
    , sd_numCPUs :: ! Int
    , sd_work :: ! Double
    }

type CounterMap = Map CtrName CtrVal
type ShotCounterMap = Map ShotId (CounterMap, TimeNanosecs, Int)
type ShotWorkMap = Map ShotId Double
type ShotDataMap = Map ShotId ShotData
type ShotGroupDataMap = Map ShotGroupId ShotData

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
          calcNs ts = 1000000000 * fromIntegral (Pb.sec ts)
                      + fromIntegral (Pb.nsec ts)
          stopNs :: Integer
          stopNs = calcNs $ Pb.stop_time cd
          startNs :: Integer
          startNs = calcNs $ Pb.start_time cd

addShotData :: MonadError String m
            => Bool
            -> (String -> ShotId)
            -> ShotCounterMap
            -> Pb.CounterData
            -> m ShotCounterMap
addShotData pfCPU shotIdFun shotMap cd =
   if shotId `Map.member` shotMap
     then throwError $ "Duplicate shotID: " ++ show shotId
     else return $ Map.insert shotId
                              (ctrMap', runTime cd, numCPUs)
                              shotMap
     where insertVals :: CounterMap -> Pb.CounterValue -> CounterMap
           insertVals cmap cv =
               if pfCPU
                  then cmap `Map.union` Map.fromList (postfixedCtrVals cv)
                  else Map.insert (ctrName cv) (sum $ ctrVals cv) cmap
           postfixedCtrVals :: Pb.CounterValue -> [(CtrName, CtrVal)]
           postfixedCtrVals cv = zip (postfixedCtrNames cv)
                                     (ctrVals cv)
           ctrVals :: Pb.CounterValue -> [Integer]
           ctrVals = F.toList . fmap toInteger . Pb.counter_value_per_cpu
           ctrName :: Pb.CounterValue -> String
           ctrName = uToString . Pb.counter_name
           postfixedCtrNames :: Pb.CounterValue -> [String]
           postfixedCtrNames cv = map (showString (ctrName cv) .
                                       showString ":CPU" .
                                       show) [1..]
           ctrMap' :: CounterMap
           ctrMap' = F.foldl' insertVals Map.empty (Pb.counters cd)
           shotId :: ShotId
           shotId = shotIdFun $ uToString $ Pb.shot_id cd
           numCPUs :: Int
           numCPUs = fromIntegral $ Pb.cpu_count cd

shotCounterMap :: MonadError String m
               => Bool
               -> (String -> ShotId)
               -> [Pb.CounterData]
               -> m ShotCounterMap
shotCounterMap pfCPU shotIdFun = foldM (addShotData pfCPU shotIdFun) Map.empty

allCounters :: ShotDataMap -> [CtrName]
allCounters sm = nub $ concatMap (Map.keys . sd_counterMap) (Map.elems sm)

printRTAB :: Bool -> ShotDataMap -> IO ()
printRTAB ratios sm =
    do putStr "SHOT_ID\t"
       putStr "WORK\t"
       putStr "TIME_NANO_DIFF\t"
       mapM_ (\t -> putStr $ "CPU" ++ show t ++ "_TIME_NANOS\t") [1..maxCPUs]
       mapM_ (\c -> putStr $ c ++ "\t") allCtrs
       when ratios $
           mapM_ (\c -> putStr $ "RATIO_" ++ c ++ "\t") allCtrs
       putStr "\n"
       mapM_ printShot (Map.keys sm)
       where maxCPUs :: Int
             maxCPUs = Map.fold (max . sd_numCPUs) 0 sm
             allCtrs = allCounters sm
             valueMap :: ShotId -> Map CtrName CtrVal
             valueMap sid =
                 Map.findWithDefault Map.empty sid $ Map.map sd_counterMap sm

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
                 putStr (Map.findWithDefault "NA\t" c (strValueMap sid)
                         ++ "\t")

             printCounterRatio :: ShotId -> String -> IO ()
             printCounterRatio sid c =
                 putStr (Map.findWithDefault "NA\t" c (strRatioMap sid)
                         ++ "\t")

             printShot :: ShotId -> IO ()
             printShot sid =
                 do putStr $ show sid ++ "\t"
                    putStr (Map.findWithDefault "NA" sid
                                                (Map.map (show.sd_work) sm))
                    putStr "\t"
                    putStr rTime
                    putStr "\t"
                    replicateM_ numCPUs $ putStr (rTime ++ "\t")
                    replicateM_ (maxCPUs-numCPUs) $ putStr "0\t"
                    mapM_ (printCounter sid) allCtrs
                    when ratios $
                       mapM_ (printCounterRatio sid) allCtrs
                    putStr "\n"
                 where numCPUs :: Int
                       numCPUs = Map.findWithDefault 0 sid
                                                     (Map.map sd_numCPUs sm)
                       rTime :: String
                       rTime = Map.findWithDefault "NA" sid
                                                   (Map.map (show.sd_time) sm)

calcWorkFile :: ShotId -> FilePath'
calcWorkFile sid = showString "work_" $ shows sid ".work"

isCounterFile :: FilePath' -> Bool
isCounterFile f = ".ctrs" `isSuffixOf` takeFileName f

findWorkFile :: ShotId -> WorkFileMap -> Maybe FilePath'
findWorkFile = Map.lookup

getWork :: WorkFileMap -> ShotId -> IO (Maybe Double)
getWork files sid =
    case findWorkFile sid files of
      Nothing -> return Nothing
      Just file ->
          do val <- T.readFile file
             case T.double val of
               Right (v, _) -> return $ Just v
               Left _ -> return Nothing

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
             joinData :: (CounterMap, TimeNanosecs, Int) -> Double -> ShotData
             joinData (a, b, c) d =
                 ShotData { sd_counterMap = a
                          , sd_time = b
                          , sd_numCPUs = c
                          , sd_work = d
                          }

shotIdFromString :: Maybe Char -> String -> ShotId
shotIdFromString mGroupChar s =
    case mGroupChar of
      Nothing -> ShotId s Nothing Nothing
      Just gc ->
         if gc `elem` s
           then ShotId { si_shotGroupId = takeWhile (gc /=) s
                       , si_subShotId = Just (drop 1 (dropWhile (gc /=) s))
                       , si_groupChar = mGroupChar
                       }
           else ShotId s Nothing Nothing

shotGroupIds :: [ShotId] -> [[ShotId]]
shotGroupIds sids = groupBy (\l r -> si_shotGroupId l == si_shotGroupId r)
                            (sort sids)

fromEitherAccum :: forall k b. b -> [k] -> k -> Maybe b -> ([k], b)
fromEitherAccum dfl acc key m =
    case m of
      Nothing -> (key:acc, dfl)
      Just v -> (acc, v)

groupShotDataMap :: MonadError String m
                 => ShotDataMap
                 -> Double
                 -> m ShotGroupDataMap
groupShotDataMap sdm maxRelErr =
    do groupNames <- mapM shotGroupname groupedShotIds
       shotGroupData <- mapM accumShotDataMap groupedShotIds
       return $ Map.fromList $ zip groupNames shotGroupData
    where
    groupedShotIds :: [[ShotId]]
    groupedShotIds = shotGroupIds $ Map.keys sdm
    mCounterMaps :: Monad m => [CounterMap] -> [Map CtrName (m CtrVal)]
    mCounterMaps = map (Map.map return)
    accumCounterMaps :: MonadError String m => [CounterMap] -> m CounterMap
    accumCounterMaps cms =
       case maybeMap of
         ([], m) -> return m
         (errs, _) -> throwError $ "multiple values for counters: " ++ show errs
         where maybeMap =
                   Map.mapAccumWithKey (fromEitherAccum (-1)) []
                       (foldl' (Map.unionWith (\_ _ -> Nothing))
                              Map.empty $
                              mCounterMaps cms :: Map CtrName (Maybe CtrVal))
    accumDouble :: MonadError String m => [ShotId] -> [Double] -> m Double
    accumDouble sids ws =
        if relstddev ws > maxRelErr
           then throwError $ "relative standard deviation (" ++
                             show (relstddev ws)
                             ++ ") too high, shot ids: " ++ show sids
                             ++ ", values: " ++ show ws
           else return $ mean ws
    accumCPUs :: MonadError String m => [ShotId] -> [Int] -> m Int
    accumCPUs sids cpus = if length (group cpus) /= 1
                             then throwError "unequal CPU count!"
                             else return $ head cpus

    shotGroupname :: MonadError String m => [ShotId] -> m ShotGroupId
    shotGroupname sids =
        case sids of
          [] -> throwError "empty shot group --> cannot generate group id"
          firstShot:_ -> return $ si_shotGroupId firstShot
    shotListData :: [ShotId] -> [ShotData]
    shotListData = mapMaybe (`Map.lookup` sdm)
    accumShotDataMap :: MonadError String m
                     => [ShotId]
                     -> m ShotData
    accumShotDataMap sids =
       do counters <- accumCounterMaps $ map sd_counterMap (shotListData sids)
          time <- accumDouble sids $ map sd_time (shotListData sids)
          nCPUs <- accumCPUs sids $ map sd_numCPUs (shotListData sids)
          work <- accumDouble sids $ map sd_work (shotListData sids)
          return ShotData { sd_counterMap = counters
                          , sd_time = time
                          , sd_numCPUs = nCPUs
                          , sd_work = work
                          }

data MainOptions =
     MainOptions { mo_groupChar :: String
                 , mo_maxRelStdDev :: String
                 , mo_printRefCols :: Bool
                 , mo_postfixCPU :: Bool
                 , mo_counterFile :: String
                 , mo_fileArgs :: [String]
                 } deriving (CL.Typeable, CL.Data, Eq)
_EMPTY_MAIN_OPTIONS_ :: MainOptions
_EMPTY_MAIN_OPTIONS_ = MainOptions "" "" False False "" []

instance CL.Attributes MainOptions where
    attributes _ =
        CL.group "Options"
            [ mo_groupChar    %> [ CL.Help "Group shot ids by common prefix"
                                 , CL.ArgHelp "GROUP-CHAR"
                                 , CL.Default ("" :: String)
                                 , CL.Long ["group-char"]
                                 , CL.Short "g"
                                 ]
            , mo_maxRelStdDev %> [ CL.Help $ "Maximal relative standard "
                                          ++ "deviation "
                                          ++ "when accumulating work"
                                 , CL.Default _ERROR_REL_STDDEV_
                                 , CL.ArgHelp "MAX-REL-STDDEV"
                                 , CL.Long [ "maximimal-relative-stddev"
                                        , "max-rel-stddev"
                                        ]
                                 , CL.Short "m"
                                 ]
            , mo_printRefCols %> [ CL.Help "Print reference columns (BROKEN!)"
                                 , CL.Default False
                                 , CL.Long [ "print-ref-columns"
                                           , "print-ref-cols"
                                           ]
                                 , CL.Short "p"
                                 ]
            , mo_postfixCPU   %> [ CL.Help "Postfix CPU instead of sum"
                                 , CL.Default False
                                 , CL.Long ["postfix-cpu"]
                                 , CL.Short "P"
                                 ]
            , mo_counterFile %>  [ CL.Help "Only repect counters listed in file"
                                 , CL.Default ("" :: String)
                                 , CL.Long ["counter-file"]
                                 , CL.Short "c"
                                 ]
            , mo_fileArgs     %> CL.Extra True
            ]

instance CL.RecordCommand MainOptions where
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
main = CL.getArgs >>= CL.executeR _EMPTY_MAIN_OPTIONS_ >>= \opts ->
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
       let eSmap = shotCounterMap (mo_postfixCPU opts)
                                  (shotIdFromString mGroupChar)
                                  (catMaybes mCDs)
       smapRaw <- case eSmap of
                 Right r -> return r
                 Left err -> putStrLn err >> exitFailure
       goodCounters <-
           case opt_counterFile of
             Just f -> liftM Just $ readGoodCounters f
             Nothing -> return Nothing
       let smap = case goodCounters of
             Just gcs -> Map.map (\(a, b, c) -> (delBadCounters gcs a, b, c))
                                 smapRaw
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
       hPutStrLn stderr $ "group char: " ++ show mGroupChar
       --hPutStrLn stderr $ "GOOD sids: " ++ show $ Map.keys sdmap
       hPutStrLn stderr $ "Ignored sids: " ++ show bad_sids
       --hPutStrLn stderr $ "leaps <- regsubsets(unlist(t['WORK'])~" ++
       --                   (intercalate "+" (map rCompatibleCounterName
       --                   (allCounters sdmap))) ++
       --                   ", data=t, force.in=c('CPU_CLK_UNHALTED', " ++
       --                   "'INST_RETIRED'), nbest=10)"
       case groupShotDataMap sdmap maxRelStdDev of
         Left err -> hPutStrLn stderr err
         Right gsdmap ->
             printRTAB (mo_printRefCols opts) $
              case mGroupChar of
                Nothing -> sdmap
                Just _ -> Map.mapKeys (\k -> ShotId k Nothing Nothing) gsdmap
