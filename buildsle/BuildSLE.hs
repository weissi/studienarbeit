{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Prelude
import Control.Monad ()
import Control.Monad.Error (MonadError(), throwError)
import Data.List ( nub, isSuffixOf, foldl', groupBy, sort, (\\)
                 , intercalate)
import Data.Map (Map)
import Data.Maybe (catMaybes, fromMaybe)
import Math.Statistics (mean, stddev)
import Safe (readMay)
import System.Console.CmdLib
import System.FilePath.Posix (takeFileName)
import System.IO (hPutStrLn, stderr)
import Text.ProtocolBuffers.Basic (uToString)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Foldable as F
import qualified Data.Map as Map

import Text.ProtocolBuffers.WireMessage (messageGet)
import HsPerfCounters.CounterData(CounterData(..))
import HsPerfCounters.CounterValue(CounterValue(..))

type ShotGroupId = String
data ShotId = ShotId { si_shotGroupId :: ShotGroupId
                     , si_subShotId :: Maybe String
                     , si_groupChar :: Maybe Char
                     } deriving (Eq, Ord)
type CtrName = String
type CtrVal = Integer

instance Show ShotId where
    show (ShotId shotGroupId mSubShotId mGroupChar) =
        shotGroupId ++ (maybe "" (:[]) mGroupChar)
        ++ (fromMaybe "" mSubShotId)

type CounterMap = Map CtrName CtrVal
type ShotCounterMap = Map ShotId CounterMap
type ShotWorkMap = Map ShotId Double
type ShotDataMap = Map ShotId (CounterMap, Double)
type ShotGroupDataMap = Map ShotGroupId (CounterMap, Double)

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

loadCounterDataProto :: FilePath -> IO (Maybe CounterData)
loadCounterDataProto fn =
    do pbs <- BSL.readFile fn
       case messageGet pbs of
         Right (cd, _) ->
             return (Just cd)
         Left err ->
             do hPutStrLn stderr $ "ERROR on file `" ++ fn ++ "': " ++ err
                return Nothing

addShotData :: (String -> ShotId)
            -> CounterData
            -> ShotCounterMap
            -> ShotCounterMap
addShotData shotIdFun cd shotMap = Map.insert shotId ctrMap' shotMap
    where extractVal :: CounterValue -> CounterMap -> CounterMap
          extractVal cv cmap =
             Map.insert (uToString $ counter_name cv)
                        (F.sum $ fmap toInteger $ counter_value_per_cpu cv)
                        cmap
          ctrMap' :: CounterMap
          ctrMap' = F.foldr extractVal ctrMap (counters cd)
          shotId :: ShotId
          shotId = shotIdFun $ uToString $ shot_id cd
          -- should always return Map.empty for NON-grouped shot ids
          ctrMap :: CounterMap
          ctrMap = Map.findWithDefault Map.empty shotId shotMap

shotCounterMap :: (String -> ShotId) -> [CounterData] -> ShotCounterMap
shotCounterMap shotIdFun cds = foldr (addShotData shotIdFun) Map.empty cds

allCounters :: ShotDataMap -> [CtrName]
allCounters sm = nub $ concat $ map (Map.keys . fst) $ Map.elems sm

printRTAB :: Bool -> ShotDataMap -> IO ()
printRTAB ratios sm =
    do putStr "SHOT_ID\t"
       putStr "WORK\t"
       mapM_ (\c -> putStr $ c ++ "\t") allCtrs
       if ratios
          then mapM_ (\c -> putStr $ "RATIO_" ++ c ++ "\t") allCtrs
          else return ()
       putStr "\n"
       mapM_ printShot (Map.keys sm)
       where allCtrs = allCounters sm
             valueMap :: ShotId -> Map CtrName CtrVal
             valueMap sid =
                 fst $ Map.findWithDefault (Map.empty, undefined) sid sm

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
                    putStr (Map.findWithDefault "NA" sid (Map.map (show.snd) sm))
                    putStr "\t"
                    mapM_ (printCounter sid) allCtrs
                    if ratios
                       then mapM_ (printCounterRatio sid) allCtrs
                       else return ()
                    putStr "\n"

calcWorkFile :: ShotId -> FilePath
calcWorkFile sid = "work_" ++ (show sid) ++ ".work"

isCounterFile :: FilePath -> Bool
isCounterFile f = ".ctrs" `isSuffixOf` takeFileName f

findWorkFile :: ShotId -> [FilePath] -> Maybe FilePath
findWorkFile sid files =
    case (filter (calcWorkFile sid `isSuffixOf`) files) of
      [] -> Nothing
      f:[] -> Just f
      _ -> Nothing

getWork :: [FilePath] -> ShotId -> IO (Maybe Double)
getWork files sid =
    case findWorkFile sid files of
      Just file ->
          do val <- readFile file
             return $ readMay val
      Nothing -> return Nothing

shotDataMap :: ShotCounterMap -> [FilePath] -> IO ShotDataMap
shotDataMap scmap files =
    do mWorks <- mapM (getWork files) sids
       return $ Map.intersectionWith (,) scmap (swmap mWorks)
       where sids = Map.keys scmap
             check :: (ShotId, Maybe Double) -> Maybe (ShotId, Double)
             check (f, mS) =
                 case mS of
                   Nothing -> Nothing
                   Just s -> Just (f, s)
             swmap :: [Maybe Double] -> ShotWorkMap
             swmap w = Map.fromList $ catMaybes (map check (zip sids w))

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
    accumWork :: MonadError String m => [ShotId] -> [Double] -> m Double
    accumWork sids ws =
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
    shotListData :: [ShotId] -> ([CounterMap], [Double])
    shotListData sids = unzip $ catMaybes $ map (flip Map.lookup sdm) sids
    accumShotDataMap :: MonadError String m => [ShotId] -> m (CounterMap, Double)
    accumShotDataMap sids =
       do let counters = accumCounterMaps $ fst (shotListData sids)
          work <- accumWork sids $ snd (shotListData sids)
          return (counters, work)

data MainOptions =
     MainOptions { mo_groupChar :: String
                 , mo_maxRelStdDev :: String
                 , mo_printRefCols :: Bool
                 , mo_fileArgs :: [String]
                 } deriving (Typeable, Data, Eq)
_EMPTY_MAIN_OPTIONS_ :: MainOptions
_EMPTY_MAIN_OPTIONS_ = MainOptions "" "" False []

instance Attributes MainOptions where
    attributes _ =
        group "Options"
            [ mo_groupChar    %> [ Help "Group shot ids by common prefix"
                                 , ArgHelp "GROUP-CHAR"
                                 , Default ""
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
            , mo_fileArgs     %> Extra True
            ]

instance RecordCommand MainOptions where
    mode_summary _ = "BuildSLE"

main :: IO ()
main = getArgs >>= executeR _EMPTY_MAIN_OPTIONS_ >>= \opts ->
    do let opt_files = mo_fileArgs opts
       mGroupChar <-
           case mo_groupChar opts of
             g : [] -> return $ Just g
             [] -> return Nothing
             _ ->
                 do hPutStrLn stderr "WARNING: group char ignored (length)"
                    return Nothing
       maxRelStdDev <-
           case readMay $ mo_maxRelStdDev opts of
             Just v -> return v
             Nothing ->
                 do hPutStrLn stderr "WARNING: max rel stddev param ignored"
                    return _ERROR_REL_STDDEV_
       mCDs <- mapM loadCounterDataProto $ filter isCounterFile opt_files
       let smap = shotCounterMap (shotIdFromString mGroupChar) (catMaybes mCDs)
           all_arg_files_basenames = map takeFileName opt_files
           all_sids = Map.keys smap
           work_file_available :: ShotId -> Bool
           work_file_available sid = calcWorkFile sid `elem`
                                     all_arg_files_basenames
           good_sids = filter work_file_available all_sids
           bad_sids = all_sids \\ good_sids
           smap' = foldr Map.delete smap bad_sids
       sdmap <- shotDataMap smap' opt_files
       hPutStrLn stderr $ "group char: " ++ (show mGroupChar)
       hPutStrLn stderr $ "GOOD sids: " ++ (show $ Map.keys sdmap)
       hPutStrLn stderr $ "Ignored sids: " ++ (show bad_sids)
       hPutStrLn stderr $ "leaps <- regsubsets(unlist(t['WORK'])~" ++
                          (intercalate "+" (map rCompatibleCounterName
                          (allCounters sdmap))) ++
                          ", data=t, force.in=c('CPU_CLK_UNHALTED', " ++
                          "'INST_RETIRED'), nbest=10)"
       case groupShotDataMap sdmap maxRelStdDev of
         Left err -> putStrLn err
         Right gsdmap ->
             do printRTAB (mo_printRefCols opts) $
                 case mGroupChar of
                   Nothing -> sdmap
                   Just _ -> Map.mapKeys (\k -> ShotId k Nothing Nothing) gsdmap
       return ()
