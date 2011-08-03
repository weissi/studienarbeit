import Data.List (nub, isSuffixOf, intercalate, (\\))
import Data.Map (Map)
import Data.Maybe (catMaybes)
import Safe (readMay)
import System.Environment (getArgs)
import System.FilePath.Posix (takeFileName)
import System.IO (hPutStrLn, stderr)
import Text.ProtocolBuffers.Basic (uToString)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Foldable as F
import qualified Data.Map as Map

import Text.ProtocolBuffers.WireMessage (messageGet)
import HsPerfCounters.CounterData(CounterData(..))
import HsPerfCounters.CounterValue(CounterValue(..))

type ShotId = String
type CtrName = String
type CtrVal = Integer

type CounterMap = Map CtrName CtrVal
type ShotCounterMap = Map ShotId CounterMap
type ShotWorkMap = Map ShotId Double
type ShotDataMap = Map ShotId (CounterMap, Double)

_NaN_ :: Fractional a => a
_NaN_ = 0/0

_REF_COL_ :: String
_REF_COL_ = "CPU_CLK_UNHALTED"


tr :: Eq a => a -> a -> [a] -> [a]
tr a b = map f
    where f c = if c == a then b else c

loadCounterDataProto :: FilePath -> IO (Maybe CounterData)
loadCounterDataProto fn =
    do pbs <- BSL.readFile fn
       case messageGet pbs of
         Right (cd, _) ->
             return (Just cd)
         Left err ->
             do hPutStrLn stderr $ "ERROR on file `" ++ fn ++ "': " ++ err
                return Nothing

addShotData :: CounterData -> ShotCounterMap -> ShotCounterMap
addShotData cd shotMap = Map.insert shotId ctrMap shotMap
    where extractVal :: CounterValue -> CounterMap -> CounterMap
          extractVal cv cmap =
             Map.insert (uToString $ counter_name cv)
                        (F.sum $ fmap toInteger $ counter_value_per_cpu cv)
                        cmap
          ctrMap :: CounterMap
          ctrMap = F.foldr extractVal Map.empty (counters cd)
          shotId :: String
          shotId = uToString $ shot_id cd

shotCounterMap :: [CounterData] -> ShotCounterMap
shotCounterMap cds = foldr addShotData Map.empty cds

allCounters :: ShotDataMap -> [CtrName]
allCounters sm = nub $ concat $ map (Map.keys . fst) $ Map.elems sm

rCompatibleCounterName :: CtrName -> String
rCompatibleCounterName = tr ':' '.'

printRTAB :: ShotDataMap -> IO ()
printRTAB sm =
    do putStr "SHOT_ID\t"
       putStr "WORK\t"
       mapM_ (\c -> putStr $ c ++ "\t") allCtrs
       mapM_ (\c -> putStr $ "RATIO_" ++ c ++ "\t") allCtrs
       putStr "\n"
       mapM_ printShot (Map.keys sm)
       where allCtrs = allCounters sm
             valueMap :: ShotId -> Map CtrName CtrVal
             valueMap sid =
                 fst $ Map.findWithDefault (Map.empty, undefined) sid sm

             strValueMap :: ShotId -> Map CtrName String
             strValueMap sid =
                 Map.map show $ valueMap sid

             referenceValue :: Fractional a => ShotId -> a
             referenceValue sid =
                 Map.findWithDefault _NaN_ _REF_COL_ (Map.map fromIntegral (valueMap sid))

             strRatioMap :: ShotId -> Map CtrName String
             strRatioMap sid =
                 Map.map (\v -> show (fromIntegral v / referenceValue sid)) (valueMap sid)

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
                 do putStr $ sid ++ "\t"
                    putStr (Map.findWithDefault "NA" sid (Map.map (show.snd) sm))
                    putStr "\t"
                    mapM_ (printCounter sid) allCtrs
                    mapM_ (printCounterRatio sid) allCtrs
                    putStr "\n"

calcWorkFile :: ShotId -> FilePath
calcWorkFile sid = "work_" ++ sid ++ ".work"

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

main :: IO ()
main =
    do args <- getArgs
       mCDs <- mapM loadCounterDataProto $ filter isCounterFile args
       let smap = shotCounterMap $ catMaybes mCDs
           all_arg_files_basenames = map takeFileName args
           all_sids = Map.keys smap
           work_file_available :: ShotId -> Bool
           work_file_available sid = calcWorkFile sid `elem` all_arg_files_basenames
           good_sids = filter work_file_available all_sids
           bad_sids = all_sids \\ good_sids
           smap' = foldr Map.delete smap bad_sids
       sdmap <- shotDataMap smap' args
       hPutStrLn stderr $ "GOOD sids: " ++ (show $ Map.keys sdmap)
       hPutStrLn stderr $ "Ignored sids: " ++ (show bad_sids)
       hPutStrLn stderr $ "leaps <- regsubsets(unlist(t['WORK'])~" ++
           (intercalate "+" (map rCompatibleCounterName (allCounters sdmap))) ++
           ", data=t, force.in=c('CPU_CLK_UNHALTED', 'INST_RETIRED'), nbest=10)"
       printRTAB sdmap
       return ()
