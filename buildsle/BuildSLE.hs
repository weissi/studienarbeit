{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
import Prelude
import Control.Monad.Error (MonadError(), throwError)
import Data.List (nub, isSuffixOf, genericLength, foldl', groupBy, sort, (\\))
import Data.Map (Map)
import Data.Maybe (catMaybes, fromMaybe)
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

average :: forall a b. (Real b, Fractional a) => [b] -> a
average xs = realToFrac (sum xs) / genericLength xs

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

allCounters :: ShotDataMap -> [String]
allCounters sm = nub $ concat $ map (Map.keys . fst) $ Map.elems sm

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
                    mapM_ (printCounterRatio sid) allCtrs
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

groupShotDataMap :: MonadError String m => ShotDataMap -> m ShotGroupDataMap
groupShotDataMap sdm =
    do groupNames <- sequence $ map shotGroupname groupedShotIds
       let shotGroupData = map accumShotDataMapNEW groupedShotIds
       return $ Map.fromList $ zip groupNames shotGroupData
    where
    groupedShotIds :: [[ShotId]]
    groupedShotIds = shotGroupIds $ Map.keys sdm
    accumCounterMaps :: [CounterMap] -> CounterMap
    accumCounterMaps cms = foldl' Map.union Map.empty cms
    accumWork :: [Double] -> Double
    accumWork = average
    shotGroupname :: MonadError String m => [ShotId] -> m ShotGroupId
    shotGroupname sids =
        case sids of
          [] -> throwError "empty shot group --> cannot generate group id"
          firstShot:_ -> return $ si_shotGroupId firstShot
    shotListData :: [ShotId] -> ([CounterMap], [Double])
    shotListData sids = unzip $ catMaybes $ map (flip Map.lookup sdm) sids
    accumShotDataMapNEW :: [ShotId] -> (CounterMap, Double)
    accumShotDataMapNEW sids = ( accumCounterMaps $ fst (shotListData sids)
                               , accumWork $ snd (shotListData sids)
                               )

main :: IO ()
main =
    do rawArgs <- getArgs
       let (args, mGroupChar) =
               case rawArgs of
                 "-g" : (gc : []) : as -> (as, Just gc)
                 as -> (as, Nothing)
       mCDs <- mapM loadCounterDataProto $ filter isCounterFile args
       let smap = shotCounterMap (shotIdFromString mGroupChar) (catMaybes mCDs)
           all_arg_files_basenames = map takeFileName args
           all_sids = Map.keys smap
           work_file_available :: ShotId -> Bool
           work_file_available sid = calcWorkFile sid `elem`
                                     all_arg_files_basenames
           good_sids = filter work_file_available all_sids
           bad_sids = all_sids \\ good_sids
           smap' = foldr Map.delete smap bad_sids
       sdmap <- shotDataMap smap' args
       hPutStrLn stderr $ "group char: " ++ (show mGroupChar)
       hPutStrLn stderr $ "GOOD sids: " ++ (show $ Map.keys sdmap)
       hPutStrLn stderr $ "Ignored sids: " ++ (show bad_sids)
       case groupShotDataMap sdmap of
         Left err -> putStrLn err
         Right gsdmap ->
             do printRTAB $
                 case mGroupChar of
                   Nothing -> sdmap
                   Just _ -> Map.mapKeys (\k -> ShotId k Nothing Nothing) gsdmap
       return ()
