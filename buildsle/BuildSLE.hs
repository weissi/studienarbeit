import Prelude hiding (sum)
import Data.List (nub)
import Data.Map (Map)
import Data.Maybe (catMaybes)
import Data.Sequence (Seq)
import System.Environment (getArgs)
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
type ShotMap = Map ShotId CounterMap

loadCounterDataProto :: FilePath -> IO (Maybe CounterData)
loadCounterDataProto fn =
    do pbs <- BSL.readFile fn
       case messageGet pbs of
         Right (cd, leftOver) ->
             return (Just cd)
         Left err ->
             do putStrLn $ "ERROR: " ++ err
                return Nothing

addShotData :: CounterData -> ShotMap -> ShotMap
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

shotMap :: [CounterData] -> ShotMap
shotMap cds = foldr addShotData Map.empty cds

allCounters :: ShotMap -> [String]
allCounters sm = nub $ concat $ map Map.keys $ Map.elems sm

printRTAB :: ShotMap -> IO ()
printRTAB sm =
    do putStr "SHOT-ID\t"
       mapM_ (\c -> putStr $ c ++ "\t") counters
       putStr "\n"
       mapM_ (printCounters sm) (Map.keys sm)
       where counters = allCounters sm
             strValueMap :: ShotMap -> ShotId -> Map CtrName String
             strValueMap sm sid = Map.map show $
                                          Map.findWithDefault Map.empty sid sm
             printCounter :: ShotMap -> ShotId -> String -> IO ()
             printCounter sm sid c =
                 putStr ((Map.findWithDefault "NA\t" c (strValueMap sm sid))
                                    ++ "\t")
             printCounters :: ShotMap -> ShotId -> IO ()
             printCounters sm sid =
                 do putStr $ sid ++ "\t"
                    mapM_ (printCounter sm sid) (allCounters sm)
                    putStr "\n"

main :: IO ()
main =
    do putStrLn "Welcome to BuildSLE"
       mCDs <- getArgs >>= mapM loadCounterDataProto
       let smap = shotMap $ catMaybes mCDs
       printRTAB smap
       return ()
