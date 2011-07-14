import Prelude hiding (sum)
import Data.List (nub)
import Data.Map (Map)
import Data.Maybe (catMaybes)
import Data.Foldable (Foldable, toList, sum)
import Data.Sequence (Seq)
import System.Environment (getArgs)
import Text.ProtocolBuffers.Basic (uToString)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map

import Text.ProtocolBuffers.WireMessage (messageGet)
import HsPerfCounters.CounterData(CounterData(..))
import HsPerfCounters.CounterValue(CounterValue(..))

type ShotId = String
type CtrName = String
type CtrVal = Integer

type ShotMap = Map ShotId [(CtrName, CtrVal)]

loadCounterDataProto :: FilePath -> IO (Maybe CounterData)
loadCounterDataProto fn =
    do pbs <- BSL.readFile fn
       case messageGet pbs of
         Right (cd, leftOver) ->
             return (Just cd)
         Left err ->
             do putStrLn $ "ERROR: " ++ err
                return Nothing

extractShotData :: CounterData -> (ShotId, [(CtrName, CtrVal)])
extractShotData cd = (shotId, toList $ fmap extractVal cvs)
    where extractVal :: CounterValue -> (CtrName, CtrVal)
          extractVal cv = ( uToString $ counter_name cv
                          , sum $ fmap toInteger $ counter_value_per_cpu cv
                          )
          cvs :: Seq CounterValue
          cvs = counters cd
          shotId :: String
          shotId = uToString $ shot_id cd

shotMap :: [CounterData] -> ShotMap
shotMap cds = Map.fromList $ map extractShotData cds

allCounters :: ShotMap -> [String]
allCounters sm = nub $ map fst $ concat $ Map.elems sm

main :: IO ()
main =
    do putStrLn "Welcome to BuildSLE"
       mCDs <- getArgs >>= mapM loadCounterDataProto
       let smap = shotMap $ catMaybes mCDs
       putStrLn $ "All shots: " ++ (show $ Map.keys smap)
       putStrLn $ "All counters: " ++ (show $ allCounters smap)
       return ()
