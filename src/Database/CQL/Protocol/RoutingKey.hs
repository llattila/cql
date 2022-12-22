module Database.CQL.Protocol.RoutingKey where

import Database.CQL.Protocol.Internal
import qualified Data.ByteString as B
import Data.Serialize
import Data.Word
import Database.CQL.Protocol.Murmur3 (murmur3)
import Data.UUID
import Data.Text (Text)
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set

newtype MurmurToken = MurmurToken { unMurmurToken :: Int }
  deriving (Eq, Ord, Show)

getHostForTimeSeriesData :: Map.Map Int (Set.Set MurmurToken) -> (UUID, Text) -> Maybe Int
getHostForTimeSeriesData hostMap = getMatchingHost hostMap . routingKeyForTimeSeriesData

getMatchingHost :: Map.Map Int (Set.Set MurmurToken) -> MurmurToken -> Maybe Int
getMatchingHost hostMap tokenToSeek =
  let distancesToHosts = Map.map ((\x -> getDistance x tokenToSeek) . Set.toList) hostMap
  in fst <$> Map.lookupMin distancesToHosts

getDistance :: [MurmurToken] -> MurmurToken -> Int
getDistance [] _ = 9223372036854775807
getDistance [x] (MurmurToken token) = if token > unMurmurToken x then token - unMurmurToken x else 9223372036854775807
getDistance (x:y:xs) (MurmurToken token) = if token < unMurmurToken y && token > unMurmurToken x
  then token - unMurmurToken x
  else getDistance (y:xs) (MurmurToken token)

routingKeyForTimeSeriesData :: (UUID, Text) -> MurmurToken
routingKeyForTimeSeriesData (uuid, monthlyBucket) = fromJust $ generateTokenFromElements [CqlUuid uuid, CqlText monthlyBucket]

generateTokenFromElements :: [Value] -> Maybe MurmurToken
generateTokenFromElements [] = Nothing
generateTokenFromElements [x] = Just $ MurmurToken $ head $ murmur3 (B.drop 4 $ runPut $ putValue V4 x) 0
generateTokenFromElements xs = Just $ MurmurToken $ head $ murmur3 (B.concat (map generateSingleToken xs)) 0

generateSingleToken :: Value -> B.ByteString
generateSingleToken valueToConvert =
  let valueByteString = B.drop 4 $ runPut $ putValue V4 valueToConvert
  in encode (fromIntegral (B.length valueByteString) :: Word16) <> valueByteString <> encode (0 :: Word8)
