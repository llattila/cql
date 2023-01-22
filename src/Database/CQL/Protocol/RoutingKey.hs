{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.CQL.Protocol.RoutingKey where

import Database.CQL.Protocol.Types
import Database.CQL.Protocol.Codec
import qualified Data.ByteString as B
import Data.Serialize
import Data.Word
import Database.CQL.Protocol.Murmur3
import Data.UUID
import Data.Text (Text)
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Int

newtype RoutingKey  = RoutingKey { unRoutingKey :: Int64 }
  deriving (Eq, Ord, Show)

-- getHostForTimeSeriesData :: Tuple a => Map.Map Int (Set.Set MurmurToken) -> [Int32] -> a -> Maybe Int
-- getHostForTimeSeriesData hostTokens primaryIndexesForQuery tupleToProcess = getMatchingHost hostTokens =<< routingKeyForTuple primaryIndexesForQuery tupleToProcess
-- 
-- getMatchingHost :: Map.Map Int (Set.Set MurmurToken) -> MurmurToken -> Maybe Int
-- getMatchingHost hostMap tokenToSeek =
--   let distancesToHosts = Map.map ((\x -> getDistance x tokenToSeek) . Set.toList) hostMap
--   in fst <$> Map.lookupMin distancesToHosts
-- 
-- getDistance :: [MurmurToken] -> MurmurToken -> Int64
-- getDistance [] _ = 9223372036854775807
-- getDistance [x] (MurmurToken token) = if token > unMurmurToken x then token - unMurmurToken x else 9223372036854775807
-- getDistance (x:y:xs) (MurmurToken token) = if token < unMurmurToken y && token > unMurmurToken x
--   then token - unMurmurToken x
--   else getDistance (y:xs) (MurmurToken token)

-- routingKeyForTimeSeriesData :: (UUID, Text) -> MurmurToken
-- routingKeyForTimeSeriesData (uuid, monthlyBucket) = fromJust $ generateTokenFromElements [CqlUuid uuid, CqlText monthlyBucket]
-- 
-- routingKeyForTuple :: forall a. Tuple a => [Int32] -> a -> Maybe MurmurToken
-- routingKeyForTuple primaryIndexesForQuery tupleToProcess =
--    let c = untag (count :: Tagged a Int)
--    in case (c, primaryIndexesForQuery) of
--      (0,_ ) -> Nothing
--      (1, [0]) -> generateTokenFromElements [runIdentity tupleToProcess] 


generateTokenFromElements :: [Value] -> Maybe RoutingKey 
generateTokenFromElements [] = Nothing
generateTokenFromElements [x] = Just $ RoutingKey $ fromIntegral $ head $ murmur3 (B.drop 4 $ runPut $ putValue V4 x) 0
generateTokenFromElements xs = Just $ RoutingKey $ fromIntegral $ head $ murmur3 (B.concat (map generateSingleToken xs)) 0

generateSingleToken :: Value -> B.ByteString
generateSingleToken valueToConvert =
  let valueByteString = B.drop 4 $ runPut $ putValue V4 valueToConvert
  in encode (fromIntegral (B.length valueByteString) :: Word16) <> valueByteString <> encode (0 :: Word8)
