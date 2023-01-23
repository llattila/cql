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
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Int

newtype RoutingToken  = RoutingToken { unRoutingToken :: Int64 }
  deriving (Eq, Ord, Show)

getMatchingHost :: Map.Map Int (Set.Set RoutingToken) -> RoutingToken -> Maybe Int
getMatchingHost hostMap tokenToSeek =
  let distancesToHosts = Map.map ((\x -> getDistance x tokenToSeek) . Set.toList) hostMap
  in fst <$> Map.lookupMin distancesToHosts

getDistance :: [RoutingToken] -> RoutingToken -> Int64
getDistance [] _ = 9223372036854775807
getDistance [x] (RoutingToken token) = if token > unRoutingToken x then token - unRoutingToken x else 9223372036854775807
getDistance (x:y:xs) (RoutingToken token) = if token < unRoutingToken y && token > unRoutingToken x
  then token - unRoutingToken x
  else getDistance (y:xs) (RoutingToken token)

generateTokenFromElements :: [Value] -> Maybe RoutingToken 
generateTokenFromElements [] = Nothing
generateTokenFromElements [x] = Just $ RoutingToken $ fromIntegral $ head $ murmur3 (B.drop 4 $ runPut $ putValue V4 x) 0
generateTokenFromElements xs = Just $ RoutingToken $ fromIntegral $ head $ murmur3 (B.concat (map generateSingleToken xs)) 0

generateSingleToken :: Value -> B.ByteString
generateSingleToken valueToConvert =
  let valueByteString = B.drop 4 $ runPut $ putValue V4 valueToConvert
  in encode (fromIntegral (B.length valueByteString) :: Word16) <> valueByteString <> encode (0 :: Word8)
