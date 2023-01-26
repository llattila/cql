{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

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
import GHC.Generics

newtype RoutingToken  = RoutingToken { unRoutingToken :: Int64 }
  deriving (Eq, Ord, Show, Generic)

generateTokenFromElements :: [Value] -> Maybe RoutingToken 
generateTokenFromElements [] = Nothing
generateTokenFromElements [x] = Just $ RoutingToken $ fromIntegral $ head $ murmur3 (B.drop 4 $ runPut $ putValue V4 x) 0
generateTokenFromElements xs = Just $ RoutingToken $ fromIntegral $ head $ murmur3 (B.concat (map generateSingleToken xs)) 0

generateSingleToken :: Value -> B.ByteString
generateSingleToken valueToConvert =
  let valueByteString = B.drop 4 $ runPut $ putValue V4 valueToConvert
  in encode (fromIntegral (B.length valueByteString) :: Word16) <> valueByteString <> encode (0 :: Word8)
