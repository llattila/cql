{-# LANGUAGE ForeignFunctionInterface #-}
{- |
This module is a modification of Data.Dish.Murmur3. The Murmur3 used by
Cassandra is not a correct implementation of Murmur3 and as such, this
needs to be taken in account. The C-library is a direct copy from
https://github.com/russss/murmur3-cassandra  
-} 
module Database.CQL.Protocol.Murmur3(
                         -- * Hashable
                         -- If a value can be represented as a 'CString' then it can be hashed
                         Murmur3Hashable(..),
                         Str(..),
                         -- * Default/Direct Map
                         -- $default
                         murmur3,
                         murmur3',
                         -- * X64 128 bits
                         -- $64_128
                         murmur3IntegerX64,
                         murmur3IntegerX64',
                         -- * FFI
                         murmur3Raw
                         ) where
import Foreign.C
import Foreign.Ptr
import Foreign
import qualified Data.List as L
import qualified Data.Bits as BITS
import qualified System.IO.Unsafe as US
import qualified Data.ByteString as B

--http://www.haskell.org/haskellwiki/List_instance
-- | Because String isn't a \'real\' type :(
newtype Str = Str{ strCon :: String }

-- | Provides an interface for any value which is capable of being represented as a 'CString'
class Murmur3Hashable a where
  toCstring :: a -> IO CStringLen
    
instance Murmur3Hashable Str where
  toCstring val = withCAStringLen (strCon val) $ \x -> return x
    
instance Murmur3Hashable B.ByteString where
  toCstring val = B.useAsCStringLen val $ \x -> return x     
  
{-$default 
Simple, verbose interface for generating hashes
-}
murmur3 :: Murmur3Hashable a => a   -- ^ The hashable to be hashed
           -> Int   -- ^ A seed value for the hash function
           -> [Int] -- ^ returns 4, 32 bit ints, if 'X86_32' is used only the first has a value and the other 3 are 0
murmur3 v s = US.unsafePerformIO $ murmur3' v s

murmur3' :: Murmur3Hashable a => a     -- ^ The hashable to be hashed
           -> Int      -- ^ A seed value for the hash function
           -> IO [Int] -- ^ returns 2 values that are the token values from MurMur3 
murmur3' v s = do m <- murmur3Raw v s; toArr m
  where 
    toArr :: [CLong] -> IO [Int]
    toArr [] = return []
    toArr l = return $ b l []     
              where b :: [CLong] -> [Int] -> [Int]
                    b xs l2 = foldl (\ list x -> list ++ [w x] ) l2 xs
                    w :: CLong -> Int
                    w = fromIntegral

{-$64_128
Generate 128 bit hash values, optimized for 64 bit systems
-}
murmur3IntegerX64' :: Murmur3Hashable a => a       -- ^ The hashable to be hashed
                     -> Int        -- ^ A seed value for the hash function
                     -> IO Integer -- ^ 128 bit number generated from the hashable
murmur3IntegerX64' val seed = x128 val seed

{- | Generate a 128 bit hash from the given value, this function's implementation 
     is optimized for x64 architectures but works on any.
     Its throughput is 250% higher than 'murmur3IntegerX86', but it has roughly 
     the same latency. 
-}
murmur3IntegerX64 :: Murmur3Hashable a => a     -- ^ The hashable to be hashed
                     -> Int     -- ^ A seed value for the hash function
                     -> Integer -- ^ 128 bit number generated from the hashable
murmur3IntegerX64 val seed = US.unsafePerformIO $ murmur3IntegerX64' val seed

foreign import ccall "MurmurHash3_x64_128_cassandra" c_x64_128
  ::  CString -> CInt -> CUInt ->  Ptr CLong -> IO ()

-- | all murmur functions use this and manipulate its response to return a different format  
murmur3Raw :: Murmur3Hashable a => a -> Int ->  IO [CLong]
murmur3Raw val seed = do
  val' <- toCstring val
  let cstr = strFromCStr val'
  let strLength = strLFromCStr val'
  outPtr <- mallocArray arrSize
  doHash cstr strLength (fromIntegral seed) outPtr
  peekArray arrSize outPtr
  where arrSize = 2 
        strFromCStr :: CStringLen -> CString
        strFromCStr = fst
        strLFromCStr :: CStringLen -> CInt
        strLFromCStr i = fromIntegral $ snd i
        doHash :: CString -> CInt -> CUInt -> Ptr CLong -> IO()
        doHash v s se o = c_x64_128 v s se o

x128 :: Murmur3Hashable a => a -> Int -> IO Integer
x128 val seed = do 
  v <- hash 
  return $ twiddle 0 v 
  where hash :: IO [CLong]
        hash = murmur3Raw val seed
        twiddle :: Integer -> [CLong] -> Integer
        twiddle i [] = i
        twiddle i (0:xs) = twiddle i xs -- don't shift when val is 0
        twiddle i (x:xs) = twiddle (BITS.shift i (BITS.bitSize x) `BITS.xor` fromIntegral x) xs
