{-# LANGUAGE ScopedTypeVariables #-}
module Engine.FixedArray
  ( FixedArrayElem
  , Array
  , mkElem
  , new
  , add
  , delete
  , read
  , write
  , modify
  , forM_
  , foldlM
  ) where

import Prelude hiding (init, length, read)
import Control.Monad (unless)
import Data.Foldable (for_)
import Data.Functor (($>))
import Foreign.Storable (Storable (..))
import Foreign.Storable.TH.Internal (roundUp)
import qualified Data.Foldable as F
import qualified Data.Vector.Storable.Mutable as VM
import qualified Engine.Vec as V

data FixedArrayElem a = FixedArrayElem
  { arrayElem :: !a
  , nextFree  :: {-# UNPACK #-} !Int
  } deriving Show

instance Storable a => Storable (FixedArrayElem a) where
  sizeOf _ = sizeOf (undefined :: a) + sizeOf (undefined :: Int)
  alignment _ = max (alignment (undefined :: a)) (alignment (undefined :: Int))
  peek ptr = FixedArrayElem
    <$> peekByteOff ptr 0
    <*> peekByteOff ptr (roundUp (sizeOf (undefined :: a)) (alignment (undefined :: FixedArrayElem a)))
  poke ptr e = do
    pokeByteOff ptr 0 $ arrayElem e
    pokeByteOff ptr (roundUp (sizeOf (undefined :: a)) (alignment (undefined :: FixedArrayElem a))) $ nextFree e

mkElem :: a -> FixedArrayElem a
mkElem a = FixedArrayElem a 0

data Array a = Array
  { arrayStore :: {-# UNPACK #-} !(V.Vec (FixedArrayElem a))
  , arrayTop   :: {-# UNPACK #-} !Int
  , arrayEnd   :: {-# UNPACK #-} !Int
  }

instance Show (Array a) where
  show arr = "Array top: " ++ show (arrayTop arr)
          ++ " end: " ++ show (arrayEnd arr)

new :: VM.Storable a => Int -> IO (Array a)
new size = Array <$> V.new (size + 1) <*> pure (-1) <*> pure 1

add :: VM.Storable a => Array a -> a -> V.UpdateM (Array a)
add arr v
  | arrayTop arr == -1 =
    if arrayEnd arr == V.length (arrayStore arr)
      then pure arr
      else V.write (arrayStore arr) (arrayEnd arr) e
        $> arr { arrayEnd = arrayEnd arr + 1 }
  | otherwise = do
    freeValue <- V.read (arrayStore arr) (arrayTop arr)
    V.write (arrayStore arr) (arrayTop arr) e
    pure arr { arrayTop = nextFree freeValue }
 where e = FixedArrayElem v 0

delete :: VM.Storable a => Array a -> Int -> V.UpdateM (Array a)
delete arr i
  =  V.modify (arrayStore arr) (\e -> e { nextFree = arrayTop arr }) i
  $> arr { arrayTop = i }

read :: (V.ReadVec m, VM.Storable a) => Array a -> Int -> m (FixedArrayElem a)
read arr = V.read (arrayStore arr)
{-# INLINABLE read #-}

write :: VM.Storable a => Array a -> Int -> a -> V.UpdateM ()
write arr i v = modify arr (const v) i

modify :: VM.Storable a => Array a -> (a -> a) -> Int -> V.UpdateM ()
modify arr f =
  V.modify (arrayStore arr) (\e -> e { arrayElem = f (arrayElem e) })

indexes :: Array a -> [Int]
indexes arr = [1..arrayEnd arr - 1]

forM_ :: (V.ReadVec m, VM.Storable a) => Array a -> (Int -> a -> m ()) -> m ()
forM_ arr f = for_ (indexes arr) $ \i -> do
  e <- read arr i
  unless (nextFree e /= 0) $ f i (arrayElem e)

foldlM :: (V.ReadVec m, VM.Storable a)
       => (b -> Int -> a -> m b) -> b -> Array a -> m b
foldlM f init arr = F.foldlM f' init (indexes arr)
 where
  f' v i = do
    e <- read arr i
    if nextFree e /= 0
      then pure v
      else f v i (arrayElem e)
