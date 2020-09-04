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
import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Functor (($>))
import Foreign.Storable (Storable (..))
import qualified Control.Monad as M
import qualified Data.Foldable as F
import qualified Data.Vector.Storable.Mutable as VM

data FixedArrayElem a = FixedArrayElem
  { arrayElem :: !a
  , nextFree  :: {-# UNPACK #-} !Int
  }

instance Storable a => Storable (FixedArrayElem a) where
  sizeOf _ = sizeOf (undefined :: a) + sizeOf (undefined :: Int)
  alignment _ = max (sizeOf (undefined :: a)) (sizeOf (undefined :: Int))
  peek ptr = FixedArrayElem
    <$> peekByteOff ptr 0
    <*> peekByteOff ptr (sizeOf (undefined :: a))
  poke ptr e = do
    pokeByteOff ptr 0 $ arrayElem e
    pokeByteOff ptr (sizeOf (undefined :: a)) $ nextFree e

mkElem :: a -> FixedArrayElem a
mkElem a = FixedArrayElem a 0

data Array m a = Array
  { arrayStore :: {-# UNPACK #-} !(VM.MVector (PrimState m) (FixedArrayElem a))
  , arrayTop   :: {-# UNPACK #-} !Int
  , arrayEnd   :: {-# UNPACK #-} !Int
  }

new :: (PrimMonad m, VM.Storable a) => Int -> m (Array m a)
new size = Array <$> VM.new (size + 1) <*> pure 0 <*> pure 1

add :: (PrimMonad m, VM.Storable a) => Array m a -> a -> m (Array m a)
add arr v
  | arrayTop arr == 0 =
    if arrayEnd arr == VM.length (arrayStore arr)
      then pure arr
      else VM.write (arrayStore arr) (arrayEnd arr) e
        $> arr { arrayEnd = arrayEnd arr + 1 }
  | otherwise = do
    freeValue <- VM.read (arrayStore arr) (arrayTop arr)
    VM.write (arrayStore arr) (arrayTop arr) e
    pure arr { arrayTop = nextFree freeValue }
 where e = FixedArrayElem v 0

delete :: (PrimMonad m, VM.Storable a) => Array m a -> Int -> m (Array m a)
delete arr i
  =  VM.modify (arrayStore arr) (\e -> e { nextFree = arrayTop arr }) i
  $> arr { arrayTop = i }

read :: (PrimMonad m, VM.Storable a) => Array m a -> Int -> m (FixedArrayElem a)
read arr = VM.read (arrayStore arr)

write :: (PrimMonad m, VM.Storable a) => Array m a -> Int -> a -> m ()
write arr i v = modify arr (const v) i

modify :: (PrimMonad m, VM.Storable a) => Array m a -> (a -> a) -> Int -> m ()
modify arr f =
  VM.modify (arrayStore arr) (\e -> e { arrayElem = f (arrayElem e) })

indexes :: Array m a -> [Int]
indexes arr = [1..arrayEnd arr - 1]

forM_ :: (PrimMonad m, VM.Storable a) => Array m a -> (Int -> a -> m ()) -> m ()
forM_ arr f = M.forM_ (indexes arr) $ \i -> do
  e <- read arr i
  M.unless (nextFree e > 0) $ f i (arrayElem e)

foldlM :: (PrimMonad m, VM.Storable a)
       => (b -> Int -> a -> m b) -> b -> Array m a -> m b
foldlM f init arr = F.foldlM f' init (indexes arr)
 where
  f' v i = do
    e <- read arr i
    if nextFree e > 0
      then pure v
      else f v i (arrayElem e)
