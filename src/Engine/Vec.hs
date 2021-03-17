{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Engine.Vec
  ( UpdateM ()
  , ReadVec (freeze, read)
  , Vec ()
  , unsafeRunUpdateM
  , new
  , length
  , fromList
  , write
  , modify
  ) where

import Prelude hiding (length, read)
import Control.Monad.Primitive (PrimState)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM

newtype UpdateM a = UpdateM (IO a) deriving (Functor, Applicative, Monad)

class Monad m => ReadVec m where
  read :: VM.Storable a => Vec a -> Int -> m a
  freeze :: VM.Storable a => Vec a -> m (V.Vector a)

-- | WARNING: DO NOT USE THIS FUNCTION OUTSIDE OF THE MAIN GAME LOOP!
--
-- UpdateM is not designed to be ran inside IO.
unsafeRunUpdateM :: UpdateM a -> IO a
unsafeRunUpdateM (UpdateM io) = io

newtype Vec a = Vec { unVec :: VM.MVector (PrimState IO) a }

new :: VM.Storable a => Int -> IO (Vec a)
new = fmap Vec . VM.new

length :: VM.Storable a => Vec a -> Int
length = VM.length . unVec

fromList :: VM.Storable a => [a] -> IO (Vec a)
fromList = fmap Vec . V.thaw . V.fromList

instance ReadVec IO where
  read = VM.read . unVec
  {-# INLINE read #-}

  freeze = V.freeze . unVec
  {-# INLINE freeze #-}

instance ReadVec UpdateM where
  read v = UpdateM . VM.read (unVec v)
  {-# INLINE read #-}

  freeze = UpdateM . V.freeze . unVec
  {-# INLINE freeze #-}

write :: VM.Storable a => Vec a -> Int -> a -> UpdateM ()
write v i a = UpdateM $ VM.write (unVec v) i a

modify :: VM.Storable a => Vec a -> (a -> a) -> Int -> UpdateM ()
modify v f i = UpdateM $ VM.modify (unVec v) f i
