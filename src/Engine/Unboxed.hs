{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UnboxedTuples #-}
module Engine.Unboxed (pattern Coords, CoordsMaybe, noCoord, mkCoords) where

data CoordsMaybe = CoordsMaybe (# (# Double, Double #) | (# #) #)

pattern Coords :: Double -> Double -> CoordsMaybe
pattern Coords x y = CoordsMaybe (# (# x, y #) | #)

noCoord :: CoordsMaybe
noCoord = CoordsMaybe (# | (# #) #)
{-# INLINE noCoord #-}

mkCoords :: Double -> Double -> CoordsMaybe
mkCoords a b = CoordsMaybe (# (# a, b #) | #)
{-# INLINE mkCoords #-}
