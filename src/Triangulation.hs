{-# LANGUAGE BangPatterns #-}

module Triangulation where

import qualified Data.HashMap as HM
import Data.List
import Point

type Space a = HM.Map Integer (Point a)

data Plane a
  = PlaneX a
  | PlaneY a
  | PlaneZ a
  deriving (Eq, Show)

-- 2-D Face
data Face a = Face
  { face_vertex1 :: !(Point a)
  , face_vertex2 :: !(Point a)
  , face_vertex3 :: !(Point a)
  } deriving (Show, Eq)

data Simplex a = Simplex
  { simplex_vertex1 :: !(Point a)
  , simplex_vertex2 :: !(Point a)
  , simplex_vertex3 :: !(Point a)
  , simplex_vertex4 :: !(Point a)
  } deriving (Show, Eq)

type Triangulation a = [Simplex a]

type ActiveFaceList a = [Face a]

-- |Median extracted from hstats package
median :: (Fractional a, Ord a) => [a] -> a
median x
  | odd n = head $ drop (n `div` 2) x'
  | even n = mean $ take 2 $ drop i x'
  where
    i = (length x' `div` 2) - 1
    x' = sort x
    n = length x

-- |Numerically stable mean extracted from hstats package
mean :: Fractional a => [a] -> a
mean x = fst $ foldl' addElement (0, 0) x
  where
    addElement (!m, !n) x = (m + (x - m) / (n + 1), n + 1)

distPlane :: (Num a, Fractional a) => Point a -> Plane a -> a
distPlane p (PlaneX alpha) = abs (alpha - (x p))
distPlane p (PlaneY alpha) = abs (alpha - (y p))
distPlane p (PlaneZ alpha) = abs (alpha - (z p))

-- | A partition is a halfspace and the plane that produced it.
type Partition a = (Space a, Plane a)

splitSpace ::
     (Num a, Fractional a, Ord a) => Partition a -> (Partition a, Partition a)
splitSpace (space, PlaneX _) =
  ((fst halfspaces, PlaneY alpha), (snd halfspaces, PlaneY alpha))
  where
    alpha = median $ map (y . snd) $ HM.toList space
    halfspaces = HM.partition (\t -> y t < alpha) space
splitSpace (space, PlaneY _) =
  ((fst halfspaces, PlaneZ alpha), (snd halfspaces, PlaneZ alpha))
  where
    alpha = median $ map (z . snd) $ HM.toList space
    halfspaces = HM.partition (\t -> z t < alpha) space
splitSpace (space, PlaneZ _) =
  ((fst halfspaces, PlaneX alpha), (snd halfspaces, PlaneX alpha))
  where
    alpha = median $ map (x . snd) $ HM.toList space
    halfspaces = HM.partition (\t -> x t < alpha) space
