{-# LANGUAGE BangPatterns #-}
module Triangulation where

import qualified Data.HashMap as HM
import Data.List

type Point = (Double, Double, Double)

type Space = HM.Map Integer Point

data Plane =  PlaneX
           | PlaneY
           | PlaneZ

-- 2-D Face 
data Face = Face { face_vertex1 :: !Point
                 , face_vertex2 :: !Point
                 , face_vertex3 :: !Point
                 }deriving (Show, Eq)

data Simplex = Simplex { simplex_vertex1 :: !Point
                       , simplex_vertex2 :: !Point
                       , simplex_vertex3 :: !Point
                       , simplex_vertex4 :: !Point
                       }deriving (Show, Eq)

type Triangulation = [Simplex]
type ActiveFaceList = [Face]

-- |Median extracted from hstats package
median :: (Fractional a, Ord a) => [a] -> a
median x | odd n  = head  $ drop (n `div` 2) x'
         | even n = mean $ take 2 $ drop i x'
                  where i = (length x' `div` 2) - 1
                        x' = sort x
                        n  = length x

-- |Numerically stable mean extracted from hstats package
mean :: Fractional a => [a] -> a
mean x = fst $ foldl' addElement (0,0) x
    where
      addElement (!m,!n) x = (m + (x-m)/(n+1), n+1)


hFst :: (a, a, a) -> a
hFst (x,_,_) = x

hSnd :: (a, a, a) -> a
hSnd (_,x,_) = x

hTrd :: (a, a, a) -> a
hTrd (_,_,x) = x

splitSpace :: Space -> Plane -> ((Space, Space), Double)
splitSpace space PlaneX = (halfspaces, alpha)
  where alpha = median (map hFst (map snd (HM.toList space)))
        halfspaces = HM.partition (\(x,_,_) -> x < alpha) space
splitSpace space PlaneY = (halfspaces, alpha)
  where alpha = median (map hSnd (map snd (HM.toList space)))
        halfspaces = HM.partition (\(_,x,_) -> x < alpha) space
splitSpace space PlaneZ = (halfspaces, alpha)
  where alpha = median (map hTrd (map snd (HM.toList space)))
        halfspaces = HM.partition (\(_,_,x) -> x < alpha) space

distanceToPlane :: Point -> Plane -> Double -> Double
distanceToPlane (x,_,_) PlaneX alpha = abs (alpha - x)
distanceToPlane (_,x,_) PlaneY alpha = abs (alpha - x)
distanceToPlane (_,_,x) PlaneZ alpha = abs (alpha - x)


main :: IO ()
main =  do
  let list = [(1, (2,4,5)), (2,(5,9,3)), (3,(1,5,4)), (4,(7,2,9)),(5, (14,5,6)), (6,(3,7,9)), (7,(4,8,5))] :: [(Integer, Point)]
  let space = HM.fromList list
  let subdivision =  splitSpace space PlaneX
  let dist = distanceToPlane (space HM.! 1) PlaneX (snd subdivision)
  putStrLn $ show subdivision
  putStrLn $ show dist

