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

subsPoints :: Point -> Point -> Point
subsPoints (x1,y1,z1) (x2,y2,z2)= (x1-x2, y1-y2, z1-z2)

addPoints :: Point -> Point -> Point
addPoints (x1,y1,z1) (x2,y2,z2)= (x1+x2, y1+y2, z1+z2)

det2 :: (Double, Double) -> (Double,Double) -> Double
det2 (a1,a2) (b1,b2) = a1*b2 - a2*b1

-- Each Point is a row of the matrix
det3 :: Point -> Point -> Point -> Double
det3 (a1,a2,a3) (b1,b2,b3) (c1,c2,c3) = a1*(det2 (b2,b3) (c2,c3)) - a2*(det2 (b1,b3) (c1,c3)) + a1*(det2 (b1,b2) (c1,c2))

getX :: Point -> Point -> Point -> Double
getX (x1, y1,_) (x2, y2,_) (x3, y3,_) = ( det (-x3^2+x1^2-y3^2+y1^2,-2*(y3-y1)) (-x2^2+x1^2-y2^2+y1^2,2*(y2-y1)) ) / ( det (-2*(x3-x1),-2*(y3-y1)) (-2*(x2-x1),-2*(y2-y1))


getY :: Point -> Point -> Point -> Double
getY (x1, y1,_) (x2, y2,_) (x3, y3,_) = ( det (-2*(x3-x1),-x3^2+x1^2-y3^2+y1^2) (-2*(x3-x2),-x2^2+x1^2-y2^2+y1^2) ) / ( det (-2*(x3-x1),-2*(y3-y1)) (-2*(x3-x2),-2*(y3-y2))


getParameters :: Point -> Point -> Point -> (Double, Double)
getParameters (x1, y1,_) (x2, y2,_) (x3, y3,_) =
main :: IO ()
main =  do
  let list = [(1, (2,4,5)), (2,(5,9,3)), (3,(1,5,4)), (4,(7,2,9)),(5, (14,5,6)), (6,(3,7,9)), (7,(4,8,5))] :: [(Integer, Point)]
  let space = HM.fromList list
  let subdivision =  splitSpace space PlaneX
  let dist = distanceToPlane (space HM.! 1) PlaneX (snd subdivision)
  putStrLn $ show subdivision
  putStrLn $ show dist
