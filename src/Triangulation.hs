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
getX (x1, y1,_) (x2, y2,_) (x3, y3,_) = det2 (numa1, numa2) (numb1, numb2) /  det2 (dena1, dena2) (denb1, denb2)
  where numa1 = -2*(y3-y1)
        numa2 = -x3^2+x1^2-y3^2+y1^2
        numb1 = -2*(y2-y1)
        numb2 = -x2^2+x1^2-y2^2+y1^2
        dena1 = -2*(y3-y1)
        dena2 = -2*(y2-y1)
        denb1 = -2*(x3-x1)
        denb2 = -2*(x2-x1)

getY :: Point -> Point -> Point -> Double
getY (x1, y1,_) (x2, y2,_) (x3, y3,_) =  det2 (numa1, numa2) (numb1, numb2) /  det2 (dena1, dena2) (denb1, denb2)
  where numa1 = -2*(x3-x1)
        numa2 = -x3^2+x1^2-y3^2+y1^2
        numb1 = -2*(x2-x1)
        numb2 = -x2^2+x1^2-y2^2+y1^2
        dena1 = -2*(x3-x1)
        dena2 = -2*(x2-x1)
        denb1 = -2*(y3-y1)
        denb2 = -2*(y2-y1)
        
getBeta :: Point -> Point -> Point -> Double
getBeta (x1, y1, z1) (x2, y2, z2) (x3, y3, z3) = (det2 (x-x1, y-y1) (x2-x1, y2-y1)) / (det2 (x3-x1, y3-y1) (x2-x1, y2-y1))
  where x = getX (x1, y1, z1) (x2, y2, z2) (x3, y3, z3)
        y = getY (x1, y1, z1) (x2, y2, z2) (x3, y3, z3)

getAlpha :: Point -> Point -> Point -> Double
getAlpha (x1, y1, z1) (x2, y2, z2) (x3, y3, z3) = ( x - x1 - beta * (x3-x1)) / (x2-x1)
  where x = getX (x1, y1, z1) (x2, y2, z2) (x3, y3, z3)
        beta = getBeta (x1, y1, z1) (x2, y2, z2) (x3, y3, z3)

getZ :: Point -> Point -> Point -> Double
getZ (x1, y1, z1) (x2, y2, z2) (x3, y3, z3) = z1 + alpha*(z2-z1) + beta*(z3-z1)
  where alpha = getAlpha (x1, y1, z1) (x2, y2, z2) (x3, y3, z3)
        beta = getBeta (x1, y1, z1) (x2, y2, z2) (x3, y3, z3)

getCenter :: Point -> Point -> Point -> Point
getCenter p1 p2 p3 = (x, y, z)
  where x = getX p1 p2 p3
        y = getY p1 p2 p3
        z = getZ p1 p2 p3

getRadius :: Point -> Point -> Point -> Double
getRadius (x1, y1, z1) p2 p3 = sqrt $ (x1-x)^2 + (y1-y)^2 + (z1-z)^2
  where (x, y, z) = getCenter (x1, y1, z1) p2 p3

main :: IO ()
main =  do
  let list = [(1, (2,4,5)), (2,(5,9,3)), (3,(1,5,4)), (4,(7,2,9)),(5, (14,5,6)), (6,(3,7,9)), (7,(4,8,5))] :: [(Integer, Point)]
  let space = HM.fromList list
  let subdivision =  splitSpace space PlaneX
  let dist = distanceToPlane (space HM.! 1) PlaneX (snd subdivision)
  putStrLn $ show subdivision
  putStrLn $ show dist
