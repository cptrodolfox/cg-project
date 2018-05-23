{-# LANGUAGE FlexibleContexts #-}

module NSCABDT where

import qualified Data.HashMap as HM
import Data.List
import Point
import Triangulation

lengthEdge :: (Num a, Floating a) => (Point a, Point a) -> a
lengthEdge (p1,p2) = sqrt $ distance p1 p2

globalMean :: (Num a, Fractional a, Floating a) => [(Point a, Point a)] -> a
globalMean [] = 0
globalMean edges = let l = length edges
                    in sum [lengthEdge x | x <- edges ] / fromIntegral l

globalStd ::  (Num a, Fractional a, Floating a) => [(Point a, Point a)] -> a
globalStd [] = 0
globalStd edges = let mean = globalMean edges
                      l = fromIntegral $ length edges
                    in sqrt $ sum [ (mean - lengthEdge x)^2 | x <- edges ] / l

adjacentEdge :: (Eq a, Num a) => [(Point a, Point a)] ->  Point a -> [(Point a, Point a)]
adjacentEdge [] _ = []
adjacentEdge edges p = [x | x <- edges , fst x == p || snd x == p]

adjacentNode :: (Eq a, Num a) => [(Point a, Point a)] ->  Point a -> [Point a]
adjacentNode [] _ = []
adjacentNode edges p = let adjedge = adjacentEdge edges p
                        in [ if fst x == p then snd x else fst x | x <- adjedge ]

localMean :: (Eq a, Num a, Fractional a, Floating a) => [(Point a, Point a)] -> Point a -> a
localMean edges p = let localedges = adjacentEdge edges p 
                        l = fromIntegral $ length localedges
                      in sum [lengthEdge x | x <- localedges ] / l 

relativeMean :: (Eq a, Num a, Fractional a, Floating a) => [(Point a, Point a)] ->  Point a -> a
relativeMean edges p = let gmean = globalMean edges
                           lmean = localMean edges p
                       in gmean / lmean

criterionFunction :: (Eq a, Num a, Fractional a, Floating a) => [(Point a, Point a)] ->  Point a -> a
criterionFunction edges p = let gmean = globalMean edges
                                gstd = globalStd edges
                                rmean = relativeMean edges p
                            in gmean + gstd * rmean

medianlengthEdge :: (Floating a, Fractional a, Ord a) => [(Point a, Point a)] -> a
medianlengthEdge edges = median [ lengthEdge  x | x <- edges ]

removeEdge :: (Ord a, Floating a) => [(Point a, Point a)] -> a -> [(Point a, Point a)]
removeEdge [] _ = []
removeEdge (y:ys) cf  | cf <= l   = removeEdge ys cf
                      | otherwise = y : removeEdge ys cf
              where l = lengthEdge y
              
spatialClustering :: (Eq a, Ord a, Floating a, Num a) => [(Point a, Point a)] ->  Point a -> [(Point a, Point a)]
spatialClustering edges p = let cf = criterionFunction edges p
                            in removeEdge edges cf 

intersection :: (Eq a) => [[a]] -> [a]
intersection = foldr1 intersect

unionSet :: (Eq a) => [[a]] -> [a]
unionSet = foldr1 union

main :: IO()
main = do
  let p1 = Point 0.0 0.0 0.0
      p2 = Point 1.0 1.0 0.0
      p3 = Point 2.0 1.0 0.0
      p4 = Point 1.0 2.0 0.0
      p5 = Point 1.5 0.5 0.0
      p6 = Point 2.5 0.5 0.0
      p7 = Point 3.0 2.5 0.0
      points = [ p1, p2, p3, p4, p5, p6, p7]
      edges = [(p1,p2),(p1,p4),(p1,p5),(p1,p6),(p2,p3),(p2,p4),(p2,p5),(p3,p5),(p3,p6),(p3,p7),(p3,p4),(p4,p7),(p5,p6),(p6,p7)]
      b = [spatialClustering edges x | x <- points ]
  print $ intersection b
  return()