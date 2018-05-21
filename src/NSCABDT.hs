{-# LANGUAGE FlexibleContexts #-}

module NSCABDT where

import qualified Data.HashMap as HM
import Data.List
import Point

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