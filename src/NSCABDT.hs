{-# LANGUAGE FlexibleContexts #-}

module NSCABDT where

import qualified Data.HashMap as HM
import Data.List
import Point

adjacentEdge :: Edges ->  Points -> HM.Map Integer Edges