module Triangulation where

import Control.Monad.State
import Data.List (elemIndex, sortOn)
import Data.Map (Map, (!), adjust, elemAt, foldrWithKey)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Point

-- | In the Space all points (Vertex) are identified as Integers.
type Vertex = Integer

-- | The Space that contains all the points is implemented as a HashMap to have
--  O(1) operations.
type Space a = Map Vertex (Point a)

-- | The Plane that cuts the Space
data Plane a
  = PlaneX a -- Plane alpha parallel YZ
  | PlaneY a -- Plane alpha parallel XZ
  | PlaneZ a -- Plane alpha parallel XY
  deriving (Show, Eq)

-- | Distance of a Point to a Plane.
dPlane :: (Num a, Fractional a) => Point a -> Plane a -> a
dPlane p (PlaneX alpha) = abs (alpha - x p)
dPlane p (PlaneY alpha) = abs (alpha - y p)
dPlane p (PlaneZ alpha) = abs (alpha - z p)

-- | Distance of a Vertex to a Plane.
dVPlane :: (Num a, Fractional a) => Space a -> Plane a -> Vertex -> a
dVPlane space plane vertex = dPlane point plane
  where
    point = space ! vertex

-- | A 3D face, a triangle.
data Face =
  Face Vertex
       Vertex
       Vertex
  deriving (Show, Eq)

-- | A 3D simplex,
data Simplex =
  Simplex Vertex
          Vertex
          Vertex
          Vertex
  deriving (Show, Eq)

-- | Side of Vertex to respect to a Plane.
data PlaneSide
  = OnLeft -- ^The Vertex is on the "Left" of the plane.
  | OnRight -- ^The Vertex is on the "Right" of the plane.
  deriving (Show, Eq, Ord)

-- | The Halfspaces obtained by the partition of the space by a plane.
type HalfSpaces = Map PlaneSide [Vertex]

-- | An empty HalfSpaces.
emptyHalfSpaces :: HalfSpaces
emptyHalfSpaces = M.fromList [(OnLeft, []), (OnRight, [])]

-- | Changes the Side of a Plane.
changeSide :: PlaneSide -> PlaneSide
changeSide OnLeft = OnRight
changeSide OnRight = OnLeft

auxSplitSpace ::
     (Num a, Ord a, Fractional a)
  => Plane a
  -> Vertex
  -> Point a
  -> HalfSpaces
  -> HalfSpaces
auxSplitSpace plane vertex point halfSpaces =
  let side = whichSideOfPlane plane point
   in adjust (vertex :) side halfSpaces

-- | Splits the space by a Plane.
splitSpaceByPlane ::
     (Num a, Ord a, Fractional a) => Space a -> Plane a -> HalfSpaces
splitSpaceByPlane space plane =
  foldrWithKey (auxSplitSpace plane) emptyHalfSpaces space

-- | Determines the side of the plane where point is.
whichSideOfPlane ::
     (Num a, Ord a, Fractional a) => Plane a -> Point a -> PlaneSide
whichSideOfPlane (PlaneX alpha) point =
  if alpha > x point
    then OnLeft
    else OnRight
whichSideOfPlane (PlaneY alpha) point =
  if alpha > y point
    then OnLeft
    else OnRight
whichSideOfPlane (PlaneZ alpha) point =
  if alpha > z point
    then OnLeft
    else OnRight

-- | Given to Vertex find the closest to the plane, auxilary function to the foldr in closestVertexToPlane
closestVertexFun ::
     (Num a, Fractional a, Ord a)
  => Plane a
  -> Vertex
  -> Point a
  -> (Vertex, Point a)
  -> (Vertex, Point a)
closestVertexFun plane newVertex point old =
  let oldDistance = dPlane (snd old) plane
      newDistance = dPlane point plane
   in if newDistance > oldDistance
        then (newVertex, point)
        else old

-- | Obtains the Vertex from a space that is closest to the plane, if there is more than one that is closest
-- choose the first one.
closestVertexToPlane ::
     (Num a, Fractional a, Ord a) => Space a -> Plane a -> Vertex
closestVertexToPlane space plane =
  fst $ foldrWithKey (closestVertexFun plane) (elemAt 0 space) space

-- | Given a list of vertex find the one closes to the point.
closestVertexToPoint ::
     (Num a, Fractional a, Ord a) => Space a -> Point a -> [Vertex] -> Vertex
closestVertexToPoint space point vertexes =
  let points = map (space !) vertexes
      dists = map (distance point) points
   in snd $ head $ sortOn fst $ zip dists vertexes

-- | Find the Vertex closest to the point at the other side of the plane. 
closestVertexOtherSide ::
     (Num a, Fractional a, Ord a)
  => Space a
  -> HalfSpaces
  -> PlaneSide
  -> Point a
  -> Vertex
closestVertexOtherSide space halfSpaces side point =
  let otherSide = changeSide side
      vertexes = halfSpaces ! side
   in closestVertexToPoint space point vertexes

-- | Helper function for finding third vertex in make first simplex.
auxTrdPoint ::
     (Num a, Fractional a, Ord a)
  => Point a
  -> Point a
  -> Space a
  -> Vertex
  -> Point a
  -> Vertex
  -> Vertex
auxTrdPoint fv sv space newVertex point oldVertex =
  let oldPoint = space ! oldVertex
      oldRadius = radiusCircle fv sv oldPoint
      newRadius = radiusCircle fv sv point
   in if fv == point || sv == point
        then oldVertex
        else if newRadius < oldRadius
               then newVertex
               else oldVertex

-- | Helper function for finding fourth vertex in make frist simplex.
auxFthPoint ::
     (Num a, Fractional a, Ord a)
  => Point a
  -> Point a
  -> Point a
  -> Space a
  -> Vertex
  -> Point a
  -> Vertex
  -> Vertex
auxFthPoint fv sv tv space nVertex point oVertex =
  let oldPoint = space ! oVertex
      oldRadius = radiusSphere fv sv tv oldPoint
      newRadius = radiusSphere fv sv tv point
   in if fv == point || sv == point || tv == point
        then oVertex
        else if newRadius < oldRadius
               then nVertex
               else oVertex

-- | Make first simplex based on the DeWall algorithm.
makeFirstSimplex ::
     (Num a, Fractional a, Ord a) => Space a -> Plane a -> HalfSpaces -> Simplex
makeFirstSimplex space plane halfSpaces =
  let closest = closestVertexToPlane space plane
      sideOfClosest = whichSideOfPlane plane (space ! closest)
      sndVertex =
        closestVertexOtherSide space halfSpaces sideOfClosest (space ! closest)
      trdVertex =
        foldrWithKey
          (auxTrdPoint (space ! closest) (space ! sndVertex) space)
          sndVertex
          space
      fthVertex =
        foldrWithKey
          (auxFthPoint
             (space ! closest)
             (space ! sndVertex)
             (space ! trdVertex)
             space)
          trdVertex
          space
   in Simplex closest sndVertex trdVertex fthVertex
  
-- | Auxilary function for the Delaunay Distance, checks if the Vertex (center of a circumsphere) belongs to the halfspace that contains the tetrahedra (formed by the face and point).
--auxDelaunayDistance :: (Num a, Fractional a, Ord a) => Space a -> Face -> Point a -> Bool
--auxDelaunayDistance space (Face v1 v2 v3) point = let p1 = space ! v1
--                                                      p2 = space ! v2
--                                                      p3 = space ! v3
--                                                      center = centerSphere p1 p2 p3 point
--                                                   in 
-- | Delaunay distance 
--delaunayDistance :: (Num a, Fractional a, Ord a) => Space a -> Face -> Point a
--delaunayDistance space (Face v1 v2 v3) point = let p1 = space ! v1
--                                                   p2 = space ! v2
--                                                   p3 = space ! v3
--                                                   radius  = radiusSphere p1 p2 p3 point
--                                                in if 

