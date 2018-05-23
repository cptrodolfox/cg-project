module Point where

-- | A 3D point.
data Point a = Point
  { x :: !a
  , y :: !a
  , z :: !a
  } deriving (Show, Eq)

-- | Instance of Functor for Point.
instance Functor Point where
  fmap f (Point x1 y1 z1) = Point (f x1) (f y1) (f z1)

-- | Vector addition.
(|+|) :: Num a => Point a -> Point a -> Point a
(|+|) (Point x1 y1 z1) (Point x2 y2 z2) = Point (x1 + x2) (y1 + y2) (z1 + z2)

-- | Vector scalar product
(|*|) :: Num a => a -> Point a -> Point a
(|*|) s = fmap (* s)

-- | Vector inner product.
(|.|) :: Num a => Point a -> Point a -> a
(|.|) (Point x1 y1 z1) (Point x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

-- | Vector Substraction
(|-|) :: Num a => Point a -> Point a -> Point a
(|-|) a b = a |+| ((-1) |*| b)

-- | Euclidean distance between Points.
distance :: Num a => Point a -> Point a -> a
distance a b = (a |-| b) |.| (a |-| b)

-- | Determinant of 2 x 2 matrix
det2 :: Num a => (a, a) -> (a, a) -> a
det2 (x1, y1) (x2, y2) = x1 * y2 - x2 * y1

-- | Determinant of 3 x 3 matrix
det3 :: Num a => Point a -> Point a -> Point a -> a
det3 (Point x1 y1 z1) (Point x2 y2 z2) (Point x3 y3 z3) =
  let a = det2 (y2, z2) (y3, z3)
      b = det2 (x2, z2) (x3, z3)
      c = det2 (x2, y2) (x3, y3)
   in x1 * a - y1 * b + z1 * c

{-| Center of the sphere that passes through four Points.
    The center is obtained by solving:
      (x-x1)^2 + (y-y1)^2 + (z-z1)^2 - r^2 = 0
      (x-x2)^2 + (y-y2)^2 + (z-z2)^2 - r^2 = 0
      (x-x3)^2 + (y-y3)^2 + (z-z3)^2 - r^2 = 0
      (x-x4)^2 + (y-y4)^2 + (z-z4)^2 - r^2 = 0
      where (x,y,z) are coordinates of the center point.
-}
centerSphere ::
     (Num a, Fractional a)
  => Point a
  -> Point a
  -> Point a
  -> Point a
  -> Point a
centerSphere pa pb pc pd =
  let ba = ((-2) |*| (pb |-| pa))
      ca = ((-2) |*| (pc |-| pa))
      da = ((-2) |*| (pd |-| pa))
      detD = det3 ba ca da
      alpha = (pb |.| pb) - (pa |.| pa)
      beta = (pc |.| pc) - (pa |.| pa)
      gamma = (pd |.| pd) - (pa |.| pa)
      detDx =
        det3
          (Point alpha (y ba) (z ba))
          (Point beta (y ca) (z ca))
          (Point gamma (y da) (z da))
      detDy =
        det3
          (Point (x ba) alpha (z ba))
          (Point (x ca) beta (z ca))
          (Point (x da) gamma (z da))
      detDz =
        det3
          (Point (x ba) (y ba) alpha)
          (Point (x ca) (y ca) beta)
          (Point (x da) (y da) gamma)
   in Point (detDx / detD) (detDy / detD) (detDz / detD)

{-|
    The radius of the Sphere that passes through the four points.
-}
radiusSphere ::
     (Num a, Fractional a) => Point a -> Point a -> Point a -> Point a -> a
radiusSphere pa pb pc pd =
  let center = centerSphere pa pb pc pd
   in distance pa center

{-|
    Center of the circumcricle of 3 Points, the center is obtained
    by solving the following system of equations:
      (x-x1)^2 + (y-y1)^2 + (z-z1)^2 - r^2 = 0
      (x-x2)^2 + (y-y2)^2 + (z-z2)^2 - r^2 = 0
      (x-x3)^2 + (y-y3)^2 + (z-z3)^2 - r^2 = 0
      x  = x1 + alpha(x2-x1) + beta(x3-x1)
      y  = y1 + alpha(y2-y1) + beta(y3-y1)
      z  = z1 + alpha(z2-z1) + beta(z3-z1)
    where (x,y,z) are the coordinates of the center.
-}
centerCircle ::
     (Num a, Fractional a) => Point a -> Point a -> Point a -> Point a
centerCircle pa pb pc =
  let ba = pb |-| pa
      ca = pc |-| pa
      bb = pb |.| pb
      aa = pa |.| pa
      cc = pc |.| pc
      h = bb - aa - 2 * (pa |.| ba)
      k = cc - aa - 2 * (pa |.| ca)
      detD =
        det2
          ((-2) * (ba |.| ba), (-2) * (ca |.| ba))
          ((-2) * (ba |.| ca), (-2) * (ca |.| ca))
      detDa = det2 (h, (-2) * (ca |.| ba)) (k, (-2) * (ca |.| ca))
      detDb = det2 ((-2) * (ba |.| ba), h) ((-2) * (ba |.| ca), k)
      alpha = detDa / detD
      beta = detDb / detD
      aba = alpha |*| ba
      bca = beta |*| ca
   in pa |+| (aba |+| bca)

{-|
    The radius of the circumcircle of three points.  
-}
radiusCircle :: (Num a, Fractional a) => Point a -> Point a -> Point a -> a
radiusCircle pa pb pc =
  let center = centerCircle pa pb pc
   in distance pa center


