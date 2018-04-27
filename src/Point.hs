{-# LANGUAGE FlexibleContexts #-}

module Point where

data Point a = Point
  { x :: !a
  , y :: !a
  , z :: !a
  } deriving (Show, Eq)

instance Functor Point where
  fmap f (Point x1 y1 z1) = Point (f x1) (f y1) (f z1)

(|+|) :: Num a => Point a -> Point a -> Point a
(|+|) (Point x1 y1 z1) (Point x2 y2 z2) = Point (x1+x2) (y1+y2) (z1+z2) 

(|*|) :: Num a => a -> Point a -> Point a
(|*|) s = fmap (*s)

(|.|) :: Num a => Point a -> Point a -> a
(|.|) (Point x1 y1 z1) (Point x2 y2 z2) =  x1*x2 + y1*y2 + z1*z2

(|-|) :: Num a => Point a -> Point a -> Point a
(|-|) a b = a |+| ((-1)|*|b)

distance :: Num a => Point a -> Point a -> a
distance a b = (a|-|b)|.|(a|-|b)

det2 :: Num a => (a,a) -> (a,a) -> a
det2 (x1,y1) (x2,y2) = x1*y2 - x2*y1

det3 :: Num a => Point a -> Point a -> Point a -> a
det3 (Point x1 y1 z1) (Point x2 y2 z2) (Point x3 y3 z3) = let a = det2 (y2,z2) (y3,z3)
                                                              b = det2 (x2,z2) (x3,z3)
                                                              c = det2 (x2,y2) (x3,y3)
                                                           in x1*a - y1*b + z1*c

radiusSphere :: (Num a, Fractional a) => Point a -> Point a -> Point a -> Point a -> a
radiusSphere a b c d = let ba = ((-2)|*|(b|-|a))
                           ca = ((-2)|*|(c|-|a))
                           da = ((-2)|*|(d|-|a))
                           detD = det3 ba ca da
                           alpha = (b|.|b) - (a|.|a)
                           beta = (c|.|c) - (a|.|a)
                           gamma = (d|.|d) - (a|.|a)
                           detDx = det3 (Point alpha (y ba) (z ba)) (Point beta (y ca) (z ca)) (Point gamma (y da) (z da))
                           detDy = det3 (Point (x ba) alpha (z ba)) (Point (x ca) beta (z ca)) (Point (x da) gamma (z da))
                           detDz = det3 (Point (x ba) (y ba) alpha) (Point (x ca) (y ca) beta) (Point (x da) (y da) gamma)
                           center = Point (detDx/detD) (detDy/detD) (detDz/detD)
                        in (b|-|center)|.|(b|-|center)


radiusCircle :: (Num a, Fractional a) => Point a -> Point a -> Point a -> a
radiusCircle a b c = let ba = b|-|a
                         ca = c|-|a
                         ba' = b|+|a
                         ca' = c|+|a
                         detD = det2 ((-2)*(x ba),(-2)*(y ba)) ((-2)*(x ca),(-2)*(y ca))
                         alpha = (-1)*((x ba)*(x ba') + (y ba)*(y ba'))
                         beta = (-1)*((x ca)*(x ca') + (y ca)*(y ca'))
                         detDx = det2 (alpha,(-2)*(y ba)) (beta,(-2)*(y ca))
                         detDy = det2 ((-2)*(x ba),alpha) ((-2)*(x ca),beta)
                         centerX = detDx/detD
                         centerY = detDy/detD
                         gamma = (det2 (centerX - (x a),centerY - (y a)) (x ba,y ba))/(det2 (x ca,y ca) (x ba,y ba))
                         teta = (centerX - (x a) - gamma*(x ca))/(x ba)
                         centerZ = (z a) + teta*(z ba) + gamma*(z ca)
                         center = Point centerX centerY centerZ
                      in (b|-|center)|.|(b|-|center)


