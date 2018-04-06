{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where
import Data.Csv
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL
import Graphics.UI.GLUT
import Data.IORef

data Star = Star { star_id :: !Integer
                 , star_x :: !Double
                 , star_y :: !Double
                 , star_z :: !Double}
            deriving(Show)
instance FromNamedRecord Star where
  parseNamedRecord m = Star
                       <$> m .: "id"
                       <*> m .: "x"
                       <*> m .: "y"
                       <*> m .: "z"

-- Max distance of star for distance normalization purposes
maxDistance :: GLdouble
maxDistance = 50.0

-- Helper Functions
modVar v f = do
  v' <- get v
  v $= (f v')
mapFst f (x,y) = (f x,y)
mapSnd f (x,y) = (x,f y)

-- Main
main :: IO ()
main = do
  csvData <- BL.readFile "../assets/hygdata_v3.csv"
  let v = decodeByName csvData :: Either String (Header, V.Vector Star)
  putStrLn "hello world"
  as <- case v of
          Left err -> putStrLn err
          Right (_, vs) -> do
            (_progName, _args) <- getArgsAndInitialize
            initialDisplayMode $= [WithDepthBuffer,DoubleBuffered,RGBMode]
            _window <- createWindow "3D cg-project"
            depthFunc $= Just Less
            windowSize $= Size 800 600
            angle <- newIORef ((35,0)::(GLfloat, GLfloat))
            zoom <- newIORef (2::GLfloat)
            campos <- newIORef ((0.7,0)::(GLfloat,GLfloat))
            idleCallback $= Just idle
            keyboardMouseCallback $= Just (keyboardMouse angle zoom campos)
            displayCallback $= display vs angle zoom campos
            --reshapeCallback $= Just reshape
            putStrLn "Done drawing"
            mainLoop
  return ()

-- The idle changes the states
idle = postRedisplay Nothing

-- Keyboard and Mouse controls
keyboardMouse angle zoom campos key state modifiers position =
  keyaction angle zoom campos key state
  where
    keyaction a z p (Char ' ') Down = do
      a $= (0,0)
      z $= 1
      p $= (0,0)
    -- Use hjkl to rotate
    keyaction a _ _ (Char 'h') Down = modVar a (mapFst (+0.5))
    keyaction a _ _ (Char 'l') Down = modVar a (mapFst (+(-0.5)))
    keyaction a _ _ (Char 'j') Down = modVar a (mapSnd (+0.5))
    keyaction a _ _ (Char 'k') Down = modVar a (mapSnd (+(-0.5)))
    -- use oi to zoom
    keyaction _ z _ (Char 'i') Down = modVar z (*1.1)
    keyaction _ z _ (Char 'o') Down = modVar z (*0.9)
    -- use sdfe to move the camera
    keyaction _ _ p (Char 's') Down = modVar p (mapFst (+0.1))
    keyaction _ _ p (Char 'f') Down = modVar p (mapFst (+(-0.1)))
    keyaction _ _ p (Char 'd') Down = modVar p (mapSnd (+0.1))
    keyaction _ _ p (Char 'e') Down = modVar p (mapSnd (+(-0.1)))
    -- any other key do nothing
    keyaction _ _ _ _ _= return ()

-- DisplayFunction
--display :: V.Vector Star -> DisplayCallback
display stars angle zoom position = do
  clearColor $= Color4 0 0.1686 0.2117 1
  clear [ ColorBuffer, DepthBuffer ]
  loadIdentity
  (x,y) <- get position
  translate $ Vector3 x y 0
  z <- get zoom
  scale z z z
  (xangle,yangle) <- get angle
  rotate xangle $ Vector3 1.0 0.0 (0.0::GLfloat)
  rotate yangle $ Vector3 0.0 1.0 (0.0::GLfloat)
  renderPrimitive Points $
    mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) (getPoints stars)
  swapBuffers
  --flush

reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)
  postRedisplay Nothing

getPoints :: V.Vector Star -> [(GLdouble, GLdouble, GLdouble)]
getPoints stars = V.toList $ V.map (\s -> ((star_x s)/maxDistance :: GLdouble
                                          , (star_y s)/maxDistance :: GLdouble
                                          , (star_z s)/maxDistance :: GLdouble)) stars

