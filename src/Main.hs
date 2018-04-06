{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Csv
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL
import Graphics.UI.GLUT


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


main :: IO ()
main = do
  csvData <- BL.readFile "../assets/hygdata_v3.csv"
  let v = decodeByName csvData :: Either String (Header, V.Vector Star)
  putStrLn "hello world"
  as <- case v of
          Left err -> putStrLn err
          Right (_, vs) -> do
            (_progName, _args) <- getArgsAndInitialize
            --initialDisplayMode $= [WithDephBuffer,DoubleBuffered,RGBMode]
            _window <- createWindow "cg-project"
            displayCallback $= display vs
            reshapeCallback $= Just reshape
            putStrLn "Done drawing"
            mainLoop
  return ()

display :: V.Vector Star -> DisplayCallback
display stars = do
  clear [ ColorBuffer ]
  renderPrimitive Points $
    mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) (getPoints stars)
  flush

reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)
  postRedisplay Nothing

getPoints :: V.Vector Star -> [(GLdouble, GLdouble, GLdouble)]
getPoints stars = V.toList $ V.map (\s -> ((star_x s)/50 :: GLdouble, (star_y s)/50 :: GLdouble, 0.0 :: GLdouble)) stars

