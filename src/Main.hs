{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Csv
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL


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
          Right (_, vs) -> V.forM_ vs (putStrLn . show)
  return ()
