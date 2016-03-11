module Main where

import BDDGen.Algorithm
import BDDGen.BoolParser
import BDDGen.GraphGenerator

main = do
  s <- getContents 
  case parseBool s of
    Left e -> error $ show e
    Right b -> putStrLn . generateGraph (title s) . convert $ b
  where
    title = filter (not . flip elem ['\n','\r'])
