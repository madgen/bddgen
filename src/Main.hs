module Main where

import BDD.Algorithm
import BDD.BoolParser
import BDD.GraphGenerator

main = do
  s <- getContents 
  case parseBool s of
    Left e -> error $ show e
    Right b -> putStrLn . generateGraph . convert $ b
