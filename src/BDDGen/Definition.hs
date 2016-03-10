module BDDGen.Definition where

data BoolExp = 
    And BoolExp BoolExp
  | Or BoolExp BoolExp
  | Neg BoolExp
  | Imply BoolExp BoolExp
  | Iff BoolExp BoolExp
  | Literal String
  deriving Show

data BDD = Node BDD String BDD | Leaf Bool
  deriving (Show, Eq)
