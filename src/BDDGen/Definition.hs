module BDDGen.Definition (BoolExp(..), BDD(..), toID, node) where

data BoolExp = 
    And BoolExp BoolExp
  | Or BoolExp BoolExp
  | Neg BoolExp
  | Imply BoolExp BoolExp
  | Iff BoolExp BoolExp
  | Literal String
  deriving Show

type ID = String
data BDD = Node BDD String BDD ID | Leaf Bool
  deriving (Show, Eq)

toID :: BDD -> String
toID (Leaf True) = "+"
toID (Leaf False) = "-"
toID (Node _ _ _ id) = id

node :: BDD -> String -> BDD -> BDD
node bdd1 s bdd2 = Node bdd1 s bdd2 (toID bdd1 ++ s ++ toID bdd2)
