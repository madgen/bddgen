module BDDGen.Algorithm where

import Prelude hiding (negate)

import BDDGen.Definition

convert :: BoolExp -> BDD
convert (Literal s) = node (Leaf True) s (Leaf False)
convert (Neg exp) = negate . convert $ exp
convert (And exp1 exp2) = combine (&&) (convert exp1) (convert exp2)
convert (Or exp1 exp2) = combine (||) (convert exp1) (convert exp2)
convert (Imply exp1 exp2) =
  combine (\a b -> not a || b) (convert exp1) (convert exp2)
convert (Iff exp1 exp2) =
  combine (\a b -> (not a && not b) || (a && b)) (convert exp1) (convert exp2)

combine :: (Bool -> Bool -> Bool) -> BDD -> BDD -> BDD
combine op (Leaf b1) (Leaf b2) = Leaf $ op b1 b2
combine op l@Leaf{} n@(Node _ s _ _)  = combine op (wrap s l) n
combine op n@(Node _ s _ _) l@Leaf{}  = combine op n (wrap s l)
combine op n1@(Node ln s1 rn id1) n2@(Node ln' s2 rn' id2)
  | s1 < s2 = combine op n1 (wrap s1 n2)
  | s1 > s2 = combine op (wrap s2 n1) n2
  | otherwise =
    let bdd1 = combine op ln ln'
        bdd2 = combine op rn rn'
    in if bdd1 == bdd2 then bdd1 else node bdd1 s1 bdd2 

wrap :: String -> BDD -> BDD
wrap s n = node n s n

negate :: BDD -> BDD
negate (Node bdd1 s bdd2 id) = Node (negate bdd1) s (negate bdd2) ('!':id)
negate (Leaf b) = Leaf $ not b
