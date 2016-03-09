{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (negate)

import BoolParser

import Data.GraphViz.Printing (toDot, renderDot)
import Data.GraphViz.Types.Monadic
import qualified Data.Text.Lazy as T
import Data.List (group, sort)
import Data.GraphViz.Types.Generalised
import Data.GraphViz.Attributes.Complete ( Attribute(..)
                                         , StyleItem(..)
                                         , StyleName(..) 
                                         , Label(..)
                                         , Shape(..) )

data BDD = Node BDD String BDD | Leaf Bool
  deriving (Show, Eq)

convert :: BoolExp -> BDD
convert (Literal s) = Node (Leaf True) s (Leaf False)
convert (Neg exp) = negate . convert $ exp
convert (And exp1 exp2) = combine (&&) (convert exp1) (convert exp2)
convert (Or exp1 exp2) = combine (||) (convert exp1) (convert exp2)
convert (Imply exp1 exp2) =
  combine (\a b -> not a || b) (convert exp1) (convert exp2)
convert (Iff exp1 exp2) =
  combine (\a b -> (not a && not b) || (a && b)) (convert exp1) (convert exp2)

combine :: (Bool -> Bool -> Bool) -> BDD -> BDD -> BDD
combine op (Leaf b1) (Leaf b2) = Leaf $ op b1 b2
combine op l@Leaf{} n@(Node _ s _)  = combine op (wrap s l) n
combine op n@(Node _ s _) l@Leaf{}  = combine op n (wrap s l)
combine op n1@(Node ln s1 rn) n2@(Node ln' s2 rn')
  | s1 < s2 = combine op n1 (wrap s1 n2)
  | s1 > s2 = combine op (wrap s2 n1) n2
  | otherwise =
    let bdd1 = combine op ln ln'
        bdd2 = combine op rn rn'
    in if bdd1 == bdd2 then bdd1 else Node bdd1 s1 bdd2

wrap :: String -> BDD -> BDD
wrap s n = Node n s n

negate :: BDD -> BDD
negate (Node bdd1 s bdd2) = Node (negate bdd1 ) s (negate bdd2)
negate (Leaf b) = Leaf $ not b

type Name = T.Text

name :: BDD -> String
name (Leaf b) = show b
name (Node bdd1 s bdd2) = name bdd1 ++ s ++ name bdd2

type Edges = [ (Name, Bool, Name) ]
type Nodes = [ (Name, Label) ]

nodesAndEdges :: BDD -> (Nodes, Edges)
nodesAndEdges (Leaf b) = ( [ (T.pack $ show b, StrLabel $ T.pack $ show b) ], [] )
nodesAndEdges n@(Node b1 s b2) =
      let sb1 = label b1
          sb2 = label b2
          (nodes', edges') = nodesAndEdges b1
          (nodes'', edges'') = nodesAndEdges b2
          curLabel = T.pack $ name n
          curNode = (curLabel, StrLabel $ T.pack s)
      in ( curNode : nodes' ++ nodes''
         , (curLabel , True, T.pack $ name b1) : 
           (curLabel , False, T.pack $ name b2) : 
           edges' ++ edges'')
  where
    label bdd = case bdd of { Leaf b -> show b; Node _ s _ -> s }

toGraph :: (Nodes, Edges) -> DotGraph T.Text
toGraph (nodes, edges) = digraph (Str "BDD") $ do
    mapM_ nodeGen uniqNodes 
    mapM_ edgeGen uniqEdges
  where
    uniqNodes = map head . group . sort $ nodes
    uniqEdges = map head . group . sort $ edges
    edgeGen (n, True, n') = edge n n' [ Style [ SItem Solid [] ] ]
    edgeGen (n, False, n') = edge n n' [ Style [ SItem Dashed [] ] ]
    nodeGen (n,l) 
      | n == "True" = node "True" [ Label $ StrLabel $ T.pack "1", Shape Square ]
      | n == "False" = node "False" [ Label $ StrLabel $ T.pack "0", Shape Square ]
      | otherwise = node n [ Label l ] 

main = do
    s <- getContents 
    case parseBool s of
      Left e -> error $ show e
      Right b ->
        putStrLn . T.unpack . renderDot . toDot . toGraph . nodesAndEdges . convert $ b
