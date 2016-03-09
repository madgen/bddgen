{-# LANGUAGE OverloadedStrings #-}

module BDD.GraphGenerator (generateGraph) where

import qualified Data.Text.Lazy as T

import Data.GraphViz.Printing (toDot, renderDot)
import Data.GraphViz.Types.Monadic
import Data.GraphViz.Types.Generalised
import Data.GraphViz.Attributes.Complete ( Attribute(..)
                                         , StyleItem(..)
                                         , StyleName(..) 
                                         , Label(..)
                                         , Shape(..) )

import Data.List (group, sort)

import BDD.Definition (BDD(..))

type Name = T.Text

name :: BDD -> String
name (Leaf b) = show b
name (Node bdd1 s bdd2) = name bdd1 ++ s ++ name bdd2

type Graph = (Nodes, Edges)
type Edges = [ (Name, Bool, Name) ]
type Nodes = [ (Name, Label) ]

nodesAndEdges :: BDD -> Graph
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

toGraph :: Graph -> DotGraph T.Text
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

generateGraph :: BDD -> String
generateGraph = T.unpack . renderDot . toDot . toGraph . nodesAndEdges
