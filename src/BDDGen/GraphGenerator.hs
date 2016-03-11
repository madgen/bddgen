{-# LANGUAGE OverloadedStrings #-}

module BDDGen.GraphGenerator (generateGraph) where

import qualified Data.Text.Lazy as T

import Data.GraphViz.Printing (toDot, renderDot)
import Data.GraphViz.Types.Monadic
import Data.GraphViz.Types.Generalised
import Data.GraphViz.Attributes.Complete ( Attribute(..)
                                         , StyleItem(..)
                                         , StyleName(..) 
                                         , Label(..)
                                         , VerticalPlacement(..)
                                         , Shape(..) )

import Data.List (group, sort)

import BDDGen.Definition (BDD(..), toID)

type Name = T.Text

type Graph = (Nodes, Edges)
type Edges = [ (Name, Bool, Name) ]
type Nodes = [ (Name, Label) ]

nodesAndEdges :: BDD -> Graph
nodesAndEdges l@(Leaf b) = ( [ (T.pack $ toID l, StrLabel $ T.pack $ show b) ], [] )
nodesAndEdges n@(Node b1 s b2 idd) =
      let sb1 = label b1
          sb2 = label b2
          (nodes', edges') = nodesAndEdges b1
          (nodes'', edges'') = nodesAndEdges b2
          curLabel = T.pack idd
          curNode = (curLabel, StrLabel $ T.pack s)
      in ( curNode : nodes' ++ nodes''
         , (curLabel , True, T.pack $ toID b1) : 
           (curLabel , False, T.pack $ toID b2) : 
           edges' ++ edges'')
  where
    label bdd = case bdd of { Leaf b -> show b; Node _ s _ _ -> s }

toGraph :: String -> Graph -> DotGraph T.Text
toGraph title (nodes, edges) = digraph (Str "BDD") $ do
    graphAttrs [ LabelLoc VTop, Label $ StrLabel $ T.pack title ] 
    mapM_ nodeGen uniqNodes 
    mapM_ edgeGen uniqEdges
  where
    uniqNodes = map head . group . sort $ nodes
    uniqEdges = map head . group . sort $ edges
    edgeGen (n, True, n') = edge n n' [ Style [ SItem Solid [] ] ]
    edgeGen (n, False, n') = edge n n' [ Style [ SItem Dashed [] ] ]
    nodeGen (n,l) 
      | n == "+" = node "+" [ Label $ StrLabel $ T.pack "1", Shape Square ]
      | n == "-" = node "-" [ Label $ StrLabel $ T.pack "0", Shape Square ]
      | otherwise = node n [ Label l ] 

generateGraph :: String -> BDD -> String
generateGraph title = T.unpack . renderDot . toDot . toGraph title . nodesAndEdges
