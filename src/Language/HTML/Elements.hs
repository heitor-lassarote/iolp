module Language.HTML.Elements where

import Language.HTML.AST

type Node = Attributes -> [AST] -> AST
type Node' = [AST] -> AST
type Leaf = Attributes -> AST
type Leaf' = AST

node :: Name -> Node
node name attributes asts = Tag name attributes asts

node' :: Name -> Node'
node' name asts = Tag name [] asts

leaf :: Name -> Leaf
leaf name attributes = Tag name attributes []

leaf' :: Name -> Leaf'
leaf' name = Tag name [] []

a :: Node
a = node "a"

a' :: Node'
a' = node' "a"

div :: Node
div = node "div"

div' :: Node'
div' = node' "div"

h1 :: Node
h1 = node "h1"

h1' :: Node'
h1' = node' "h1"

h2 :: Node
h2 = node "h2"

h2' :: Node'
h2' = node' "h2"

h3 :: Node
h3 = node "h3"

h3' :: Node'
h3' = node' "h3"

h4 :: Node
h4 = node "h4"

h4' :: Node'
h4' = node' "h4"

h5 :: Node
h5 = node "h5"

h5' :: Node'
h5' = node' "h5"

h6 :: Node
h6 = node "h6"

h6' :: Node'
h6' = node' "h6"

p :: Node
p = node "p"

p' :: Node'
p' = node' "p"

script :: Node
script = node "script"

script' :: Node'
script' = node' "script"
