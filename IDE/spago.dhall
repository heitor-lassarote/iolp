{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "arrays"
  , "console"
  , "control"
  , "css"
  , "effect"
  , "free"
  , "halogen"
  , "halogen-css"
  , "lists"
  , "ordered-collections"
  , "profunctor-lenses"
  , "psci-support"
  , "tree"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
