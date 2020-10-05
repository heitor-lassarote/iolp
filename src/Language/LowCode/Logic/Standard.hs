module Language.LowCode.Logic.Standard
    ( boolType
    , false
    , true
    , bool
    , unitType
    , unit'
    , unit
    , moduleJson
    , jsonType
    , json
    , prelude
    ) where

import Universum hiding (Type, bool)

import qualified Data.Map.Strict as Map

import Language.LowCode.Logic.AST
import Language.LowCode.Logic.Module

prelude :: Module Type ()
prelude = Module
    { adtTemplates = Map.unions
        [ bool
        , unit
        ]
    , externs = Map.empty
    , functions = []
    , importedModules = []
    , moduleName = "Prelude"
    }

-- TODO: In the future, move these "concrete" types to a standard library.
boolType :: Type
boolType = AlgebraicType "Bool"

falseConstructor :: Constructor e
falseConstructor = Constructor "False" Nothing

false :: Structure Type
false = Algebraic boolType falseConstructor

trueConstructor :: Constructor e
trueConstructor = Constructor "True" Nothing

true :: Structure Type
true = Algebraic boolType trueConstructor

bool :: Map Name [Constructor Type]
bool = Map.singleton "Bool" [falseConstructor, trueConstructor]

unitType :: Type
unitType = AlgebraicType "Unit"

unitConstructor :: Constructor e
unitConstructor = Constructor "Unit" Nothing

unit' :: Structure Type
unit' = Algebraic unitType unitConstructor

unit :: Map Name [Constructor Type]
unit = Map.singleton "Unit" [unitConstructor]

-- JSON
moduleJson :: Module Type ()
moduleJson = Module
    { adtTemplates = json
    , externs = Map.empty
    , functions =
        [ decodeArray
        , decodeBoolean
        , decodeNull
        , decodeNumber
        , decodeObject
        , decodeString
        ]
    , importedModules = ["Prelude"]
    , moduleName = "JSON"
    }

jsonType :: Type
jsonType = AlgebraicType "JSON"

json :: Map Name [Constructor Type]
json = Map.singleton "JSON"
    [ Constructor "Array"   (Just (ArrayType jsonType))
    , Constructor "Boolean" (Just boolType)
    , Constructor "Null"    Nothing
    , Constructor "Number"  (Just DoubleType)
    , Constructor "Object"  (Just (ArrayType (RecordType [Field "key" TextType, Field "value" jsonType])))
    , Constructor "String"  (Just TextType)
    ]

decodeArray :: Function Type ()
decodeArray = Function () "decodeArray" (FunctionType [jsonType] ret) ["array"]
    $ Match () (Variable jsonType "array")
        [ ( AlgebraicPattern (Constructor "Array" (Just (NamePattern "a")))
          , Return () (Just (Structure ret $ Record ret [Field "isSuccess" (Structure boolType true), Field "result" (Variable arr "a")]))
          )
        ]
    $ Return () (Just (Structure ret (Record ret [Field "isSuccess" (Structure boolType false), Field "result" (Structure arr $ Array jsonType [])])))
  where
    arr = ArrayType jsonType
    ret = RecordType [Field "isSuccess" boolType, Field "result" arr]

decodeBoolean :: Function Type ()
decodeBoolean = Function () "decodeBoolean" (FunctionType [jsonType] ret) ["bool"]
    $ Match () (Variable jsonType "bool")
        [ ( AlgebraicPattern (Constructor "Boolean" (Just (NamePattern "b")))
          , Return () (Just (Structure ret $ Record ret [Field "isSuccess" (Structure boolType true), Field "result" (Variable boolType "b")]))
          )
        ]
    $ Return () (Just (Structure ret (Record ret [Field "isSuccess" (Structure boolType false), Field "result" (Structure boolType false)])))
  where
    ret = RecordType [Field "isSuccess" boolType, Field "result" boolType]

decodeNull :: Function Type ()
decodeNull = Function () "decodeNull" (FunctionType [jsonType] ret) ["null"]
    $ Match () (Variable jsonType "null")
        [ ( AlgebraicPattern (Constructor "Null" Nothing)
          , Return () (Just (Structure ret $ Record ret [Field "isSuccess" (Structure boolType true)]))
          )
        ]
    $ Return () (Just (Structure ret (Record ret [Field "isSuccess" (Structure boolType false)])))
  where
    ret = RecordType [Field "isSuccess" boolType]

decodeNumber :: Function Type ()
decodeNumber = Function () "decodeNumber" (FunctionType [jsonType] ret) ["number"]
    $ Match () (Variable jsonType "number")
        [ ( AlgebraicPattern (Constructor "Number" (Just (NamePattern "n")))
          , Return () (Just (Structure ret $ Record ret [Field "isSuccess" (Structure boolType true), Field "result" (Variable DoubleType "n")]))
          )
        ]
    $ Return () (Just (Structure ret (Record ret [Field "isSuccess" (Structure boolType false), Field "result" (Literal DoubleType (Double (0 / 0)))])))
  where
    ret = RecordType [Field "isSuccess" boolType, Field "result" DoubleType]

decodeObject :: Function Type ()
decodeObject = Function () "decodeObject" (FunctionType [jsonType] ret) ["object"]
    $ Match () (Variable jsonType "object")
        [ ( AlgebraicPattern (Constructor "Object" (Just (NamePattern "o")))
          , Return () (Just (Structure ret $ Record ret [Field "isSuccess" (Structure boolType true), Field "result" (Variable arr "o")]))
          )
        ]
    $ Return () (Just (Structure ret (Record ret [Field "isSuccess" (Structure boolType false), Field "result" (Structure arr (Array arr []))])))
  where
    pairType = RecordType [Field "key" TextType, Field "value" jsonType]
    arr = ArrayType pairType
    ret = RecordType [Field "isSuccess" boolType, Field "result" arr]

decodeString :: Function Type ()
decodeString = Function () "decodeString" (FunctionType [jsonType] ret) ["string"]
    $ Match () (Variable jsonType "object")
        [ ( AlgebraicPattern (Constructor "String" (Just (NamePattern "s")))
          , Return () (Just (Structure ret $ Record ret [Field "isSuccess" (Structure boolType true), Field "result" (Variable TextType "s")]))
          )
        ]
    $ Return () (Just (Structure ret (Record ret [Field "isSuccess" (Structure boolType false), Field "result" (Literal TextType (Text ""))])))
  where
    ret = RecordType [Field "isSuccess" boolType, Field "result" TextType]

--parseJson :: Functor Type ()
--parseJson = Function () "parseJSON" (FunctionType [TextType] ret) ["input"]
--    $ 
--  where
--    ret = RecordType [Field "isSuccess" boolType, Field "result" TextType]
--
--    space = Function () "space" [FunctionType [TextType] TextType) ["input"]
--        $ While () (Index (Variable "input"))
