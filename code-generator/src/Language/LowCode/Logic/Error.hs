{-# LANGUAGE DataKinds #-}
module Language.LowCode.Logic.Error
    ( Error (..)
    , prettyError
    , Warning (..)
    , prettyWarning
    ) where

import Universum hiding (Type)

import Formatting (float, int, sformat, shown, stext, (%))

import Language.LowCode.Logic.AST
import Language.LowCode.Logic.Codegen
import Language.LowCode.Logic.Type (Type)

-- TODO: Add information about where the error is coming from.
-- TODO: Should IncompatibleTypes and TypeMismatch really have these signatures?
data Error where
    ConstructorMismatch :: !Name -> !Name -> !Name -> Error
    CyclicImports :: !(NonEmpty Name) -> Error
    DuplicateModule :: !Name -> Error
    DuplicateRecord :: !Name -> Error
    IncompatibleSignatures :: !Name -> !Int -> !Int -> Error
    IncompatibleTypes1 :: (Show e) => !UnarySymbol -> !(Expression e) -> Error
    IncompatibleTypes2 :: Each '[Show] [l, r] => !(Expression l) -> !BinarySymbol -> !(Expression r) -> Error
    InvalidAssignment :: Each '[Show] [l, r] => !(Expression l) -> !(Expression r) -> Error
    MainNotFound :: Error
    MissingFields :: !(NonEmpty Name) -> Error
    NoSuchConstructor :: !Name -> !Name -> Error
    NoSuchModule :: !Name -> Error
    NotAFunction :: !Name -> Error
    NotAMember :: !Type -> !Name -> Error
    NotARecord :: !Type -> !Name -> Error
    ShadowedVariable :: !Name -> Error
    TypeMismatch :: !Text -> !Type -> !Type -> Error
    UndefinedVariable :: !Name -> Error
    UnknownPattern :: !MatchPattern -> !Type -> Error
    UnknownType :: (Show e) => !(Expression e) -> Error

deriving stock instance Show Error

prettyCyclic :: NonEmpty Name -> Text
prettyCyclic (f :| []) = sformat ("'" % stext % "' imports itself.") f
prettyCyclic (f :| cs) = sformat ("'" % stext % "' imports '" % stext) f (go cs)
  where
    go (x : xs) = sformat ("'" % stext % "', which imports\n" % stext) x (go xs)
    go []       = sformat ("'" % stext % "'.") f

prettyAdt :: Name -> Name -> Text
prettyAdt adtName adtConstructor = sformat ("'" % stext % "::" % stext % "'")
    adtName
    adtConstructor

prettyError :: Error -> Text
prettyError = \case
    ConstructorMismatch adtName expected actual -> sformat
        ("Invalid constructor '" % stext % "'. The only constructor for the given ADT is '" % stext % "'.")
        (prettyAdt adtName actual)
        (prettyAdt adtName expected)
    CyclicImports cycles -> prettyCyclic cycles
    DuplicateModule name -> sformat ("Duplicate module '" % stext % "'.") name
    DuplicateRecord fieldName -> sformat
        ("Duplicate field '" % stext % "' on record.")
        fieldName
    -- FIXME: singular/plural (should apply to some other errors here too)
    IncompatibleSignatures name args1 args2 -> sformat
        ("'" % stext % "' expects " % int % " arguments, but " % int % " were given.")
        name
        args1
        args2
    IncompatibleTypes1 symbol expr -> sformat
        ("Incompatible expression '" % stext % "' for unary operator '" % stext % "'.")
        (unsafeCodegen' expr)
        (unaryToText symbol)
    IncompatibleTypes2 left symbol right -> sformat
        ("Incompatible expressions '" % stext % "' and '" % stext % "' for binary operator '" % stext % "'.")
        (unsafeCodegen' left)
        (unsafeCodegen' right)
        (binaryToText symbol)
    InvalidAssignment left right -> sformat
        ("Cannot assign expression '" % stext % "' to '" % stext % "'. Only access, index and variable are assignable.")
        (unsafeCodegen' right)
        (unsafeCodegen' left)
    MainNotFound -> "No function with name 'main' and type () -> Unit could be found."
    MissingFields (name :| names) -> sformat
        ("Record does not have the required fields: " % shown % ".")
        (name : names)
    NoSuchConstructor adtName cName -> sformat
        ("Could not find ADT containing a constuctor called " % stext % ".")
        (prettyAdt adtName cName)
    NoSuchModule name -> sformat ("Could not find module '" % stext % "'.") name
    -- TODO: Add types/expressions being applied? And maybe the inferred type?
    NotAFunction name -> sformat ("Can't use '" % stext % "' as function.") name
    NotAMember record memberName -> sformat
        ("'" % stext % "' is not a member of the record '" % shown % "'.")
        memberName
        record
    NotARecord type' memberName -> sformat
        ("Can't access '" % stext % "' on non-record type '" % shown % "'.")
        memberName
        type'
    ShadowedVariable name -> sformat
        -- TODO: Add where it was defined and what is the new value being assigned. Maybe old value too.
        ("'" % stext % "' was shadowed.")
        name
    TypeMismatch name expected actual -> sformat
        ("Type mismatch in '" % stext % "'. Expected '" % shown % "' but got '" % shown % "'.")
        name
        expected
        actual
    UndefinedVariable name -> sformat
        -- TODO (optional): Scan for similarly named variables.
        ("'" % stext % "' was used but it was not defined. Perhaps you forgot to declare it or made a typo?")
        name
    UnknownPattern pattern expectedType -> sformat
        ("Could not deduce type for pattern expression: '" % stext % "'." % shown)
        (unsafeCodegen' pattern)
        expectedType
    UnknownType expr -> sformat
        ("Could not deduce type for '" % stext % "'.")
        (unsafeCodegen' expr)

data Warning where
    FloatingPointEquality :: !Double -> Warning
    UnreachableStatement :: (Show a) => !(AST a) -> Warning
    UnusedVariable :: !Name -> Warning

deriving instance Show Warning

prettyWarning :: Warning -> Text
prettyWarning = \case
    FloatingPointEquality value -> sformat
        ("Floating point is imprecise and equality may fail. Test for delta " % float % " -ε < x < " % float % " + ε instead.")
        value
        value
    UnreachableStatement ast -> sformat
        ("Unreachable statement: " % stext)
        (unsafeCodegen' ast)
    UnusedVariable name -> sformat
        ("Declared but not used: '" % stext % "'.")
        name
