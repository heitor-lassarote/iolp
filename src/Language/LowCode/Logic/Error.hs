module Language.LowCode.Logic.Error
    ( Error (..)
    , prettyError
    , Warning (..)
    , prettyWarning
    ) where

import Universum hiding (Type)

import Formatting (float, int, sformat, shown, stext, (%))

import Language.Codegen (unsafeCodegen')
import Language.LowCode.Logic.AST

-- TODO: Add information about where the error is coming from.
-- TODO: Should IncompatibleTypes and TypeMismatch really have these signatures?
data Error
    = CyclicImports !(NonEmpty Name)
    | DuplicateModule !Name
    | DuplicateRecord !Name
    | IncompatibleSignatures !Name !Int !Int
    | IncompatibleTypes1 !UnarySymbol !(Expression ())
    | IncompatibleTypes2 !(Expression ()) !BinarySymbol !(Expression ())
    | MissingFields (NonEmpty Name)
    | NoSuchConstructor !Name
    | NoSuchModule !Name
    | NotAFunction !Name
    | NotAMember !Type !Name
    | NotARecord !Type !Name
    | ShadowedVariable !Name
    | TypeMismatch !Text !Type !Type
    | UndefinedVariable !Name
    | UnknownArray
    deriving (Eq, Show)

prettyCyclic :: NonEmpty Name -> Text
prettyCyclic (f :| c) = sformat ("'" % stext % "' imports '" % stext) f (go c)
  where
    go (x : xs) = sformat ("'" % stext % "', which imports\n" % stext) x (go xs)
    go []       = sformat ("'" % stext % "'.") f

prettyError :: Error -> Text
prettyError = \case
    CyclicImports cycles -> prettyCyclic cycles
    DuplicateModule name -> sformat ("Duplicate module '" % stext % "'.") name
    DuplicateRecord fieldName -> sformat
        ("Duplicate field '" % stext % "' on record.")
        fieldName
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
    MissingFields (name :| names) -> sformat
        ("Record does not have the required fields: " % shown % ".")
        (name : names)
    NoSuchConstructor name -> sformat
        ("Could not find ADT containing a constuctor called '" % stext % "'.")
        name
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
        -- TODO: Scan for similarly named variables.
        ("'" % stext % "' was used but it was not defined. Perhaps you forgot to declare it or made a typo?")
        name
    UnknownArray -> "Could not deduce type for array."

data Warning
    = FloatingPointEquality !Double
    | UnusedVariable !Name
    deriving (Eq, Show)

prettyWarning :: Warning -> Text
prettyWarning = \case
    FloatingPointEquality value -> sformat
        ("Floating point is imprecise and equality may fail. Test for delta " % float % " - ε < x < " % float % " + ε instead.")
        value
        value
    UnusedVariable name -> sformat
        ("Declared but not used: '" % stext % "'.")
        name
