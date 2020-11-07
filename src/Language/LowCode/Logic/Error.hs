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
data Error
    = ConstructorMismatch !Name !Name !Name
    | CyclicImports !(NonEmpty Name)
    | DuplicateModule !Name
    | DuplicateRecord !Name
    | IncompatibleSignatures !Name !Int !Int
    | IncompatibleTypes1 !UnarySymbol !(Expression ())
    | IncompatibleTypes2 !(Expression ()) !BinarySymbol !(Expression ())
    | MainNotFound
    | MissingFields !(NonEmpty Name)
    | NoSuchConstructor !Name !Name
    | NoSuchModule !Name
    | NotAFunction !Name
    | NotAMember !Type !Name
    | NotARecord !Type !Name
    | ShadowedVariable !Name
    | TypeMismatch !Text !Type !Type
    | UndefinedVariable !Name
    | UnknownPattern !MatchPattern !Type
    | UnknownType !(Expression ())
    deriving (Eq, Show)

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

data Warning
    = FloatingPointEquality !Double
    | UnreachableStatement !(AST ())
    | UnusedVariable !Name
    deriving (Eq, Show)

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
