# Expression syntax

The primary rule for parsing expressions is given by `expression`. The remainder of the grammar follows from it.

```
expression
  = primary
  , [ binary-op , expression ]
  ;

primary
  = ( value
    | interpolated
    | unary
    | parenthesis
    )
  , [ secondary ]
  ;

secondary
  = ( access | call | index )
  , [ secondary ]
  ;

value = structure | literal | variable ;
```

`interpolated` allows users to write interpolated strings such as `#"Hello, #(name)"`.

```
interpolated
  = '#"'
  , [ { "\#"
      | ( "#(" , expression , ")" )
      | symbol
      }
    ]
  , '"'
  ;
```

```
unary = unary-op , primary ;

parenthesis = "(" , expression , ")" ;
```

`access` allows projecting records, like `foo.bar`, `call` allows calling functions like `foo(12, "bar")`, `index` allows indexing, such as `foo[42]`.

```
access = "." , variable-name ;

call
  = "("
  , [ expression
    , [ { "," , expression } ]
    ]
  , ")"
  ;

index = "[" , expression , "]" ;

variable = variable-name ;

literal = char | float | integer | text ;

char = "'" , symbol , "'" ;

float
  = decimal
  , "."
  , decimal
  , [ exponent ]
  ;
exponent = ("e" | "E") , [ "-" ] , decimal ;
decimal = digit , { digit } ;
```

Integer literals à là Smalltalk. Either `1234` or `10r1234` represent the same thing. A hex literal could be written as `16r00D8FF`, for example.

```
integer
  = [ decimal , ( "r" | "R" ) ]
  , alphanumeric, { alphanumeric }
  ;

text = '"' , [ { symbol } ] , '"' ;

variable-name
  = ( letter | "_" )
  , [ { alphanumeric | "_" } ]
  ;
```

Creating data structures. Algebraic data types such as `Bool::True`, arrays such as `[1, 2, 3]` and records like `{ foo: 12, bar: "bar" }`.

```
structure = algebraic | array | record ;

algebraic
  = variable-name
  , "::"
  , variable-name
  , [ "(" , expression , ")" ]
  ;

array
  = "["
  , [ expression
    , [ { "," , expression } , [ "," ] ]
    ]
  , "]"
  ;

record
  = "{"
  , [ { variable-name , ":" , expression , [ "," ] }
    ]
  , "}"
  ;
```
