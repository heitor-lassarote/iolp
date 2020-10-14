module Language.LowCode.Logic.Standard.JSON
    ( moduleJson
    ) where

import Universum

import Text.RawString.QQ

import Language.LowCode.Logic.Module

unsafeRight :: Either a b -> b
unsafeRight (Left  _) = error "fromRight called with Left."
unsafeRight (Right b) = b

moduleJson :: Module ()
moduleJson = unsafeRight $ parseModule [r|
module JSON;

import Prelude;

type JSON {
    Array([JSON]),
    Boolean(Bool),
    Null,
    Number(Double),
    Object([{key: Text, value: JSON}]),
    String(Text)
}

decodeArray(JSON array) -> {isSuccess: Bool, result: [JSON]} {
    match array {
        JSON::Array(a) { return {isSuccess: Bool::True, result: a}; },
        _ { return {isSuccess: Bool::False, result: []}; }
    }
}

decodeBoolean(JSON bool) -> {isSuccess: Bool, result: Bool} {
    match bool {
        JSON::Boolean(b) { return {isSuccess: Bool::True, result: b}; },
        _ { return {isSuccess: Bool::False, result: Bool::False}; }
    }
}

decodeNull(JSON n) -> {isSuccess: Bool} {
    match n {
        JSON::Null { return {isSuccess: Bool::True}; },
        _ { return {isSuccess: Bool::False}; }
    }
}

decodeNumber(JSON number) -> {isSuccess: Bool, result: Double} {
    match number {
        JSON::Number(n) { return {isSuccess: Bool::True, result: n}; },
        _ { return {isSuccess: Bool::False, result: 0.0}; }
    }
}

decodeObject(JSON object) -> {isSuccess: Bool, result: [{key: Text, value: JSON}]} {
   match object {
       JSON::Object(o) { return {isSuccess: Bool::True, result: o}; },
       _ { return {isSuccess: Bool::False, result: []}; }
   }
}

decodeString(JSON string) -> {isSuccess: Bool, result: Text} {
    match string {
        JSON::String(s) { return {isSuccess: Bool::True, result: s}; },
        _ { return {isSuccess: Bool::False, result: ""}; }
    }
}

type ParseResult {
    Ok(JSON),
    Fail
}

type Parser {
    ParseSuccess({stream: Text, pos: Integer, result: JSON}),
    ParseError
}

extern Text -> Integer length;
extern [JSON] -> [JSON] snoc;
extern Integer -> Double toDouble;

parseJson(Text input) -> ParseResult {
    match parseJsonImpl(input, skipSpace(input, 0)) {
        Parser::ParseSuccess({stream: _, pos: _, result: r}) { return ParseResult::Ok(r); },
        _ { return ParseResult::Fail; }
    }
}

skipSpace(Text input, Integer pos) -> Integer {
    while pos < length(input) {
        if input[pos] = ' ' or input[pos] = '\n' or input[pos] = '\t' {
            pos = pos + 1;
        } else {
            return pos;
        }
    }
    return pos;
}

chunk(Text match, Text input, Integer pos) -> Bool {
    Integer i = 0;
    while i < length(match) and pos < length(input) {
        if match[i] <> input[pos] {
            return Bool::False;
        }

        i = i + 1;
        pos = pos + 1;
    }

    return Bool::True;
}

isDigit(Char c) -> Bool {
    return '0' <= c and c <= '9';
}

parseJsonImpl(Text input, Integer pos) -> Parser {
    Char c = input[0];
    if c = '[' {
        return parseArray(input, pos);
    } else if c = 'b' or c = 't' {
        return parseBoolean(input, pos);
    } else if isDigit(c) {
        return parseNumber(input, pos);
    } else if c = '{' {
        return parseObject(input, pos);
    } else if c = '"' {
        return parseString(input, pos);
    } else if c = 'n' {
        return parseNull(input, pos);
    }
    return Parser::ParseError;
}

parseArray(Text input, Integer pos) -> Parser {
    [JSON] arr = [];
    pos = pos + 1;
    while pos < length(input) and input[pos] <> ']' {
        match parseJsonImpl(input, pos) {
            Parser::ParseSuccess({stream: s, pos: i, result: r}) {
                snoc(arr, r);
                pos = i;
            },
            Parser::ParseError {
                return Parser::ParseError;
            }
        }

        if input[pos] = ',' {
            pos = skipSpace(input, pos + 1);
        }
    }

    if pos < length(input) and input[pos] = ']' {
        pos = skipSpace(input, pos + 1);
        return Parser::ParseSuccess({stream: input, pos: pos, result: JSON::Array(arr)});
    }
    return Parser::ParseError;
}

parseBoolean(Text input, Integer pos) -> Parser {
    if chunk("false", input, pos) {
        pos = skipSpace(input, pos);
        return Parser::ParseSuccess({stream: input, pos: pos + 5, result: JSON::Boolean(Bool::False)});
    } else if chunk("true", input, pos) {
        pos = skipSpace(input, pos);
        return Parser::ParseSuccess({stream: input, pos: pos + 4, result: JSON::Boolean(Bool::True)});
    }
    return Parser::ParseError;
}

parseNull(Text input, Integer pos) -> Parser {
    if chunk("null", input, pos) {
        pos = skipSpace(input, pos);
        return Parser::ParseSuccess({stream: input, pos: pos + 4, result: JSON::Null});
    }
    return Parser::ParseError;
}

digitToInt(Char input) -> Integer {
    match input {
        '0' { return 0; },
        '1' { return 1; },
        '2' { return 2; },
        '3' { return 3; },
        '4' { return 4; },
        '5' { return 5; },
        '6' { return 6; },
        '7' { return 7; },
        '8' { return 8; },
        '9' { return 9; },
    }
}

parseNumber(Text input, Integer pos) -> Parser {
    Double n = 0.0;
    Integer p = 1;
    while pos < length(input) and isDigit(input[pos]) {
        n = n * toDouble(p + digitToInt(input[pos]));
        pos = pos + 1;
        p = p * 10;
    }

    if pos < length(input) and input[pos] = '.' {
        Double d = 1.0;
        pos = pos + 1;
        while pos < length(input) and isDigit(input[pos]) {
            d = d * 10.0;
            n = n + toDouble(digitToInt(input[pos])) / d;
            pos = pos + 1;
        }
    }

    pos = skipSpace(input, pos);
    return Parser::ParseSuccess({stream: input, pos: pos, result: JSON::Number(n)});
}

parseObject(Text input, Integer pos) -> Parser {
    [{key: Text, value: JSON}] arr = [];
    pos = pos + 1;
    while pos < length(input) and input[pos] <> '}' {
        JSON key = JSON::Null;
        match parseString(input, pos) {
            Parser::ParseSuccess({stream: s, pos: i, result: r}) {
                key = r;
                pos = i;
            },
            Parser::ParseError {
                return Parser::ParseError;
            }
        }

        JSON value = JSON::Null;
        if pos >= length(input) or input[pos] <> ':' {
            return Parser::ParseError;
        } else {
            pos = skipSpace(input, pos + 1);
        }

        match parseJsonImpl(input, pos) {
            Parser::ParseSuccess({stream: s, pos: i, result: r}) {
                key = r;
                pos = i;
            },
            Parser::ParseError {
                return Parser::ParseError;
            }
        }

        if input[pos] = ',' {
            pos = skipSpace(input, pos + 1);
        }

        snoc(arr, {key: key, value: value});
    }

    if pos < length(input) and input[pos] = '}' {
        pos = skipSpace(input, pos + 1);
        return Parser::ParseSuccess({stream: input, pos: pos, result: JSON::Object(arr)});
    }
    return Parser::ParseError;
}

parseString(Text input, Integer pos) -> Parser {
    Text t = "";
    pos = pos + 1;
    while pos < length(input) and input[pos] <> '"' {
        t = snoc(t, input[pos]);
    }

    if pos < length(input) and input[pos] = '"' {
        pos = skipSpace(input, pos + 1);
        return Parser::ParseSuccess({stream: input, pos: pos, result: JSON::String(t)});
    }

    return Parser::ParseError;
}|]
