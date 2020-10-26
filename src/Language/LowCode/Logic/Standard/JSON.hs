module Language.LowCode.Logic.Standard.JSON
    ( moduleJson
    ) where

import Universum

import Text.RawString.QQ

import Language.LowCode.Logic.Module
import Utility (unsafeRight)

moduleJson :: Module ()
moduleJson = unsafeRight $ parseModule moduleJsonDef
--moduleJson :: Either Text (Module ())
--moduleJson = parseModule moduleJsonDef

moduleJsonDef :: Text
moduleJsonDef = [r|
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

// Decode
decodeArray(JSON array) -> {isSuccess: Bool, result: [JSON]} {
    match array {
        JSON::Array(a) => {isSuccess: Bool::True, result: a},
        ? => {isSuccess: Bool::False, result: []}
    }
}

decodeBoolean(JSON bool) -> {isSuccess: Bool, result: Bool} {
    match bool {
        JSON::Boolean(b) => {isSuccess: Bool::True, result: b},
        ? => {isSuccess: Bool::False, result: Bool::False}
    }
}

decodeNull(JSON n) -> {isSuccess: Bool} {
    match n {
        JSON::Null => {isSuccess: Bool::True},
        ? => {isSuccess: Bool::False}
    }
}

decodeNumber(JSON number) -> {isSuccess: Bool, result: Double} {
    match number {
        JSON::Number(n) => {isSuccess: Bool::True, result: n},
        ? => {isSuccess: Bool::False, result: 0.0}
    }
}

decodeObject(JSON object) -> {isSuccess: Bool, result: [{key: Text, value: JSON}]} {
   match object {
       JSON::Object(o) => {isSuccess: Bool::True, result: o},
       ? => {isSuccess: Bool::False, result: []}
   }
}

decodeString(JSON string) -> {isSuccess: Bool, result: Text} {
    match string {
        JSON::String(s) => {isSuccess: Bool::True, result: s},
        ? => {isSuccess: Bool::False, result: ""}
    }
}

// Parse
type ParseResult {
    Ok(JSON),
    Fail(Text),
}

type Parser {
    Ok({stream: Text, pos: Integer, result: JSON}),
    Fail({pos: Integer, reason: Text}),
}

parseJson(Text input) -> ParseResult {
    match parseJsonImpl(input, skipSpace(input, 0)) {
        Parser::Ok({stream: ?, pos: ?, result: r}) => ParseResult::Ok(r),
        Parser::Fail({pos: p, reason: f}) => ParseResult::Fail("Error on character " + integerToText(p) + ": " + f)
    }
}

skipSpace(Text input, Integer pos) -> Integer {
    while pos < input.length {
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
    while i < match.length and pos < input.length {
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
    Char c = input[pos];
    if c = '[' {
        return parseArray(input, pos);
    } else if c = 'f' or c = 't' {
        return parseBoolean(input, pos);
    } else if isDigit(c) {
        return parseNumber(input, pos);
    } else if c = '{' {
        return parseObject(input, pos);
    } else if c = '"' {
        return parseString(input, pos);
    } else if c = 'n' {
        return parseNull(input, pos);
    } else {
        return Parser::Fail({pos: pos, reason: "Not a valid JSON primitive."});
    }
}

parseArray(Text input, Integer pos) -> Parser {
    [JSON] arr = [];
    pos = pos + 1;
    while pos < input.length and input[pos] <> ']' {
        match parseJsonImpl(input, pos) {
            Parser::Ok({stream: ?, pos: i, result: r}) {
                arr.push(r);
                pos = i;
            },
            Parser::Fail(f) => Parser::Fail(f)
        }

        if input[pos] = ',' {
            pos = skipSpace(input, pos + 1);
        }
    }

    if pos >= input.length {
        return Parser::Fail({pos: pos, reason: "Unexpected end of input. Expected ']'."});
    } else if input[pos] = ']' {
        pos = skipSpace(input, pos + 1);
        return Parser::Ok({stream: input, pos: pos, result: JSON::Array(arr)});
    } else {
        return Parser::Fail({pos: pos, reason: "Expected ']'."});
    }
}

parseBoolean(Text input, Integer pos) -> Parser {
    if chunk("false", input, pos) {
        pos = skipSpace(input, pos);
        return Parser::Ok({stream: input, pos: pos + 5, result: JSON::Boolean(Bool::False)});
    } else if chunk("true", input, pos) {
        pos = skipSpace(input, pos);
        return Parser::Ok({stream: input, pos: pos + 4, result: JSON::Boolean(Bool::True)});
    } else {
        return Parser::Fail({pos: pos, reason: "Expected 'true' or 'false'."});
    }
}

parseNull(Text input, Integer pos) -> Parser {
    if chunk("null", input, pos) {
        pos = skipSpace(input, pos);
        return Parser::Ok({stream: input, pos: pos + 4, result: JSON::Null});
    } else {
        return Parser::Fail({pos: pos, reason: "Expected 'null'."});
    }
}

digitToInt(Char input) -> Integer {
    match input {
        '0' => 0,
        '1' => 1,
        '2' => 2,
        '3' => 3,
        '4' => 4,
        '5' => 5,
        '6' => 6,
        '7' => 7,
        '8' => 8,
        '9' => 9,
    }
}

parseNumber(Text input, Integer pos) -> Parser {
    Double n = 0.0;
    Integer p = 1;
    Bool isNegative = input[pos] = '-';
    if isNegative {
        p = -1;
    }

    while pos < input.length and isDigit(input[pos]) {
        n = n * integerToDouble(p) + integerToDouble(digitToInt(input[pos]));
        pos = pos + 1;
        p = p * 10;
    }

    if pos < input.length and input[pos] = '.' {
        Double d = 1.0;
        pos = pos + 1;
        while pos < input.length and isDigit(input[pos]) {
            d = d * 10.0;
            n = n + integerToDouble(digitToInt(input[pos])) / d;
            pos = pos + 1;
        }
    }

    pos = skipSpace(input, pos);
    return Parser::Ok({stream: input, pos: pos, result: JSON::Number(n)});
}

parseObject(Text input, Integer pos) -> Parser {
    [{key: Text, value: JSON}] arr = [];
    pos = pos + 1;
    while pos < input.length and input[pos] <> '}' {
        Text key = "";
        match parseString(input, pos) {
            Parser::Ok({stream: s, pos: i, result: JSON::String(r)}) {
                key = r;
                pos = i;
            },
            Parser::Fail(f) => Parser::Fail(f)
        }

        JSON value = JSON::Null;
        if pos >= input.length {
            return Parser::Fail({pos: pos, reason: "Unexpected end of input."});
        } else if input[pos] <> ':' {
            return Parser::Fail({pos: pos, reason: "Expected ':'."});
        } else {
            pos = skipSpace(input, pos + 1);
        }

        match parseJsonImpl(input, pos) {
            Parser::Ok({stream: s, pos: i, result: r}) {
                value = r;
                pos = i;
            },
            Parser::Fail(f) => Parser::Fail(f)
        }

        if pos < input.length and input[pos] = ',' {
            pos = skipSpace(input, pos + 1);
        }

        arr.push({key: key, value: value});
    }

    if pos >= input.length {
        return Parser::Fail({pos: pos, reason: "Unexpected end of input."});
    } else if input[pos] = '}' {
        pos = skipSpace(input, pos + 1);
        return Parser::Ok({stream: input, pos: pos, result: JSON::Object(arr)});
    } else {
        return Parser::Fail({pos: pos, reason: "Expected '}'."});
    }
}

parseString(Text input, Integer pos) -> Parser {
    if pos >= input.length {
        return Parser::Fail({pos: pos, reason: "Unexpected end of input."});
    } else if input[pos] <> '"' {
        return Parser::Fail({pos: pos, reason: "Expected '\"'."});
    }

    Text t = "";
    pos = pos + 1;
    while pos < input.length and input[pos] <> '"' {
        t = t.push(input[pos]);
        pos = pos + 1;
    }

    if pos >= input.length {
        return Parser::Fail({pos: pos, reason: "Unexpected end of input."});
    } else if input[pos] = '"' {
        pos = skipSpace(input, pos + 1);
        return Parser::Ok({stream: input, pos: pos, result: JSON::String(t)});
    } else {
        return Parser::Fail({pos: pos, reason: "Expected '\"'."});
    }
}

// Encode
encodeJson(JSON input) -> Text {
    match input {
        JSON::Array(a)   => encodeArray(a),
        JSON::Boolean(b) => encodeBoolean(b),
        JSON::Null       => encodeNull(),
        JSON::Number(n)  => encodeNumber(n),
        JSON::Object(o)  => encodeObject(o),
        JSON::String(s)  => encodeString(s),
    }
}

encodeArray([JSON] input) -> Text {
    if input.length = 0 {
        return "[]";
    }

    Text t = "[" + encodeJson(input[0]);
    Integer i = 1;
    while i < input.length {
        t = t + "," + encodeJson(input[i]);
        i = i + 1;
    }

    return t + "]";
}

encodeBoolean(Bool input) -> Text {
    if input {
        return "true";
    } else {
        return "false";
    }
}

encodeNull() -> Text {
    return "null";
}

encodeNumber(Double input) -> Text {
    return doubleToText(input);
}

encodeObject([{key: Text, value: JSON}] input) -> Text {
    if input.length = 0 {
        return "{}";
    }

    Text t = "{" + encodeField(input[0]);
    Integer i = 1;
    while i < input.length {
        t = t + "," + encodeField(input[i]);
        i = i + 1;
    }

    return t + "}";
}

encodeField({key: Text, value: JSON} input) -> Text {
    return encodeString(input.key) + ":" + encodeJson(input.value);
}

encodeString(Text input) -> Text {
    return "\"" + input + "\"";
}|]
