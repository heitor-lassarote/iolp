"use strict";
const JSON$Array = (value) => ({$: "JSON$Array", value: value});
const JSON$Boolean = (value) => ({$: "JSON$Boolean", value: value});
const JSON$Null = {$: "JSON$Null"};
const JSON$Number = (value) => ({$: "JSON$Number", value: value});
const JSON$Object = (value) => ({$: "JSON$Object", value: value});
const JSON$String = (value) => ({$: "JSON$String", value: value});
const ParseResult$Ok = (value) => ({$: "ParseResult$Ok", value: value});
const ParseResult$Fail = (value) => ({$: "ParseResult$Fail", value: value});
const Parser$Ok = (value) => ({$: "Parser$Ok", value: value});
const Parser$Fail = (value) => ({$: "Parser$Fail", value: value});
function decodeArray(array) {
    const $temp0 = array;
    if ($temp0.$ === "JSON$Array") {
        const a = $temp0.value;
        return {isSuccess: true, result: a};
    }
    else {
        return {isSuccess: false, result: []};
    }
}

function decodeBoolean(bool) {
    const $temp1 = bool;
    if ($temp1.$ === "JSON$Boolean") {
        const b = $temp1.value;
        return {isSuccess: true, result: b};
    }
    else {
        return {isSuccess: false, result: false};
    }
}

function decodeNull(n) {
    const $temp2 = n;
    if ($temp2.$ === "JSON$Null") {
        return {isSuccess: true};
    }
    else {
        return {isSuccess: false};
    }
}

function decodeNumber(number) {
    const $temp3 = number;
    if ($temp3.$ === "JSON$Number") {
        const n = $temp3.value;
        return {isSuccess: true, result: n};
    }
    else {
        return {isSuccess: false, result: 0.0};
    }
}

function decodeObject(object) {
    const $temp4 = object;
    if ($temp4.$ === "JSON$Object") {
        const o = $temp4.value;
        return {isSuccess: true, result: o};
    }
    else {
        return {isSuccess: false, result: []};
    }
}

function decodeString(string) {
    const $temp5 = string;
    if ($temp5.$ === "JSON$String") {
        const s = $temp5.value;
        return {isSuccess: true, result: s};
    }
    else {
        return {isSuccess: false, result: ""};
    }
}

function parseJson(input) {
    const $temp6 = parseJsonImpl(input, skipSpace(input, 0));
    if ($temp6.$ === "Parser$Ok") {
        const r = $temp6.value.result;
        return ParseResult$Ok(r);
    }
    else if ($temp6.$ === "Parser$Fail") {
        const p = $temp6.value.pos;
        const f = $temp6.value.reason;
        return ParseResult$Fail("Error on character " + integerToText(p) + ": " + f);
    }
    else {
        throw new Error("Non-exhaustive pattern matches.");
    }
}

function skipSpace(input, pos) {
    while (pos < input.length) {
        if (input[pos] === " " || input[pos] === "\n" || input[pos] === "\t") {
            pos = pos + 1;
        }
        else {
            return pos;
        }
    }
    return pos;
}

function chunk(match, input, pos) {
    let i = 0;
    while (i < match.length && pos < input.length) {
        if (match[i] !== input[pos]) {
            return false;
        }
        i = i + 1;
        pos = pos + 1;
    }
    return true;
}

function isDigit(c) {
    return "0" <= c && c <= "9";
}

function parseJsonImpl(input, pos) {
    let c = input[pos];
    if (c === "[") {
        return parseArray(input, pos);
    }
    else if (c === "f" || c === "t") {
        return parseBoolean(input, pos);
    }
    else if (isDigit(c)) {
        return parseNumber(input, pos);
    }
    else if (c === "{") {
        return parseObject(input, pos);
    }
    else if (c === "\"") {
        return parseString(input, pos);
    }
    else if (c === "n") {
        return parseNull(input, pos);
    }
    else {
        return Parser$Fail({pos: pos, reason: "Not a valid JSON primitive."});
    }
}

function parseArray(input, pos) {
    let arr = [];
    pos = pos + 1;
    while (pos < input.length && input[pos] !== "]") {
        const $temp7 = parseJsonImpl(input, pos);
        if ($temp7.$ === "Parser$Ok") {
            const i = $temp7.value.pos;
            const r = $temp7.value.result;
            arr.push(r);
            pos = i;
        }
        else if ($temp7.$ === "Parser$Fail") {
            const f = $temp7.value;
            return Parser$Fail(f);
        }
        else {
            throw new Error("Non-exhaustive pattern matches.");
        }
        if (input[pos] === ",") {
            pos = skipSpace(input, pos + 1);
        }
    }
    if (pos >= input.length) {
        return Parser$Fail({pos: pos, reason: "Unexpected end of input. Expected ']'."});
    }
    else if (input[pos] === "]") {
        pos = skipSpace(input, pos + 1);
        return Parser$Ok({pos: pos, result: JSON$Array(arr), stream: input});
    }
    else {
        return Parser$Fail({pos: pos, reason: "Expected ']'."});
    }
}

function parseBoolean(input, pos) {
    if (chunk("false", input, pos)) {
        pos = skipSpace(input, pos);
        return Parser$Ok({pos: pos + 5, result: JSON$Boolean(false), stream: input});
    }
    else if (chunk("true", input, pos)) {
        pos = skipSpace(input, pos);
        return Parser$Ok({pos: pos + 4, result: JSON$Boolean(true), stream: input});
    }
    else {
        return Parser$Fail({pos: pos, reason: "Expected 'true' or 'false'."});
    }
}

function parseNull(input, pos) {
    if (chunk("null", input, pos)) {
        pos = skipSpace(input, pos);
        return Parser$Ok({pos: pos + 4, result: JSON$Null, stream: input});
    }
    else {
        return Parser$Fail({pos: pos, reason: "Expected 'null'."});
    }
}

function digitToInt(input) {
    const $temp8 = input;
    if ($temp8 === "0") {
        return 0;
    }
    else if ($temp8 === "1") {
        return 1;
    }
    else if ($temp8 === "2") {
        return 2;
    }
    else if ($temp8 === "3") {
        return 3;
    }
    else if ($temp8 === "4") {
        return 4;
    }
    else if ($temp8 === "5") {
        return 5;
    }
    else if ($temp8 === "6") {
        return 6;
    }
    else if ($temp8 === "7") {
        return 7;
    }
    else if ($temp8 === "8") {
        return 8;
    }
    else if ($temp8 === "9") {
        return 9;
    }
    else {
        throw new Error("Non-exhaustive pattern matches.");
    }
}

function parseNumber(input, pos) {
    let n = 0.0;
    let p = 1;
    let isNegative = input[pos] === "-";
    if (isNegative) {
        p = -1;
    }
    while (pos < input.length && isDigit(input[pos])) {
        n = n * integerToDouble(p) + integerToDouble(digitToInt(input[pos]));
        pos = pos + 1;
        p = p * 10;
    }
    if (pos < input.length && input[pos] === ".") {
        let d = 1.0;
        pos = pos + 1;
        while (pos < input.length && isDigit(input[pos])) {
            d = d * 10.0;
            n = n + integerToDouble(digitToInt(input[pos])) / d;
            pos = pos + 1;
        }
    }
    pos = skipSpace(input, pos);
    return Parser$Ok({pos: pos, result: JSON$Number(n), stream: input});
}

function parseObject(input, pos) {
    let arr = [];
    pos = pos + 1;
    while (pos < input.length && input[pos] !== "}") {
        let key = "";
        const $temp9 = parseString(input, pos);
        if ($temp9.$ === "Parser$Ok" && $temp9.value.result.$ === "JSON$String") {
            const s = $temp9.value.stream;
            const i = $temp9.value.pos;
            const r = $temp9.value.result.value;
            key = r;
            pos = i;
        }
        else if ($temp9.$ === "Parser$Fail") {
            const f = $temp9.value;
            return Parser$Fail(f);
        }
        else {
            throw new Error("Non-exhaustive pattern matches.");
        }
        let value = JSON$Null;
        if (pos >= input.length) {
            return Parser$Fail({pos: pos, reason: "Unexpected end of input."});
        }
        else if (input[pos] !== ":") {
            return Parser$Fail({pos: pos, reason: "Expected ':'."});
        }
        else {
            pos = skipSpace(input, pos + 1);
        }
        const $temp10 = parseJsonImpl(input, pos);
        if ($temp10.$ === "Parser$Ok") {
            const s = $temp10.value.stream;
            const i = $temp10.value.pos;
            const r = $temp10.value.result;
            value = r;
            pos = i;
        }
        else if ($temp10.$ === "Parser$Fail") {
            const f = $temp10.value;
            return Parser$Fail(f);
        }
        else {
            throw new Error("Non-exhaustive pattern matches.");
        }
        if (pos < input.length && input[pos] === ",") {
            pos = skipSpace(input, pos + 1);
        }
        arr.push({key: key, value: value});
    }
    if (pos >= input.length) {
        return Parser$Fail({pos: pos, reason: "Unexpected end of input."});
    }
    else if (input[pos] === "}") {
        pos = skipSpace(input, pos + 1);
        return Parser$Ok({pos: pos, result: JSON$Object(arr), stream: input});
    }
    else {
        return Parser$Fail({pos: pos, reason: "Expected '}'."});
    }
}

function parseString(input, pos) {
    if (pos >= input.length) {
        return Parser$Fail({pos: pos, reason: "Unexpected end of input."});
    }
    else if (input[pos] !== "\"") {
        return Parser$Fail({pos: pos, reason: "Expected '\"'."});
    }
    let t = "";
    pos = pos + 1;
    while (pos < input.length && input[pos] !== "\"") {
        t = t + input[pos];
        pos = pos + 1;
    }
    if (pos >= input.length) {
        return Parser$Fail({pos: pos, reason: "Unexpected end of input."});
    }
    else if (input[pos] === "\"") {
        pos = skipSpace(input, pos + 1);
        return Parser$Ok({pos: pos, result: JSON$String(t), stream: input});
    }
    else {
        return Parser$Fail({pos: pos, reason: "Expected '\"'."});
    }
}

function encodeJson(input) {
    const $temp11 = input;
    if ($temp11.$ === "JSON$Array") {
        const a = $temp11.value;
        return encodeArray(a);
    }
    else if ($temp11.$ === "JSON$Boolean") {
        const b = $temp11.value;
        return encodeBoolean(b);
    }
    else if ($temp11.$ === "JSON$Null") {
        return encodeNull();
    }
    else if ($temp11.$ === "JSON$Number") {
        const n = $temp11.value;
        return encodeNumber(n);
    }
    else if ($temp11.$ === "JSON$Object") {
        const o = $temp11.value;
        return encodeObject(o);
    }
    else if ($temp11.$ === "JSON$String") {
        const s = $temp11.value;
        return encodeString(s);
    }
    else {
        throw new Error("Non-exhaustive pattern matches.");
    }
}

function encodeArray(input) {
    if (input.length === 0) {
        return "[]";
    }
    let t = "[" + encodeJson(input[0]);
    let i = 1;
    while (i < input.length) {
        t = t + "," + encodeJson(input[i]);
        i = i + 1;
    }
    return t + "]";
}

function encodeBoolean(input) {
    if (input) {
        return "true";
    }
    else {
        return "false";
    }
}

function encodeNull() {
    return "null";
}

function encodeNumber(input) {
    return doubleToText(input);
}

function encodeObject(input) {
    if (input.length === 0) {
        return "{}";
    }
    let t = "{" + encodeField(input[0]);
    let i = 1;
    while (i < input.length) {
        t = t + "," + encodeField(input[i]);
        i = i + 1;
    }
    return t + "}";
}

function encodeField(input) {
    return encodeString(input.key) + ":" + encodeJson(input.value);
}

function encodeString(input) {
    return "\"" + input + "\"";
}

