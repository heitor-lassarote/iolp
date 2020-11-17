import { ExternFunction } from "../domain/logic-components";
import {
    AlgebraicType,
    DoubleType,
    FunctionType,
    IntegerType,
    TextType,
} from "../domain/output";

const UNIT = new AlgebraicType("Unit");

export const LOWCODEFUNCTIONS: ExternFunction[] = [
    {
        name: "GET",
        parameters: [
            { paramName: "url", paramValue: "", paramType: new TextType() },
            {
                paramName: "before",
                paramValue: "",
                paramType: new FunctionType([], UNIT),
            },
            {
                paramName: "success",
                paramValue: "",
                paramType: new FunctionType([], UNIT),
            },
            {
                paramName: "error",
                paramValue: "",
                paramType: new FunctionType([], UNIT),
            },
            {
                paramName: "complete",
                paramValue: "",
                paramType: new FunctionType([], UNIT),
            },
        ],
        returnType: UNIT,
    },
    {
        name: "POST",
        parameters: [
            { paramName: "url", paramValue: "", paramType: new TextType() },
            { paramName: "value", paramValue: "", paramType: new TextType() },
            {
                paramName: "before",
                paramValue: "",
                paramType: new FunctionType([], UNIT),
            },
            {
                paramName: "success",
                paramValue: "",
                paramType: new FunctionType([], UNIT),
            },
            {
                paramName: "error",
                paramValue: "",
                paramType: new FunctionType([], UNIT),
            },
            {
                paramName: "complete",
                paramValue: "",
                paramType: new FunctionType([], UNIT),
            },
        ],
        returnType: UNIT,
    },
    {
        name: "trunc",
        parameters: [
            {
                paramName: "truncValue",
                paramValue: "",
                paramType: new DoubleType(),
            },
        ],
        returnType: "integer",
    },
    {
        name: "doubleToInteger",
        parameters: [
            {
                paramName: "doubleValue",
                paramValue: "",
                paramType: new DoubleType(),
            },
        ],
        returnType: "integer",
    },
    {
        name: "doubleToText",
        parameters: [
            {
                paramName: "doubleValue",
                paramValue: "",
                paramType: new DoubleType(),
            },
        ],
        returnType: "text",
    },
    {
        name: "integerToDouble",
        parameters: [
            {
                paramName: "integerValue",
                paramValue: "",
                paramType: new IntegerType(),
            },
        ],
        returnType: "double",
    },
    {
        name: "integerToText",
        parameters: [
            {
                paramName: "integerValue",
                paramValue: "",
                paramType: new IntegerType(),
            },
        ],
        returnType: "text",
    },
    {
        name: "textToDouble",
        parameters: [
            {
                paramName: "textValue",
                paramValue: "",
                paramType: new TextType(),
            },
        ],
        returnType: "double",
    },
    {
        name: "textToInteger",
        parameters: [
            {
                paramName: "textValue",
                paramValue: "",
                paramType: new TextType(),
            },
        ],
        returnType: "integer",
    },
    {
        name: "textToIntegerRadix",
        parameters: [
            {
                paramName: "textValue",
                paramValue: "",
                paramType: new TextType(),
            },
            {
                paramName: "radix",
                paramValue: "",
                paramType: new IntegerType(),
            },
        ],
        returnType: "integer",
    },
];
