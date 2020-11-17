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
        parametersQuantity: 5,
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
        parametersQuantity: 6,
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
        parametersQuantity: 1,
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
        parametersQuantity: 1,
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
        parametersQuantity: 1,
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
        parametersQuantity: 1,
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
        parametersQuantity: 1,
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
        parametersQuantity: 1,
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
        parametersQuantity: 1,
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
        parametersQuantity: 2,
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
