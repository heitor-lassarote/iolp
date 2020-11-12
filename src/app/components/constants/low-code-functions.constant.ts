import { ExternFunction } from "../domain/logic-components";
import {
    AlgebraicType,
    DoubleType,
    FunctionType,
    IntegerType,
    TextType,
} from "../domain/output";

const UNIT = {
    tag: "adt",
    name: "Unit",
} as AlgebraicType;

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
    },
];
