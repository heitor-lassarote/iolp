import { FormGroup } from "@angular/forms";
import { Type } from "./output";

export interface LogicFunction {
    funcName: string;
    commandLine: CommandLine[];
    readonly: boolean;
    events: LogicEvent[];
    arguments: Argument[];
    returnType: Type;
}

export interface LogicEvent {
    eventName: string;
    eventType: string;
    commandLine: CommandLine[];
}

export interface CommandLine {
    type: CommandLineType;
    exec: FormGroup;
    formIndex: number;
}

export interface CommandLineType {
    name: string;
    clType: string;
}

export interface Argument {
    returnType: string;
    name: string;
}

export interface ExternFunction {
    name: string;
    parameters: { paramName: string; paramValue: any; paramType: Type }[];
    returnType: string | Type;
}
