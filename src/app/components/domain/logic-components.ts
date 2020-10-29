import { AST } from "./output";

export interface LogicFunction {
    funcName: string;
    commandLine: CommandLine[];
    readonly: boolean;
    events: LogicEvent[];
}

export interface LogicEvent {
    eventName: string;
    eventType: string;
    commandLine: CommandLine[];
}

export interface CommandLine {
    type: CommandLineType;
    exec: AST;
    formIndex: number;
}

export interface CommandLineType {
    name: string;
    clType: string;
}
