export class logicFunction {
    funcName: string;
    commandLine: CommandLine[];
    readonly: boolean;
    events: logicEvent[];
}

export class logicEvent {
    eventName: string;
    commandLine: CommandLine[];
}

export class CommandLine {
    type: string;
    exec: string;
}
