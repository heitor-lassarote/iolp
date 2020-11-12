import {
    Output,
    Page,
    HtmlTagOut,
    CssOut,
    HtmlOut,
    HtmlTextOut,
    Module,
    Function,
    FunctionType,
    If,
    AST,
    While,
    Var,
    Expression,
    BinaryOp,
    Literal,
    Literal_,
    Char,
    Double,
    Integer,
    Text,
    Type,
    AlgebraicType,
    ArrayType,
    CharType,
    DoubleType,
    IntegerType,
    RecordType,
    TextType,
    Call,
    Variable,
} from "./../../domain/output";
import { Info } from "./../../domain/info";
import { Element } from "src/app/components/domain/element";
import { Component, OnInit } from "@angular/core";
import { ResizeEvent } from "angular-resizable-element";
import { NgxSpinnerService } from "ngx-spinner";
import { SpawnComponentService } from "src/app/services/spawn/spawn-component.service";
import { ShowComponentInfoService } from "src/app/services/show-component-info/show-component-info.service";
import { SendService } from "src/app/services/send/send.service";
import { HttpErrorResponse } from "@angular/common/http";
import { ToastrService } from "ngx-toastr";
import {
    Argument,
    CommandLine,
    ExternFunction,
    LogicEvent,
    LogicFunction,
} from "../../domain/logic-components";
import {
    AbstractControl,
    FormArray,
    FormBuilder,
    FormControl,
    FormGroup,
    Validators,
} from "@angular/forms";
import { AlertService } from "src/app/services/alert/alert.service";
import { BehaviorSubject, Observable } from "rxjs";
import { Variable as Variab } from "../../domain/variable";
import { LOWCODEFUNCTIONS } from "../../constants/low-code-functions.constant";

declare let $: any;
declare let css: any;

// Constants
const UNIT = {
    tag: "adt",
    name: "Unit",
} as AlgebraicType;

@Component({
    selector: "app-canvas",
    templateUrl: "./canvas.component.html",
    styleUrls: ["./canvas.component.scss"],
})
export class CanvasComponent implements OnInit {
    // Forms
    logicForm: FormGroup;
    comparisonForm: FormGroup;
    declarationForm: FormGroup;
    httpForm: FormGroup;
    callFuncForm: FormGroup;

    isLogicContainer: boolean = true; //TODO: Change to false
    $logicContainer: BehaviorSubject<boolean>;
    public style: object = {};
    elements: Element[] = [];
    cssObject: CssOut[] = [];
    variables: Variab[] = [];
    externFunc: ExternFunction[] = [...LOWCODEFUNCTIONS];
    logicElements: LogicFunction[] = [
        {
            funcName: "main",
            readonly: true,
            events: [],
            commandLine: [],
            arguments: [],
            returnType: UNIT,
        },
    ];

    constructor(
        private spawnService: SpawnComponentService,
        private showInfosService: ShowComponentInfoService,
        private sendService: SendService,
        private spinner: NgxSpinnerService,
        private toastr: ToastrService,
        private formBuilder: FormBuilder,
        private alert: AlertService
    ) {
        this.$logicContainer = new BehaviorSubject<boolean>(true); //TODO: Change to false
    }

    ngOnInit() {
        this.spawnService.getElements().subscribe((element: Element) => {
            element.name = `component${this.elements.length}`;
            this.elements.push(element);
        });

        this.checkLogicContainerState(this.isLogicContainer);

        this.$logicContainer.subscribe((value: boolean) => {
            this.checkLogicContainerState(value);
        });

        this.logicForm = this.formBuilder.group({
            functions: this.formBuilder.array([]),
        });

        (this.logicForm.get("functions") as FormArray).push(
            this.initForm({
                funcName: "main",
                parametersQuantity: 0,
                parameters: [],
            })
        );

        this.externFunc.forEach((func) => {
            this.formData.controls.push(
                this.initForm({
                    funcName: func.name,
                    parametersQuantity: func.parametersQuantity,
                    parameters: func.parameters,
                })
            );
        });

        this.comparisonForm = this.formBuilder.group({
            comparisonArray: this.formBuilder.array([]),
        });

        this.declarationForm = this.formBuilder.group({
            declarationArray: this.formBuilder.array([]),
        });

        this.httpForm = this.formBuilder.group({
            httpArray: this.formBuilder.array([]),
        });

        this.callFuncForm = this.formBuilder.group({
            callFuncArray: this.formBuilder.array([]),
        });
    }

    get comparisonArrayData() {
        return <FormArray>this.comparisonForm.get("comparisonArray");
    }

    get declarationArrayData() {
        return <FormArray>this.declarationForm.get("declarationArray");
    }

    get httpArrayData() {
        return <FormArray>this.httpForm.get("httpArray");
    }

    getHttpBodyArrayData(
        index: number,
        isEvt: boolean,
        evtIndex: number,
        funcName: string
    ) {
        let httpFormGroup: FormGroup = this.getHttpFormGroup(
            index,
            isEvt,
            evtIndex,
            funcName
        );
        return <FormArray>httpFormGroup.get("httpBodies");
    }

    get callFuncArrayData() {
        return <FormArray>this.callFuncForm.get("callFuncArray");
    }

    getParamentersArrayData(
        index: number,
        isEvt: boolean,
        evtIndex: number,
        funcName: string
    ) {
        let parameterFormGroup: FormGroup = this.getFunctionFormGroup(
            index,
            isEvt,
            evtIndex,
            funcName
        );

        return <FormArray>parameterFormGroup.get("parameters");
    }

    private initComparisonFormArray(comparison: {
        index: number;
        funcName: string;
        evtIndex: number;
        leftExpression: string;
        symbol: string;
        rightExpression: string;
    }): FormGroup {
        return this.formBuilder.group({
            leftExpression: [comparison.leftExpression, Validators.required],
            symbol: [comparison.symbol, Validators.required],
            rightExpression: [comparison.rightExpression, Validators.required],
            index: comparison.index,
            funcName: comparison.funcName,
            evtIndex: comparison.evtIndex,
        });
    }

    private initDeclarationFormArray(declaration: {
        index: number;
        funcName: string;
        evtIndex: number;
        varType: string;
        varName: string;
        varValue: string;
    }): FormGroup {
        return this.formBuilder.group({
            varType: [declaration.varType, [Validators.required]],
            varName: [declaration.varName, [Validators.required]],
            varValue: declaration.varValue,
            index: declaration.index,
            funcName: declaration.funcName,
            evtIndex: declaration.evtIndex,
        });
    }

    private initHttpFormArray(http: {
        index: number;
        funcName: string;
        evtIndex: number;
        httpUrl: string;
        httpMethod: string;
    }): FormGroup {
        return this.formBuilder.group({
            httpUrl: [http.httpUrl, [Validators.required]],
            httpMethod: [http.httpMethod, [Validators.required]],
            httpBodies: this.formBuilder.array([]),
            index: http.index,
            funcName: http.funcName,
            evtIndex: http.evtIndex,
        });
    }

    private initCallFuncFormArray(callFunc: {
        index: number;
        funcName: string;
        evtIndex: number;
        returnType: string;
        varName: string;
        function: string;
        parametersQuantity: number;
        parameters: { paramName: string; paramValue: any; paramType: Type }[];
    }): FormGroup {
        return this.formBuilder.group({
            returnType: callFunc.returnType,
            varName: callFunc.varName,
            function: callFunc.function,
            parametersQuantity: callFunc.parametersQuantity,
            parameters: this.formBuilder.array(callFunc.parameters),
            index: callFunc.index,
            funcName: callFunc.funcName,
            evtIndex: callFunc.evtIndex,
        });
    }

    getComparisonFormGroup(
        index: number,
        isEvt: boolean,
        evtIndex: number,
        funcName: string
    ): FormGroup {
        let curElement: LogicFunction = this.logicElements.find(
            (element) => funcName === element.funcName
        );
        let formIndex: number;
        if (!isEvt) {
            formIndex = curElement.commandLine[index].formIndex;
        } else {
            formIndex =
                curElement.events[evtIndex].commandLine[index].formIndex;
        }

        return this.comparisonArrayData.controls[formIndex] as FormGroup;
    }

    getDeclarationFormGroup(
        index: number,
        isEvt: boolean,
        evtIndex: number,
        funcName: string
    ): FormGroup {
        let curElement: LogicFunction = this.logicElements.find(
            (element) => funcName === element.funcName
        );
        let formIndex: number;
        if (!isEvt) {
            formIndex = curElement.commandLine[index].formIndex;
        } else {
            formIndex =
                curElement.events[evtIndex].commandLine[index].formIndex;
        }

        return this.declarationArrayData.controls[formIndex] as FormGroup;
    }

    getHttpFormGroup(
        index: number,
        isEvt: boolean,
        evtIndex: number,
        funcName: string
    ): FormGroup {
        let curElement: LogicFunction = this.logicElements.find(
            (element) => funcName === element.funcName
        );
        let formIndex: number;
        if (!isEvt) {
            formIndex = curElement.commandLine[index].formIndex;
        } else {
            formIndex =
                curElement.events[evtIndex].commandLine[index].formIndex;
        }

        return this.httpArrayData.controls[formIndex] as FormGroup;
    }

    getCallFuncFormGroup(
        index: number,
        isEvt: boolean,
        evtIndex: number,
        funcName: string
    ): FormGroup {
        let curElement: LogicFunction = this.logicElements.find(
            (element) => funcName === element.funcName
        );
        let formIndex: number;
        if (!isEvt) {
            formIndex = curElement.commandLine[index].formIndex;
        } else {
            formIndex =
                curElement.events[evtIndex].commandLine[index].formIndex;
        }

        return this.callFuncArrayData.controls[formIndex] as FormGroup;
    }

    onChangeCallFunction(
        func: string,
        index: number,
        isEvt: boolean,
        evtIndex: number,
        funcName: string
    ) {
        let curElement: LogicFunction = this.logicElements.find(
            (element) => funcName === element.funcName
        );
        let formIndex: number;
        if (!isEvt) {
            formIndex = curElement.commandLine[index].formIndex;
        } else {
            formIndex =
                curElement.events[evtIndex].commandLine[index].formIndex;
        }

        let funcControl = this.formData.controls.find(
            (ctr) => ctr.get("funcName").value === func
        );

        let control = this.callFuncArrayData.controls[formIndex];
        let funcParams = funcControl.get("parameters") as FormArray;
        let callParams = control.get("parameters") as FormArray;

        callParams.clear();

        for (let formControl of funcParams.controls) {
            callParams.controls.push(
                this.formBuilder.group({
                    paramName: formControl.value["paramName"],
                    paramValue: formControl.value["paramValue"],
                    paramType: formControl.value["paramType"],
                })
            );
        }
        control
            .get("parametersQuantity")
            .patchValue(funcControl.get("parametersQuantity").value);
    }

    getFunctionFormGroup(
        index: number,
        isEvt: boolean,
        evtIndex: number,
        funcName: string
    ): FormGroup {
        let curElement: LogicFunction = this.logicElements.find(
            (element) => funcName === element.funcName
        );

        let formIndex: number;

        if (!isEvt) {
            formIndex = curElement.commandLine[index].formIndex;
        } else {
            formIndex =
                curElement.events[evtIndex].commandLine[index].formIndex;
        }

        return this.callFuncArrayData.controls[formIndex] as FormGroup;
    }

    private checkLogicContainerState(isLogicContainer: boolean) {
        if (isLogicContainer) {
            $("#infos-container").prop("hidden", true);
            $("#canvas-container")
                .addClass("col-lg-12")
                .removeClass("col-lg-9");
            let children = document.getElementById("side-menu").childNodes;
            children.forEach((child: HTMLElement) => {
                if (child.className === "") {
                    child.setAttribute("hidden", "true");
                }
            });
        } else {
            $("#infos-container").prop("hidden", false);
            $("#canvas-container")
                .removeClass("col-lg-12")
                .addClass("col-lg-9");
            let children = document.getElementById("side-menu").childNodes;
            children.forEach((child: HTMLElement) => {
                if (child.className === "") {
                    child.removeAttribute("hidden");
                }
            });
        }
    }

    // Change Container listeners
    setLogicContainer(value: boolean) {
        this.isLogicContainer = value;
        this.$logicContainer.next(value);
    }

    getLogicContainer(): Observable<boolean> {
        return this.$logicContainer.asObservable();
    }

    validate(event: ResizeEvent): boolean {
        console.log(event);
        const MIN_DIMENSIONS_PX: number = 50;
        if (
            event.rectangle.width &&
            event.rectangle.height &&
            (event.rectangle.width < MIN_DIMENSIONS_PX ||
                event.rectangle.height < MIN_DIMENSIONS_PX)
        ) {
            return false;
        }
        return true;
    }

    onResizeEnd(event: ResizeEvent) {
        this.style = {
            position: "fixed",
            left: `${event.rectangle.left}px`,
            top: `${event.rectangle.top}px`,
            width: `${event.rectangle.width}px`,
            height: `${event.rectangle.height}px`,
        };
    }

    click(ev: any, targ: any) {
        let comp: string = `#${targ.id}`;
        let infos: Info = {
            html: {
                name: $(comp).prop("id") !== null ? $(comp).prop("id") : "",
                type:
                    $(comp).prop("tagName") !== null
                        ? $(comp).prop("tagName").toLowerCase()
                        : "",
                text: $(comp).text() !== null ? $(comp).text() : "",
            },
            css: {
                width: $(comp).css("width"),
                height: $(comp).css("height"),
            },
        };
        this.showInfosService.setComponentInfos(infos);
    }

    private createComponent(
        element: HTMLElement,
        htmlObject: HtmlTagOut,
        isChild: boolean
    ): HtmlTagOut {
        let childNodes = element.childNodes;
        htmlObject.tag = element.tagName.toLowerCase();
        const canvasPosition = $("#canvas").offset();
        for (let i = 0; i < element.attributes.length; i++) {
            let nodeName: string = element.attributes[i].nodeName;
            let value: string = element.attributes[i].value;
            switch (nodeName) {
                case "id":
                    htmlObject.attributes.push(["id", value]);
                    break;
                case "class":
                    let classArray = value.split(" ");
                    classArray = classArray.filter((value) => {
                        return !(
                            value.includes("drag") || value.includes("ng")
                        );
                    });
                    let classString: string = classArray.join(" ");
                    htmlObject.attributes.push(["class", classString]);
                    break;
                case "style":
                    let css: CssOut = {
                        className: `#${element.id.trim()}`,
                        attributes: [],
                    };
                    if (!isChild) {
                        let elementPosition = $(`#${element.id}`).offset();
                        let top = elementPosition.top - canvasPosition.top;
                        let left = elementPosition.left - canvasPosition.left;
                        top = top < 0 ? 0 : top;
                        left = left < 0 ? 0 : left;
                        css.attributes.push(["top", top.toString()]);
                        css.attributes.push(["left", left.toString()]);
                        css.attributes.push(["position", "absolute"]);
                    }
                    let cssAttr = value.split("; ");
                    cssAttr.forEach((attr) => {
                        attr = attr.trim();
                        if (
                            attr !== "" &&
                            !attr.includes("transform") &&
                            !attr.includes("translate")
                        ) {
                            css.attributes.push([
                                attr.split(":")[0].trim(),
                                attr.split(":")[1].trim(),
                            ]);
                        }
                    });
                    this.cssObject.push(css);
                    break;
                default:
                    break;
            }
        }
        let elementCss: string[] = css(element);
        let localCssObj: CssOut;
        elementCss.forEach((css: string) => {
            let className = css.substring(0, css.indexOf("{"));
            let attributes = css.substring(
                css.indexOf("{") + 1,
                css.indexOf("}")
            );
            if (!className.includes("drag")) {
                localCssObj = {
                    className: className.trim(),
                    attributes: [],
                };
                let cssAttr = attributes.split("; ");
                cssAttr.forEach((attr) => {
                    attr = attr.trim();
                    if (attr !== "") {
                        localCssObj.attributes.push([
                            attr.split(":")[0].trim(),
                            attr.split(":")[1].trim(),
                        ]);
                    }
                });
                this.cssObject.push(localCssObj);
            }
        });
        if (element.hasChildNodes()) {
            let childNodes = element.childNodes;
            for (let i = 0; i < childNodes.length; i++) {
                let child = childNodes[i];
                switch (child.nodeType) {
                    case Node.TEXT_NODE:
                        let htmlText: HtmlTextOut = {
                            text: child.nodeValue.trim(),
                        };
                        htmlObject.ast.push(htmlText);
                        break;
                    case Node.ELEMENT_NODE:
                        htmlObject.ast.push(
                            this.createComponent(
                                child as HTMLElement,
                                htmlObject,
                                true
                            )
                        );
                        break;
                }
            }
        }
        return htmlObject;
    }

    private getLogicFunctions(): Function[] {
        let functions: Function[] = [];
        this.logicElements.forEach((func: LogicFunction) => {
            let funct: Function = {
                name: func.funcName,
                arguments: [],
                type: this.getType("function", func.arguments, func.returnType),
                body: [],
            };
            let ast: AST;
            func.commandLine.forEach((cl: CommandLine) => {
                let controls = this.checkFormControlType(
                    cl.type.name,
                    cl.type.clType
                );
                let expression: Expression;
                let curControl = controls[cl.formIndex];
                expression = this.getExpressionType(
                    cl.type.name,
                    cl.type.clType,
                    curControl as FormGroup
                );
                ast = this.getAstType(cl.type.name, expression, curControl);
                funct.body.push(ast);
            });

            console.log(funct);
            functions.push(funct);
        });

        return functions;
    }

    async apply() {
        this.spinner.show("loadingSpinner");
        let value: Output = {
            name: sessionStorage.getItem("projectName"),
            ast: {
                pages: [],
            },
        };
        let pages: Page[] = [];
        let pageTest: Page = {
            name: "Page Test",
            css: [],
            logic: [],
            html: [],
        };
        $("#canvas")
            .children()
            .each((index: number, child: HTMLElement) => {
                let nodes = child.childNodes;
                let element: HTMLElement;
                let htmlObject: HtmlTagOut = {
                    tag: "",
                    ast: [],
                    attributes: [],
                };
                for (let i = 0; i < nodes.length; i++) {
                    if (nodes[i].nodeType !== Node.COMMENT_NODE) {
                        element = nodes[i] as HTMLElement;
                    }
                }
                htmlObject = this.createComponent(element, htmlObject, false);
                pageTest.html.push(htmlObject);
            });
        let logicObject: Module = {
            adtTemplates: new Map(),
            externs: new Map(),
            functions: [],
            importedModules: ["Prelude", "JSON", "REST"],
            moduleName: "main",
        };
        logicObject.functions = this.getLogicFunctions();
        pageTest.css = this.cssObject;
        pageTest.logic.push(logicObject);
        value.ast.pages.push(pageTest);
        console.log(value);
        try {
            let projectID = await this.sendService.postCode(value);
            sessionStorage.setItem("projectID", projectID.toString());
            this.toastr.success("Aplicado com sucesso!", "Sucesso!", {
                progressBar: true,
                closeButton: true,
            });
        } catch (e) {
            if (e instanceof HttpErrorResponse) {
                let error = e.error.errors.join("; ");
                switch (e.status) {
                    case 404:
                        this.toastr.error(
                            `Motivo(s): ${error}`,
                            `Erro ${e.status} - ${e.error.message}`,
                            { progressBar: true, closeButton: true }
                        );
                        break;
                    default:
                        this.toastr.error(
                            `Motivo(s): ${error}`,
                            `Erro ${e.status} - ${e.error.message}`,
                            { progressBar: true, closeButton: true }
                        );
                        break;
                }
            }
        } finally {
            this.spinner.hide("loadingSpinner");
        }
    }

    private checkFormControlType(
        clName: string,
        clType: string
    ): AbstractControl[] {
        switch (clName) {
            case "decision":
                switch (clType) {
                    case "comparison":
                        return this.comparisonArrayData.controls;
                    default:
                        return null;
                }
            case "declaration":
                return this.declarationArrayData.controls;
            case "http":
                return this.httpArrayData.controls;
            case "call":
                return this.callFuncArrayData.controls;
            default:
                return null;
        }
    }

    private getAstType(
        clTypeName: string,
        expression: Expression,
        control: AbstractControl
    ): AST {
        let newAst: any;
        switch (clTypeName) {
            case "decision":
                newAst = new If(expression, [], []);
                break;
            case "repetition":
                newAst = new While(expression, []);
                break;
            case "declaration":
                return new Var(
                    control.get("varName").value,
                    this.getType(control.get("varType").value, [], null),
                    expression
                );
            default:
                return null;
        }
        return newAst;
    }

    private getType(
        type: string,
        funcArguments: Argument[],
        funcReturnType: Type
    ): Type {
        let newType: Type;
        switch (type) {
            case "adt":
                newType = new AlgebraicType(null);
                break;
            case "array":
                newType = new ArrayType(null);
                break;
            case "char":
                newType = new CharType();
                break;
            case "double":
                newType = new DoubleType();
                break;
            case "function":
                let args: Type[] = [];
                funcArguments.forEach((argument) => {
                    args.push(this.getType(argument.returnType, [], null));
                });
                newType = new FunctionType(args, funcReturnType);
                break;
            case "integer":
                newType = new IntegerType();
                break;
            case "record":
                newType = new RecordType([]);
                break;
            case "text":
                newType = new TextType();
                break;
        }
        return newType;
    }

    private getExpressionType(
        clName: string,
        clType: string,
        control: FormGroup
    ): Expression {
        let newExpression: Expression;

        switch (clName) {
            case "decision":
                switch (clType) {
                    case "comparison":
                    case "booleanLogic":
                        newExpression = new BinaryOp(
                            control.get("leftExpression").value,
                            control.get("symbol").value,
                            control.get("rightExpression").value
                        );
                        break;
                    default:
                        newExpression = null;
                }
                break;
            case "declaration":
                newExpression = new Literal_(
                    this.getLiteralByVarType(
                        control.get("varType").value,
                        this.getValueByVarType(
                            control.get("varType").value,
                            control.get("varValue").value
                        )
                    )
                );
            case "call":
                return new Call(new Variable(""), []);
            default:
                newExpression = null;
        }

        return newExpression;
    }

    private getValueByVarType(varType: string, value: string): number | string {
        let newValue: number | string;
        switch (varType) {
            case "integer":
                newValue = parseInt(value, 10);
                break;
            case "double":
                newValue = parseFloat(value);
            default:
                newValue = value;
        }
        return newValue;
    }

    private getLiteralByVarType(type: string, value: any): Literal {
        let newLiteral: Literal;
        switch (type) {
            case "char":
                newLiteral = new Char(value);
                break;
            case "double":
                newLiteral = new Double(value);
                break;
            case "integer":
                newLiteral = new Integer(value);
                break;
            case "text":
                newLiteral = new Text(value);
                break;
        }

        return newLiteral;
    }

    get formData() {
        return <FormArray>this.logicForm.get("functions");
    }

    private initForm(func: {
        funcName: string;
        parametersQuantity: number;
        parameters: { paramName: string; paramValue: any; paramType: Type }[];
    }) {
        return this.formBuilder.group({
            funcName: [func.funcName, [Validators.required]],
            parametersQuantity: func.parametersQuantity,
            parameters: this.formBuilder.array(func.parameters),
        });
    }

    getCurFunction(index: number): FormGroup {
        return this.formData.controls[index] as FormGroup;
    }

    // Logic interface handle
    addAction() {
        this.logicElements.push({
            funcName: "",
            readonly: false,
            commandLine: [],
            events: [],
            arguments: [],
            returnType: UNIT,
        });
        const control = this.formData.controls;
        control.push(
            this.initForm({
                funcName: "",
                parametersQuantity: 0,
                parameters: [],
            })
        );
    }

    removeFunc(index: number) {
        this.logicElements.splice(index, 1);
    }

    createItem(func: string, type: string) {
        let curElement: LogicFunction;
        const control = this.comparisonArrayData.controls;
        switch (type) {
            case "cl":
                curElement = this.logicElements.find(
                    (element) => func === element.funcName
                );
                control.push(
                    this.initComparisonFormArray({
                        leftExpression: "",
                        symbol: "Different",
                        rightExpression: "",
                        funcName: func,
                        index: curElement.commandLine.length - 1,
                        evtIndex: -1,
                    })
                );
                curElement.commandLine.push({
                    exec: null,
                    type: { name: "decision", clType: "comparison" },
                    formIndex: control.length - 1,
                });
                break;
            case "evt":
                if (this.elements.length === 0) {
                    this.alert.createConfirmDialog(
                        "Atenção!",
                        "É necessário haver algum componente antes de criar um evento!"
                    );
                } else if (
                    this.elements.length === this.logicElements[0].events.length
                ) {
                    this.alert.createConfirmDialog(
                        "Atenção!",
                        "Não há mais componentes para terem eventos adicionados!"
                    );
                } else {
                    curElement = this.logicElements.find(
                        (element) => func === element.funcName
                    );
                    curElement.events.push({
                        eventName: "",
                        commandLine: [],
                        eventType: "onClick",
                    });
                }

                break;
        }
    }

    removeItem(func: string, type: string, index: number) {
        let curElement: LogicFunction;
        switch (type) {
            case "cl":
                curElement = this.logicElements.find(
                    (element) => func === element.funcName
                );
                curElement.commandLine.splice(index, 1);
                break;
            case "evt":
                curElement = this.logicElements.find(
                    (element) => func === element.funcName
                );
                curElement.events.splice(index, 1);
                break;
        }
    }

    createEvtCl(evt: string) {
        let curEvt: LogicEvent;
        const control = this.comparisonArrayData.controls;
        this.logicElements.find((element) => {
            curEvt = element.events.find((ev) => evt === ev.eventName);
        });
        curEvt.commandLine.push({
            exec: null,
            type: { name: "decision", clType: "comparison" },
            formIndex: control.length - 1,
        });
    }

    removeEvtCl(eventName: string, index: number) {
        let curEvt: LogicEvent;
        let curElement = this.logicElements.find((element) => {
            curEvt = element.events.find((ev) => eventName === ev.eventName);
        });
        curEvt.commandLine.splice(index, 1);
    }

    funcClTypeChange(
        value: string,
        isEvt: boolean,
        funcName: string,
        index: number,
        evtIndex: number
    ) {
        let lastValue: string;
        let lastClType: string;
        let curElement: LogicFunction = this.logicElements.find(
            (element) => funcName === element.funcName
        );
        let formIndex: number = this.getFormIndexOnFunctionCommandLineChange(
            value,
            curElement.commandLine[index].type.clType,
            funcName,
            index,
            isEvt,
            evtIndex
        );
        if (!isEvt) {
            lastValue = curElement.commandLine[index].type.name;
            lastClType = curElement.commandLine[index].type.clType;
            this.removeLastValueFromForm(
                lastValue,
                lastClType,
                curElement.commandLine[index].formIndex
            );
            curElement.commandLine[index].type.name = value;
            curElement.commandLine[index].formIndex = formIndex;
        } else {
            lastValue =
                curElement.events[evtIndex].commandLine[index].type.name;
            lastClType =
                curElement.events[evtIndex].commandLine[index].type.clType;
            this.removeLastValueFromForm(
                lastValue,
                lastClType,
                curElement.commandLine[index].formIndex
            );
            curElement.events[evtIndex].commandLine[index].type.name = value;
            curElement.events[evtIndex].commandLine[
                index
            ].formIndex = formIndex;
        }
    }

    changeIfCondition(
        value: string,
        funcName: string,
        isEvt: boolean,
        index: number,
        evtIndex: number
    ) {
        let curElement: LogicFunction;
        curElement = this.logicElements.find(
            (element) => funcName === element.funcName
        );
        let formIndex: number = this.getFormIndexOnFunctionCommandLineChange(
            value,
            curElement.commandLine[index].type.clType,
            funcName,
            index,
            isEvt,
            evtIndex
        );
        if (!isEvt) {
            curElement.commandLine[index].type.clType = value;
            curElement.commandLine[index].formIndex[formIndex];
        } else {
            curElement.events[evtIndex].commandLine[index].type.clType = value;
            curElement.events[evtIndex].commandLine[index].formIndex[formIndex];
        }
    }

    private getFormIndexOnFunctionCommandLineChange(
        value: string,
        clType: string,
        funcName: string,
        index: number,
        isEvt: boolean,
        evtIndex: number
    ): number {
        let control: AbstractControl[];
        switch (value) {
            case "declaration":
                control = this.declarationArrayData.controls;
                this.variables.push({
                    name: "",
                    type: this.getType("adt", [], null),
                    value: "",
                });
                control.push(
                    this.initDeclarationFormArray({
                        varType: "adt",
                        varName: "",
                        varValue: "",
                        funcName,
                        index,
                        evtIndex: isEvt ? evtIndex : -1,
                    })
                );
                break;
            case "decision":
                switch (clType) {
                    case "comparison":
                        control = this.comparisonArrayData.controls;
                        control.push(
                            this.initComparisonFormArray({
                                leftExpression: "",
                                symbol: "",
                                rightExpression: "",
                                funcName,
                                index,
                                evtIndex: isEvt ? evtIndex : -1,
                            })
                        );
                        break;
                }
                break;
            case "http":
                control = this.httpArrayData.controls;
                control.push(
                    this.initHttpFormArray({
                        httpMethod: "get",
                        httpUrl: "",
                        funcName,
                        index,
                        evtIndex: isEvt ? evtIndex : -1,
                    })
                );
                break;
            case "call":
                control = this.callFuncArrayData.controls;
                control.push(
                    this.initCallFuncFormArray({
                        returnType: "with-return",
                        varName: "",
                        function: "",
                        funcName,
                        index,
                        parameters: [],
                        evtIndex: isEvt ? evtIndex : -1,
                        parametersQuantity: -1,
                    })
                );
                break;
            default:
                return 0;
        }
        return control.length - 1;
    }

    private removeLastValueFromForm(
        lastValue: string,
        lastClType: string,
        lastFormIndex: number
    ) {
        let control: AbstractControl[];
        switch (lastValue) {
            case "declaration":
                this.variables.splice(lastFormIndex, 1);
                control = this.declarationArrayData.controls;
                break;
            case "decision":
                switch (lastClType) {
                    case "comparison":
                        control = this.comparisonArrayData.controls;
                        break;
                }
                break;
            case "http":
                control = this.httpArrayData.controls;
                break;
            case "call":
                control = this.callFuncArrayData.controls;
                break;
            default:
                return;
        }
        control.splice(lastFormIndex, 1);
    }

    ifCondition(
        funcName: string,
        isEvt: boolean,
        index: number,
        evtIndex: number
    ): string {
        let curElement: LogicFunction = this.logicElements.find(
            (element) => funcName === element.funcName
        );
        if (!isEvt) {
            let clType = curElement.commandLine[index].type.clType;
            return clType;
        } else {
            return curElement.events[evtIndex].commandLine[index].type.clType;
        }
    }

    changeEventType(value: string, index: number) {
        this.logicElements[0].events[index].eventType = value;
    }

    addBody(index: number, isEvt: boolean, evtIndex: number, funcName: string) {
        const control = this.getHttpBodyArrayData(
            index,
            isEvt,
            evtIndex,
            funcName
        );
        // control.push(
        //     this.initHttpBodiesFormArray({
        //         httpHeaderName: "",
        //         httpHeaderValue: "",
        //     })
        // );
    }

    removeBody(
        index: number,
        isEvt: boolean,
        evtIndex: number,
        funcName: string,
        headerIndex: number
    ) {
        const control = this.getHttpBodyArrayData(
            index,
            isEvt,
            evtIndex,
            funcName
        ).controls;
        control.splice(headerIndex, 1);
    }

    callFuncChangeReturnType(
        index: number,
        isEvt: boolean,
        evtIndex: number,
        funcName: string
    ): string {
        let curElement: LogicFunction = this.logicElements.find(
            (element) => funcName === element.funcName
        );
        let control: AbstractControl[];
        let formIndex: number;
        if (!isEvt) {
            control = this.checkFormControlType(
                curElement.commandLine[index].type.name,
                curElement.commandLine[index].type.clType
            );
            formIndex = curElement.commandLine[index].formIndex;
            let returnType = control[formIndex].get("returnType").value;
            return returnType;
        } else {
            control = this.checkFormControlType(
                curElement.events[evtIndex].commandLine[index].type.name,
                curElement.events[evtIndex].commandLine[index].type.clType
            );
            formIndex =
                curElement.events[evtIndex].commandLine[index].formIndex;
            let returnType = control[formIndex].get("returnType").value;
            return returnType;
        }
    }
}
