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
    Expression_,
    Assign,
    Return,
    StringExpression,
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
const UNIT = new AlgebraicType("Unit");

@Component({
    selector: "app-canvas",
    templateUrl: "./canvas.component.html",
    styleUrls: ["./canvas.component.scss"],
})
export class CanvasComponent implements OnInit {
    // Forms
    logicForm: FormGroup;
    decisionForm: FormGroup;
    repetitionForm: FormGroup;
    comparisonForm: FormGroup;
    booleanLogicForm: FormGroup;
    customConditionForm: FormGroup;
    declarationForm: FormGroup;
    callFuncForm: FormGroup;
    attributionForm: FormGroup;
    returnForm: FormGroup;

    // Attributes
    isLogicContainer: boolean = true; //TODO: Change to false
    $logicContainer: BehaviorSubject<boolean>;
    public style: object = {};
    elements: Element[] = [];
    cssObject: CssOut[] = [];
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

        this.createForms();
    }

    private createForms() {
        this.logicForm = this.formBuilder.group({
            functions: this.formBuilder.array([]),
        });

        (this.logicForm.get("functions") as FormArray).push(
            this.initForm({
                funcName: "main",
                parameters: [],
                returnType: UNIT,
            })
        );

        this.externFunc.forEach((func) => {
            this.formData.controls.push(
                this.initForm({
                    funcName: func.name,
                    parameters: func.parameters,
                    returnType: func.returnType,
                })
            );
        });

        this.decisionForm = this.formBuilder.group({
            decisionArray: this.formBuilder.array([]),
        });

        this.repetitionForm = this.formBuilder.group({
            repetitionArray: this.formBuilder.array([]),
        });

        this.comparisonForm = this.formBuilder.group({
            comparisonArray: this.formBuilder.array([]),
        });

        this.booleanLogicForm = this.formBuilder.group({
            booleanLogicArray: this.formBuilder.array([]),
        });

        this.customConditionForm = this.formBuilder.group({
            customConditionArray: this.formBuilder.array([]),
        });

        this.declarationForm = this.formBuilder.group({
            declarationArray: this.formBuilder.array([]),
        });

        this.callFuncForm = this.formBuilder.group({
            callFuncArray: this.formBuilder.array([]),
        });

        this.attributionForm = this.formBuilder.group({
            attributionArray: this.formBuilder.array([]),
        });

        this.returnForm = this.formBuilder.group({
            returnArray: this.formBuilder.array([]),
        });
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

    // Begin Get Form Array Datas
    get formData() {
        return <FormArray>this.logicForm.get("functions");
    }

    getCurFunction(index: number): FormGroup {
        return this.formData.controls[index] as FormGroup;
    }

    get decisionArrayData() {
        return <FormArray>this.decisionForm.get("decisionArray");
    }

    getTrueBranch(
        index: number,
        isEvt: boolean,
        evtIndex: number,
        funcName: string,
        cascade: boolean
    ) {
        let decisionFormGroup: FormGroup = this.getDecisionFormGroup(
            index,
            isEvt,
            evtIndex,
            funcName,
            cascade
        );

        return <FormArray>decisionFormGroup.get("trueBranchAst");
    }

    getFalseBranch(
        index: number,
        isEvt: boolean,
        evtIndex: number,
        funcName: string,
        cascade: boolean
    ) {
        let decisionFormGroup: FormGroup = this.getDecisionFormGroup(
            index,
            isEvt,
            evtIndex,
            funcName,
            cascade
        );

        return <FormArray>decisionFormGroup.get("falseBranchAst");
    }

    get repetitionArrayData() {
        return <FormArray>this.repetitionForm.get("repetitionArray");
    }

    getWhileAst(
        index: number,
        isEvt: boolean,
        evtIndex: number,
        funcName: string,
        cascade: boolean
    ) {
        let repetitionFormGroup: FormGroup = this.getRepetitionFormGroup(
            index,
            isEvt,
            evtIndex,
            funcName,
            cascade
        );

        return <FormArray>repetitionFormGroup.get("whileAst");
    }

    get comparisonArrayData() {
        return <FormArray>this.comparisonForm.get("comparisonArray");
    }

    get booleanLogicArrayData() {
        return <FormArray>this.booleanLogicForm.get("booleanLogicArray");
    }

    get customConditionArrayData() {
        return <FormArray>this.customConditionForm.get("customConditionArray");
    }

    get declarationArrayData() {
        return <FormArray>this.declarationForm.get("declarationArray");
    }

    get callFuncArrayData() {
        return <FormArray>this.callFuncForm.get("callFuncArray");
    }

    get attributionArrayData() {
        return <FormArray>this.attributionForm.get("attributionArray");
    }

    getParamentersArrayData(
        index: number,
        isEvt: boolean,
        evtIndex: number,
        funcName: string
    ) {
        let parameterFormGroup: FormGroup = this.getCallFuncFormGroup(
            index,
            isEvt,
            evtIndex,
            funcName
        );

        return <FormArray>parameterFormGroup.get("parameters");
    }

    get returnArrayData() {
        return <FormArray>this.returnForm.get("returnArray");
    }

    getFunctionParams(funcIndex: number) {
        return <FormArray>this.formData.controls[funcIndex].get("parameters");
    }
    // End Get Form Array Datas

    // Begin Create Array Data
    private initForm(func: {
        funcName: string;
        parameters: { paramName: string; paramValue: any; paramType: Type }[];
        returnType: string | Type;
    }) {
        return this.formBuilder.group({
            funcName: [func.funcName, [Validators.required]],
            parameters: this.formBuilder.array(func.parameters),
            returnType: func.returnType,
        });
    }

    private initFuncParams(params: { paramName: string; paramType: string }) {
        return this.formBuilder.group({
            paramName: [params.paramName, Validators.required],
            paramType: params.paramType,
        });
    }

    private initDecisionFormArray(decision: {
        index: number;
        funcName: string;
        evtIndex: number;
        expression: FormGroup;
    }): FormGroup {
        return this.formBuilder.group({
            expression: decision.expression,
            trueBranchAst: this.formBuilder.array([]),
            falseBranchAst: this.formBuilder.array([]),
            index: decision.index,
            funcName: decision.funcName,
            evtIndex: decision.evtIndex,
        });
    }

    private initTrueBranchAstFormArray(trueBranch: {
        exec: FormGroup;
        clType: string;
        clTypeName: string;
        parentIndex: number;
    }): FormGroup {
        return this.formBuilder.group({
            exec: trueBranch.exec,
            clType: trueBranch.clType,
            clTypeName: trueBranch.clTypeName,
            parentIndex: trueBranch.parentIndex,
        });
    }

    private initFalseBranchAstFormArray(falseBranch: {
        exec: FormGroup;
        clType: string;
        clTypeName: string;
        parentIndex: number;
    }): FormGroup {
        return this.formBuilder.group({
            exec: falseBranch.exec,
            clType: falseBranch.clType,
            clTypeName: falseBranch.clTypeName,
            parentIndex: falseBranch.parentIndex,
        });
    }

    private initRepetitionFormArray(repetition: {
        index: number;
        funcName: string;
        evtIndex: number;
        expression: AbstractControl;
    }): FormGroup {
        return this.formBuilder.group({
            expression: repetition.expression,
            whileAst: this.formBuilder.array([]),
            index: repetition.index,
            funcName: repetition.funcName,
            evtIndex: repetition.evtIndex,
        });
    }

    private initWhileAstFormArray(whileAst: {
        exec: FormGroup;
        clType: string;
        clTypeName: string;
        parentIndex: number;
    }): FormGroup {
        return this.formBuilder.group({
            exec: whileAst.exec,
            clType: whileAst.clType,
            clTypeName: whileAst.clTypeName,
            parentIndex: whileAst.parentIndex,
        });
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

    private initBooleanLogicFormArray(booleanLogic: {
        index: number;
        funcName: string;
        evtIndex: number;
        leftExpression: string;
        symbol: string;
        rightExpression: string;
    }): FormGroup {
        return this.formBuilder.group({
            leftExpression: [booleanLogic.leftExpression, Validators.required],
            symbol: [booleanLogic.symbol, Validators.required],
            rightExpression: [
                booleanLogic.rightExpression,
                Validators.required,
            ],
            index: booleanLogic.index,
            funcName: booleanLogic.funcName,
            evtIndex: booleanLogic.evtIndex,
        });
    }

    private initCustomConditionFormArray(custom: {
        index: number;
        funcName: string;
        evtIndex: number;
        customCondition: string;
    }) {
        return this.formBuilder.group({
            customCondition: [custom.customCondition, Validators.required],
            index: custom.index,
            funcName: custom.funcName,
            evtIndex: custom.evtIndex,
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
            varType: [declaration.varType, Validators.required],
            varName: [declaration.varName, Validators.required],
            varValue: declaration.varValue,
            index: declaration.index,
            funcName: declaration.funcName,
            evtIndex: declaration.evtIndex,
        });
    }

    private initCallFuncFormArray(callFunc: {
        index: number;
        funcName: string;
        evtIndex: number;
        returnType: string;
        varName: string;
        function: string;
    }): FormGroup {
        return this.formBuilder.group({
            returnType: callFunc.returnType,
            varName: callFunc.varName,
            function: callFunc.function,
            parameters: this.formBuilder.array([]),
            index: callFunc.index,
            funcName: callFunc.funcName,
            evtIndex: callFunc.evtIndex,
        });
    }

    private initParams(params: FormControl): FormGroup {
        return this.formBuilder.group({
            paramName: params.value["paramName"],
            paramType: params.value["paramType"],
            paramValue: params.value["paramValue"],
        });
    }

    private initAttributionFormArray(attribution: {
        index: number;
        funcName: string;
        evtIndex: number;
        varName: string;
        attributionValue: string;
    }) {
        return this.formBuilder.group({
            varName: attribution.varName,
            attributionValue: [
                attribution.attributionValue,
                Validators.required,
            ],
            index: attribution.index,
            funcName: attribution.funcName,
            evtIndex: attribution.evtIndex,
        });
    }

    private initReturnFormArray(returnValue: { value: any }) {
        return this.formBuilder.group({
            returnValue: [returnValue.value, Validators.required],
        });
    }
    // End Create Array Data

    // Get Form Group
    getDecisionFormGroup(
        index: number,
        isEvt: boolean,
        evtIndex: number,
        funcName: string,
        cascade: boolean
    ): FormGroup {
        if (!cascade) {
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

            return this.decisionArrayData.controls[formIndex] as FormGroup;
        }
    }

    getRepetitionFormGroup(
        index: number,
        isEvt: boolean,
        evtIndex: number,
        funcName: string,
        cascade: boolean
    ): FormGroup {
        if (!cascade) {
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

            return this.repetitionArrayData.controls[formIndex] as FormGroup;
        }
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

    getBooleanLogicFormGroup(
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

        return this.booleanLogicArrayData.controls[formIndex] as FormGroup;
    }

    getCustomConditionFormGroup(
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

        return this.customConditionArrayData.controls[formIndex] as FormGroup;
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

    getAttributionFormGroup(
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

        return this.attributionArrayData.controls[formIndex] as FormGroup;
    }

    getReturnFormGroup(
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

        return this.returnArrayData.controls[formIndex] as FormGroup;
    }

    // Change Container listeners
    setLogicContainer(value: boolean) {
        this.isLogicContainer = value;
        this.$logicContainer.next(value);
    }

    getLogicContainer(): Observable<boolean> {
        return this.$logicContainer.asObservable();
    }

    // Resize Components
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

    // Send component data to info
    click(ev: MouseEvent, targ: HTMLElement) {
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

    // Create an event to component
    onDoubleClick(ev: MouseEvent, targ: HTMLElement) {
        const id = targ.id;

        this.logicElements[0].events.push({
            commandLine: [],
            eventName: id,
            eventType: "onClick",
        });

        this.isLogicContainer = true;
        this.setLogicContainer(true);
    }

    // Project apply
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
        console.log(JSON.stringify(value));
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

    // begin HTML part
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
    // end HTML part

    // Begin Logic Part
    private getLogicFunctions(): Function[] {
        let functions: Function[] = [];
        this.logicElements.forEach((func: LogicFunction, funcIndex: number) => {
            let funct: Function = {
                name: this.formData.controls[funcIndex].get("funcName").value,
                arguments: [],
                type: null,
                body: [],
            };
            let ast: AST;
            //Arguments
            let paramTypes: Argument[] = [];
            func.arguments.forEach((args: Argument, index: number) => {
                const paramFormArray = this.formData.controls[funcIndex].get(
                    "parameters"
                ) as FormArray;
                let control = paramFormArray.controls[index];
                funct.arguments.push(control.get("paramName").value);
                paramTypes.push({
                    name: control.get("paramName").value,
                    returnType: control.get("paramType").value,
                });
            });
            const returnValue = this.formData.controls[funcIndex].get(
                "returnType"
            ).value;
            funct.type = this.getType(
                "function",
                paramTypes,
                typeof returnValue === "string"
                    ? this.getType(returnValue, [], null)
                    : UNIT
            );
            //Command Lines
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

            console.log(JSON.stringify(funct));
            functions.push(funct);
        });

        return functions;
    }

    private checkFormControlType(
        clName: string,
        clType: string
    ): AbstractControl[] {
        switch (clName) {
            case "decision":
            case "repetition":
                switch (clType) {
                    case "comparison":
                        return this.comparisonArrayData.controls;
                    case "booleanLogic":
                        return this.booleanLogicArrayData.controls;
                    case "custom":
                        return this.customConditionArrayData.controls;
                    default:
                        return null;
                }
            case "declaration":
                return this.declarationArrayData.controls;
            case "call":
                return this.callFuncArrayData.controls;
            case "attribution":
                return this.attributionArrayData.controls;
            case "return":
                return this.returnArrayData.controls;
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
                newAst = new Var(
                    control.get("varName").value,
                    this.getType(control.get("varType").value, [], null),
                    expression
                );
                break;
            case "call":
                newAst = new Expression_(expression);
                break;
            case "attribution":
                newAst = new Expression_(expression);
                break;
            case "return":
                if (expression instanceof StringExpression) {
                    newAst = new Return(expression.value);
                } else {
                    newAst = new Return(
                        expression === null ? UNIT : expression
                    );
                }
                break;
            default:
                newAst = null;
                break;
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
            case "bool":
                newType = new AlgebraicType("Bool");
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
            case "void":
                newType = UNIT;
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
            case "repetition":
                switch (clType) {
                    case "comparison":
                    case "booleanLogic":
                        newExpression = new BinaryOp(
                            control.get("leftExpression").value,
                            control.get("symbol").value,
                            control.get("rightExpression").value
                        );
                        break;
                    case "custom":
                        newExpression = new StringExpression(
                            control.get("customCondition").value
                        );
                    default:
                        newExpression = null;
                        break;
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
                break;
            case "call":
                let args: Expression[] = [];
                let paramControls = (control.get("parameters") as FormArray)
                    .controls;
                paramControls.forEach((param) => {
                    let exp: Expression;
                    let paramType = param.get("paramType").value;
                    if (typeof paramType === "string") {
                        paramType = this.getType(paramType, [], null);
                    }
                    switch (paramType.constructor) {
                        case DoubleType:
                            exp = new Literal_(
                                new Double(
                                    parseFloat(param.get("paramValue").value)
                                )
                            );
                            break;
                        case IntegerType:
                            exp = new Literal_(
                                new Integer(
                                    parseInt(param.get("paramValue").value)
                                )
                            );
                            break;
                        case TextType:
                            exp = new Literal_(
                                new Text(param.get("paramValue").value)
                            );
                            break;
                        case CharType:
                            exp = new Literal_(
                                new Char(param.get("paramValue").value)
                            );
                            break;
                        case FunctionType:
                            exp = new Variable(param.get("paramValue").value);
                            break;
                    }

                    args.push(exp);
                });
                switch (control.get("returnType").value) {
                    case "with-return":
                        newExpression = new Assign(
                            new Variable(control.get("varName").value),
                            new Call(
                                new Variable(control.get("function").value),
                                args
                            )
                        );
                        break;
                    case "without-return":
                        newExpression = new Call(
                            new Variable(control.get("function").value),
                            args
                        );
                        break;
                }
                break;
            case "attribution":
                newExpression = new Assign(
                    new Variable(control.get("varName").value),
                    new StringExpression(control.get("attributionValue").value)
                );
                break;
            case "return":
                const returnValue: string = control
                    .get("returnValue")
                    .value.trim();
                if (returnValue !== "") {
                    let variab = this.declarationArrayData.controls.find(
                        (vari) => {
                            return vari.get("varName").value === returnValue;
                        }
                    );

                    if (variab) {
                        newExpression = new Variable(
                            variab.get("varName").value
                        );
                    } else {
                        newExpression = new StringExpression(returnValue);
                    }
                } else {
                    newExpression = null;
                }
                break;
            default:
                newExpression = null;
                break;
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
    // End Logic Part
    // End Project Apply

    // Begin Logic interface handle
    addAction() {
        this.logicElements.push({
            funcName: `function${this.logicElements.length - 1}`,
            readonly: false,
            commandLine: [],
            events: [],
            arguments: [],
            returnType: UNIT,
        });
        const control = this.formData.controls;
        control.splice(
            this.logicElements.length - 1,
            0,
            this.initForm({
                funcName: `function${this.logicElements.length - 1}`,
                parameters: [],
                returnType: "void",
            })
        );
    }

    removeFunc(index: number) {
        this.logicElements.splice(index, 1);
        const control = this.formData.controls;
        control.splice(index, 1);
    }

    createItem(func: string, type: string) {
        let curElement: LogicFunction;
        switch (type) {
            case "cl":
                const decisionControl = this.decisionArrayData.controls;
                const control = this.comparisonArrayData.controls;
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
                const formIndex: number = control.length - 1;
                decisionControl.push(
                    this.initDecisionFormArray({
                        expression: control[formIndex] as FormGroup,
                        funcName: func,
                        index: curElement.commandLine.length - 1,
                        evtIndex: -1,
                    })
                );
                curElement.commandLine.push({
                    exec: null,
                    type: { name: "decision", clType: "comparison" },
                    formIndex,
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

    createEvtCl(evt: string, func: string) {
        let curEvt: LogicEvent;
        const control = this.comparisonArrayData.controls;
        curEvt = this.logicElements[0].events.find((ev) => {
            return evt === ev.eventName;
        });
        control.push(
            this.initComparisonFormArray({
                leftExpression: "",
                symbol: "Different",
                rightExpression: "",
                funcName: func,
                index: curEvt.commandLine.length - 1,
                evtIndex: -1,
            })
        );
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

    addClToDecisionBranch(
        branch: string,
        funcName: string,
        isEvt: boolean,
        index: number,
        evtIndex: number,
        cascade: boolean
    ) {
        let decisionFormGroup = this.getDecisionFormGroup(
            index,
            isEvt,
            evtIndex,
            funcName,
            cascade
        );
        let control: AbstractControl[];
        switch (branch) {
            case "trueBranch":
                control = (decisionFormGroup.get("trueBranchAst") as FormArray)
                    .controls;
                console.log(control);
                control.push(
                    this.initTrueBranchAstFormArray({
                        exec: this.initDecisionFormArray({
                            index,
                            evtIndex,
                            funcName,
                            expression: this.initComparisonFormArray({
                                index,
                                evtIndex,
                                funcName,
                                leftExpression: "",
                                rightExpression: "",
                                symbol: "Different",
                            }),
                        }),
                        clTypeName: "decision",
                        clType: "comparison",
                        parentIndex: control.length - 1,
                    })
                );
                console.log(control);
                break;
            case "falseBranch":
                control = (decisionFormGroup.get("falseBranchAst") as FormArray)
                    .controls;
                control.push(
                    this.initFalseBranchAstFormArray({
                        exec: this.initDecisionFormArray({
                            index,
                            evtIndex,
                            funcName,
                            expression: this.initComparisonFormArray({
                                index,
                                evtIndex,
                                funcName,
                                leftExpression: "",
                                rightExpression: "",
                                symbol: "Different",
                            }),
                        }),
                        clTypeName: "decision",
                        clType: "comparison",
                        parentIndex: control.length - 1,
                    })
                );
                break;
        }
    }

    removeClFromDecisionBranch(branch: string, cl: any, clIndex: number) {
        console.log(cl);
        let control: AbstractControl[];
        switch (branch) {
            case "trueBranch":
                control = (cl.get("trueBranchAst") as FormArray).controls;
                control.splice(clIndex, 1);
                break;
            case "falseBranch":
                control = (cl.get("falseBranchAst") as FormArray).controls;
                control.splice(clIndex, 1);
                break;
        }
    }

    addClToRepetition(
        funcName: string,
        isEvt: boolean,
        index: number,
        evtIndex: number,
        cascade: boolean
    ) {
        let decisionFormGroup = this.getRepetitionFormGroup(
            index,
            isEvt,
            evtIndex,
            funcName,
            cascade
        );
        let control: AbstractControl[];
        control = (decisionFormGroup.get("whileAst") as FormArray).controls;
        console.log(control);
        control.push(
            this.initWhileAstFormArray({
                exec: this.initRepetitionFormArray({
                    index,
                    evtIndex,
                    funcName,
                    expression: this.initComparisonFormArray({
                        index,
                        evtIndex,
                        funcName,
                        leftExpression: "",
                        rightExpression: "",
                        symbol: "Different",
                    }),
                }),
                clTypeName: "repetition",
                clType: "comparison",
                parentIndex: control.length - 1,
            })
        );
        console.log(control);
    }

    removeClFromRepetition(cl: any, clIndex: number) {
        console.log(cl);
        let control: AbstractControl[];
        control = (cl.get("whileAst") as FormArray).controls;
        control.splice(clIndex, 1);
    }

    addParameterToFunction(index: number) {
        const paramFormArray = this.formData.controls[index].get(
            "parameters"
        ) as FormArray;
        let control = paramFormArray.controls;
        control.push(
            this.initFuncParams({
                paramName: "",
                paramType: "",
            })
        );

        this.logicElements[index].arguments.push({ name: "", returnType: "" });
    }

    removeParameterFromFunction(funcIndex: number, paramIndex: number) {
        const paramFormArray = this.formData.controls[funcIndex].get(
            "parameters"
        ) as FormArray;
        let control = paramFormArray.controls;
        control.splice(paramIndex, 1);

        this.logicElements[funcIndex].arguments.splice(paramIndex, 1);
    }
    // End Logic interface handle

    // Begin On Change Values
    changeEventType(value: string, index: number) {
        this.logicElements[0].events[index].eventType = value;
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
                this.initParams(formControl as FormControl)
            );
        }
        control
            .get("parametersQuantity")
            .patchValue(funcControl.get("parametersQuantity").value);
    }

    onChangeVarType(
        value: string,
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

        let control = this.declarationArrayData.controls[formIndex];

        if (value === "bool") {
            control.get("varValue").setValue("true");
        } else {
            control.get("varValue").setValue("");
        }
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
        let formIndex: number;
        if (!isEvt) {
            lastValue = curElement.commandLine[index].type.name;
            lastClType = curElement.commandLine[index].type.clType;
            this.removeLastValueFromForm(
                lastValue,
                lastClType,
                curElement.commandLine[index].formIndex
            );
            formIndex = this.getFormIndexOnFunctionCommandLineChange(
                value,
                curElement.commandLine[index].type.clType,
                funcName,
                index,
                isEvt,
                evtIndex
            );
            curElement.commandLine[index].type.name = value;
            curElement.commandLine[index].formIndex = formIndex;
            if (value === "return") {
                let returnQtt = curElement.commandLine.filter((cl) => {
                    return cl.type.name === "return";
                });
                if (returnQtt.length > 1) {
                    this.alert.createErrorDialog(
                        "Erro!",
                        "Mais de um retorno na mesma função!",
                        () => {
                            this.removeLastValueFromForm(
                                "return",
                                "",
                                formIndex
                            );
                            curElement.commandLine.splice(index, 1);
                        }
                    );
                }
            }
        } else {
            lastValue =
                curElement.events[evtIndex].commandLine[index].type.name;
            lastClType =
                curElement.events[evtIndex].commandLine[index].type.clType;
            this.removeLastValueFromForm(
                lastValue,
                lastClType,
                curElement.events[evtIndex].commandLine[index].formIndex
            );
            formIndex = this.getFormIndexOnFunctionCommandLineChange(
                value,
                curElement.commandLine[index].type.clType,
                funcName,
                index,
                isEvt,
                evtIndex
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
        let lastValue: string;
        let lastClType: string;
        let curElement: LogicFunction = this.logicElements.find(
            (element) => funcName === element.funcName
        );
        let formIndex: number = this.getFormIndexOnFunctionCommandLineChange(
            "decision",
            value,
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
            curElement.commandLine[index].type.clType = value;
            curElement.commandLine[index].formIndex = formIndex;
        } else {
            lastValue =
                curElement.events[evtIndex].commandLine[index].type.name;
            lastClType =
                curElement.events[evtIndex].commandLine[index].type.clType;
            this.removeLastValueFromForm(
                lastValue,
                lastClType,
                curElement.events[evtIndex].commandLine[index].formIndex
            );
            curElement.events[evtIndex].commandLine[index].type.clType = value;
            curElement.events[evtIndex].commandLine[
                index
            ].formIndex = formIndex;
        }
    }

    changeWhileLoop(
        value: string,
        funcName: string,
        isEvt: boolean,
        index: number,
        evtIndex: number
    ) {
        let lastValue: string;
        let lastClType: string;
        let curElement: LogicFunction = this.logicElements.find(
            (element) => funcName === element.funcName
        );
        let formIndex: number = this.getFormIndexOnFunctionCommandLineChange(
            "repetition",
            value,
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
            curElement.commandLine[index].type.clType = value;
            curElement.commandLine[index].formIndex = formIndex;
        } else {
            lastValue =
                curElement.events[evtIndex].commandLine[index].type.name;
            lastClType =
                curElement.events[evtIndex].commandLine[index].type.clType;
            this.removeLastValueFromForm(
                lastValue,
                lastClType,
                curElement.events[evtIndex].commandLine[index].formIndex
            );
            curElement.events[evtIndex].commandLine[index].type.clType = value;
            curElement.events[evtIndex].commandLine[
                index
            ].formIndex = formIndex;
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
                control = this.decisionArrayData.controls;
                control.push(
                    this.initDecisionFormArray({
                        index,
                        funcName,
                        evtIndex,
                        expression: this.getClType(
                            clType,
                            funcName,
                            index,
                            isEvt,
                            evtIndex
                        ),
                    })
                );
                break;
            case "repetition":
                control = this.repetitionArrayData.controls;
                control.push(
                    this.initRepetitionFormArray({
                        index,
                        funcName,
                        evtIndex,
                        expression: this.getClType(
                            clType,
                            funcName,
                            index,
                            isEvt,
                            evtIndex
                        ),
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
                        evtIndex: isEvt ? evtIndex : -1,
                    })
                );
                break;
            case "attribution":
                control = this.attributionArrayData.controls;
                control.push(
                    this.initAttributionFormArray({
                        varName: "",
                        attributionValue: "",
                        funcName,
                        index,
                        evtIndex: isEvt ? evtIndex : -1,
                    })
                );
                break;
            case "return":
                control = this.returnArrayData.controls;
                control.push(
                    this.initReturnFormArray({
                        value: "",
                    })
                );
                break;
            default:
                return 0;
        }
        return control.length - 1;
    }

    private getClType(
        clType: string,
        funcName: string,
        index: number,
        isEvt: boolean,
        evtIndex: number
    ): FormGroup {
        let control: AbstractControl[];
        switch (clType) {
            case "comparison":
                control = this.comparisonArrayData.controls;
                control.push(
                    this.initComparisonFormArray({
                        leftExpression: "",
                        symbol: "Different",
                        rightExpression: "",
                        funcName,
                        index,
                        evtIndex: isEvt ? evtIndex : -1,
                    })
                );
                break;
            case "booleanLogic":
                control = this.booleanLogicArrayData.controls;
                control.push(
                    this.initBooleanLogicFormArray({
                        leftExpression: "",
                        symbol: "and",
                        rightExpression: "",
                        funcName,
                        index,
                        evtIndex: isEvt ? evtIndex : -1,
                    })
                );
                break;
            case "custom":
                control = this.customConditionArrayData.controls;
                control.push(
                    this.initCustomConditionFormArray({
                        customCondition: "",
                        funcName,
                        index,
                        evtIndex: isEvt ? evtIndex : -1,
                    })
                );
                break;
        }

        return control[control.length - 1] as FormGroup;
    }

    private removeLastValueFromForm(
        lastValue: string,
        lastClType: string,
        lastFormIndex: number
    ) {
        let control: AbstractControl[];
        switch (lastValue) {
            case "declaration":
                control = this.declarationArrayData.controls;
                break;
            case "decision":
                control = this.decisionArrayData.controls;
                this.removeLastClType(lastClType, lastFormIndex);
                break;
            case "repetition":
                control = this.repetitionArrayData.controls;
                this.removeLastClType(lastClType, lastFormIndex);
                break;
            case "call":
                control = this.callFuncArrayData.controls;
                break;
            case "attribution":
                control = this.attributionArrayData.controls;
                break;
            case "return":
                control = this.returnArrayData.controls;
                break;
            default:
                return;
        }
        control.splice(lastFormIndex, 1);
    }

    private removeLastClType(clType: string, formIndex: number) {
        let control: AbstractControl[];
        switch (clType) {
            case "comparison":
                control = this.comparisonArrayData.controls;
                break;
            case "booleanLogic":
                control = this.booleanLogicArrayData.controls;
                break;
            case "custom":
                control = this.customConditionArrayData.controls;
                break;
        }
        control.splice(formIndex, 1);
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
            return curElement.commandLine[index].type.clType;
        } else {
            return curElement.events[evtIndex].commandLine[index].type.clType;
        }
    }

    whileLoopCondition(
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
    // End On Change Values

    // Check if all forms are valid
    isValid(): boolean {
        return (
            this.logicForm.valid &&
            this.comparisonForm.valid &&
            this.booleanLogicForm.valid &&
            this.customConditionForm.valid &&
            this.declarationForm.valid &&
            this.callFuncForm.valid &&
            this.attributionForm.valid &&
            this.returnForm.valid
        );
    }
}
