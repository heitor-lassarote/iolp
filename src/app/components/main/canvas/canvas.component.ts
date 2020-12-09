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
    Field,
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
import { LOWCODEFUNCTIONS } from "../../constants/low-code-functions.constant";
import { SetComponentService } from "src/app/services/set-component/set-component.service";

declare let $: any;
declare let css: any;

// Constants
const UNIT: Type = new AlgebraicType("Unit");
const consoleType: Type = new RecordType([
    new Field<FunctionType>("log", new FunctionType([new TextType()], UNIT)),
    new Field<FunctionType>("error", new FunctionType([new TextType()], UNIT)),
    new Field<FunctionType>(
        "warning",
        new FunctionType([new TextType()], UNIT)
    ),
]);

@Component({
    templateUrl: "./canvas.component.html",
    styleUrls: ["./canvas.component.scss"],
})
export class CanvasComponent implements OnInit {
    // Forms
    uiForm: FormGroup;
    logicForm: FormGroup;
    decisionForm: FormGroup;
    repetitionForm: FormGroup;
    declarationForm: FormGroup;
    callFuncForm: FormGroup;
    attributionForm: FormGroup;
    htmlElementForm: FormGroup;
    returnForm: FormGroup;
    consoleForm: FormGroup;

    // Attributes
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
        private alert: AlertService,
        private newValueService: SetComponentService
    ) {}

    ngOnInit() {
        this.spawnService.getElements().subscribe((element: Element) => {
            const control = this.uiArrayData.controls;
            element.name = `component${this.elements.length}`;
            element.formIndex = control.length;
            this.elements.push(element);
            console.log(element.position);
            control.push(
                this.initUiForm({
                    name: element.name,
                    type: element.type,
                    width: element.width,
                    height: element.height,
                    position: element.position,
                    selectOptions: element.selectOptions,
                })
            );
        });

        this.newValueService.getNewValue().subscribe((info: Info) => {
            this.setNewValueToUiComponent(info);
        });

        this.createForms();
    }

    private setNewValueToUiComponent(info: Info) {
        const formGroup = (this.uiArrayData.controls[
            info.formIndex
        ] as FormGroup).controls;
        console.log(formGroup);
        console.log(info.css);
        formGroup["type"].patchValue(info.html.type);
        formGroup["name"].patchValue(info.html.name);
        formGroup["width"].patchValue(info.css.width);
        formGroup["height"].patchValue(info.css.height);
        formGroup["selectOptions"].patchValue(info.html.selectOptions);

        let elementIndex: number;
        let element = this.elements.find((el, index) => {
            elementIndex = index;
            return el.formIndex === info.formIndex;
        });
        let newElementValue: Element = {
            formIndex: info.formIndex,
            type: info.html.type,
            name: info.html.name,
            width: info.css.width,
            height: info.css.height,
            position: element.position,
            selectOptions: info.html.selectOptions,
        };
        this.elements[elementIndex] = newElementValue;
    }

    private createForms() {
        this.uiForm = this.formBuilder.group({
            components: this.formBuilder.array([]),
        });

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

        this.declarationForm = this.formBuilder.group({
            declarationArray: this.formBuilder.array([]),
        });

        this.callFuncForm = this.formBuilder.group({
            callFuncArray: this.formBuilder.array([]),
        });

        this.attributionForm = this.formBuilder.group({
            attributionArray: this.formBuilder.array([]),
        });

        this.htmlElementForm = this.formBuilder.group({
            htmlElementArray: this.formBuilder.array([]),
        });

        this.returnForm = this.formBuilder.group({
            returnArray: this.formBuilder.array([]),
        });

        this.consoleForm = this.formBuilder.group({
            consoleArray: this.formBuilder.array([]),
        });
    }

    checkLogicContainerState(event: number) {
        if (event === 1) {
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
        } else if (event === 0) {
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
    get uiArrayData() {
        return <FormArray>this.uiForm.get("components");
    }

    getCssFromForm(formIndex: number): FormArray {
        return <FormArray>this.uiArrayData.controls[formIndex].get("css");
    }

    getSelectOptionsArray(formIndex: number): FormArray {
        return <FormArray>(
            this.uiArrayData.controls[formIndex].get("selectOptions")
        );
    }

    get eventsArrayData() {
        let func: AbstractControl = this.formData.controls[0];
        return <FormArray>func.get("events");
    }

    getEventCommandLines(evtIndex: number): FormArray {
        let evt: AbstractControl = this.eventsArrayData.controls[evtIndex];
        return <FormArray>evt.get("eventCommandLines");
    }

    get formData() {
        return <FormArray>this.logicForm.get("functions");
    }

    getCurFunction(index: number): FormGroup {
        return this.formData.controls[index] as FormGroup;
    }

    getEventsArrayData(index: number): FormArray {
        const curFunction: FormGroup = this.getCurFunction(index);

        return <FormArray>curFunction.get("parameters");
    }

    get decisionArrayData() {
        return <FormArray>this.decisionForm.get("decisionArray");
    }

    getDecisionBranch(
        isEvt: boolean,
        evtIndex: number,
        funcName: string,
        cascade: string,
        branch: string
    ) {
        const decisionFormGroup: FormGroup = this.getInnermostFormGroup(
            isEvt,
            evtIndex,
            funcName,
            cascade
        );

        return <FormArray>decisionFormGroup.get(branch);
    }

    get repetitionArrayData() {
        return <FormArray>this.repetitionForm.get("repetitionArray");
    }

    getWhileAst(
        isEvt: boolean,
        evtIndex: number,
        funcName: string,
        cascade: string
    ) {
        const repetitionFormGroup: FormGroup = this.getInnermostFormGroup(
            isEvt,
            evtIndex,
            funcName,
            cascade
        );

        return <FormArray>repetitionFormGroup.get("whileAst");
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

    get htmlElementArrayData() {
        return <FormArray>this.htmlElementForm.get("htmlElementArray");
    }

    getParamentersArrayData(
        isEvt: boolean,
        evtIndex: number,
        funcName: string,
        cascade: string
    ) {
        let parameterFormGroup: FormGroup = this.getInnermostFormGroup(
            isEvt,
            evtIndex,
            funcName,
            cascade
        );

        return <FormArray>parameterFormGroup.get("parameters");
    }

    get returnArrayData() {
        return <FormArray>this.returnForm.get("returnArray");
    }

    get consoleArrayData() {
        return <FormArray>this.consoleForm.get("consoleArray");
    }

    getFunctionParams(funcIndex: number) {
        return <FormArray>this.formData.controls[funcIndex].get("parameters");
    }
    // End Get Form Array Datas

    // Begin Create Array Data
    private initUiForm(ui: {
        type: string;
        name: string;
        width: string;
        height: string;
        position: {
            x: number;
            y: number;
        };
        selectOptions?: string[];
    }): FormGroup {
        return this.formBuilder.group({
            type: ui.type,
            name: ui.name,
            width: ui.width,
            height: ui.height,
            positionX: ui.position.x.toString() + "px",
            positionY: ui.position.y.toString() + "px",
            selectOptions: this.formBuilder.array(
                ui.selectOptions ? [] : ui.selectOptions
            ),
            css: this.formBuilder.array([]),
        });
    }

    private initForm(func: {
        funcName: string;
        parameters: { paramName: string; paramValue: any; paramType: Type }[];
        returnType: string | Type;
    }): FormGroup {
        return this.formBuilder.group({
            funcName: [func.funcName, [Validators.required]],
            parameters: this.formBuilder.array(func.parameters),
            events: this.formBuilder.array([]),
            returnType: func.returnType,
        });
    }

    private initEvent(evt: {
        eventName: string;
        eventType: string;
    }): FormGroup {
        return this.formBuilder.group({
            eventName: evt.eventName,
            eventType: evt.eventType,
            eventCommandLines: this.formBuilder.array([]),
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
        conditionType: string;
    }): FormGroup {
        return this.formBuilder.group({
            expression: decision.expression,
            trueBranchAst: this.formBuilder.array([]),
            falseBranchAst: this.formBuilder.array([]),
            conditionType: decision.conditionType,
            index: decision.index,
            funcName: decision.funcName,
            evtIndex: decision.evtIndex,
        });
    }

    private initRepetitionFormArray(repetition: {
        index: number;
        funcName: string;
        evtIndex: number;
        expression: AbstractControl;
        conditionType: string;
    }): FormGroup {
        return this.formBuilder.group({
            expression: repetition.expression,
            whileAst: this.formBuilder.array([]),
            conditionType: repetition.conditionType,
            index: repetition.index,
            funcName: repetition.funcName,
            evtIndex: repetition.evtIndex,
        });
    }

    private initBranchAstFormArray(branch: {
        exec: FormGroup;
        index: number;
        clType: string;
        clTypeName: string;
        originFormIndex: number;
    }): FormGroup {
        return this.formBuilder.group({
            exec: branch.exec,
            index: branch.index,
            originFormIndex: branch.originFormIndex,
            clType: branch.clType,
            clTypeName: branch.clTypeName,
        });
    }

    private initComparisonFormArray(comparison: {
        conditionType: string;
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
            conditionType: comparison.conditionType,
            index: comparison.index,
            funcName: comparison.funcName,
            evtIndex: comparison.evtIndex,
        });
    }

    private initBooleanLogicFormArray(booleanLogic: {
        conditionType: string;
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
            conditionType: booleanLogic.conditionType,
            index: booleanLogic.index,
            funcName: booleanLogic.funcName,
            evtIndex: booleanLogic.evtIndex,
        });
    }

    private initCustomConditionFormArray(custom: {
        conditionType: string;
        index: number;
        funcName: string;
        evtIndex: number;
        customCondition: string;
    }) {
        return this.formBuilder.group({
            customCondition: [custom.customCondition, Validators.required],
            conditionType: custom.conditionType,
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

    private initHtmlElementFormArray(htmlElement: {
        index: number;
        funcName: string;
        evtIndex: number;
        elementName: string;
        elementData: string;
        elementValue: string;
    }): FormGroup {
        return this.formBuilder.group({
            elementName: htmlElement.elementName,
            elementData: htmlElement.elementData,
            elementValue: [htmlElement.elementValue, Validators.required],
            index: htmlElement.index,
            funcName: htmlElement.funcName,
            evtIndex: htmlElement.evtIndex,
        });
    }

    private initReturnFormArray(returnValue: {
        value: any;
        type: string;
    }): FormGroup {
        return this.formBuilder.group({
            returnValue: [returnValue.value, Validators.required],
            returnType: returnValue.type,
        });
    }

    private initConsoleFormArray(consoleValue: {
        index: number;
        funcName: string;
        evtIndex: number;
        consoleType: string;
        consoleText: string;
    }): FormGroup {
        return this.formBuilder.group({
            consoleType: consoleValue.consoleType,
            consoleText: [consoleValue.consoleText, Validators.required],
            index: consoleValue.index,
            funcName: consoleValue.funcName,
            evtIndex: consoleValue.evtIndex,
        });
    }
    // End Create Array Data

    // Get Form Group
    getInnermostFormGroup(
        isEvt: boolean,
        evtIndex: number,
        funcName: string,
        cascade: string
    ) {
        let curElement: LogicFunction = this.logicElements.find(
            (element) => funcName === element.funcName
        );

        let branchName: string;
        let stack: string[] = cascade.split(",");
        let branches: number[];
        let head: FormGroup;

        for (let i = 0; i < stack.length; i++) {
            let element: string = stack[i];
            branches = element.split(":").map((value) => parseInt(value));

            if (i === 0) {
                let formIndex: number = !isEvt
                    ? curElement.commandLine[branches[0]].formIndex
                    : this.getEventCommandLines(evtIndex).controls[
                          branches[0]
                      ].get("originFormIndex").value;

                const formArray: AbstractControl[] = this.checkFormControlType(
                    !isEvt
                        ? curElement.commandLine[branches[0]].type.name
                        : this.getEventCommandLines(evtIndex).controls[
                              branches[0]
                          ].get("clTypeName").value
                );

                head = formArray[formIndex] as FormGroup;
            }

            if (branches[1] !== undefined) {
                switch (branches[1]) {
                    case 0:
                        branchName = "trueBranchAst";
                        break;
                    case 1:
                        branchName = "falseBranchAst";
                        break;
                    case 2:
                        branchName = "whileAst";
                        break;
                }
                head = (head.get(branchName) as FormArray).controls[
                    parseInt(stack[i + 1].split(":")[0])
                ].get("exec") as FormGroup;
            } else if (i !== 0) {
                const oldHead = head;
                let parent: FormGroup = head.parent as FormGroup;
                head = this.checkFormControlType(
                    parent.get("clTypeName").value
                )[parent.get("originFormIndex").value] as FormGroup;
                if (!head) {
                    head = oldHead;
                }
            }
        }

        return head;
    }

    getReturnFormGroup(
        isEvt: boolean,
        evtIndex: number,
        funcName: string,
        cascade: string,
        type: string
    ): FormGroup {
        const formGroup = this.getInnermostFormGroup(
            isEvt,
            evtIndex,
            funcName,
            cascade
        );

        formGroup.get("returnType").setValue(type);

        return formGroup;
    }

    getConditionFormGroup(
        isEvt: boolean,
        evtIndex: number,
        funcName: string,
        cascade: string
    ): FormGroup {
        const formGroup: FormGroup = this.getInnermostFormGroup(
            isEvt,
            evtIndex,
            funcName,
            cascade
        );

        return formGroup.get("expression") as FormGroup;
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
            formIndex: this.elements.find((el) => (el.name = comp)).formIndex,
            html: {
                name: $(comp).prop("id") !== null ? $(comp).prop("id") : "",
                type:
                    $(comp).prop("tagName") !== null
                        ? $(comp).prop("tagName").toLowerCase()
                        : "",
                text:
                    $(comp).text() !== null
                        ? $(comp).prop("tagName").toLowerCase() === "select"
                            ? ""
                            : $(comp).text()
                        : "",
                selectOptions:
                    $(comp).prop("tagName").toLowerCase() === "select"
                        ? this.getSelectOptions(comp)
                        : [],
            },
            css: {
                width: $(comp).css("width"),
                height: $(comp).css("height"),
                alignText: $(comp).css("text-align"),
                justifyContent: $(comp).css("justify-content"),
            },
        };
        this.showInfosService.setComponentInfos(infos);
    }

    private getSelectOptions(component: string): string[] {
        let selectOptions: string[] = [];
        [...$(component).children()].forEach((opt) => {
            console.log(opt);
            selectOptions.push(opt.value);
        });

        return selectOptions;
    }

    // Create an event to component
    onDoubleClick(ev: MouseEvent, targ: HTMLElement) {
        const id = targ.id;

        this.eventsArrayData.controls.push(
            this.initEvent({
                eventName: id,
                eventType: "click",
            })
        );

        this.logicElements[0].events.push({
            commandLine: [],
            eventName: id,
            eventType: "click",
        });

        // this.isLogicContainer = true;
        // this.setLogicContainer(true);
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
        let externs: Map<string, Type> = new Map<string, Type>([
            ["console", consoleType],
        ]);
        let externsValue: Object = {};
        for (let [key, value] of externs) {
            externsValue = { [key]: value, ...externsValue };
        }
        let logicObject: Module = {
            adtTemplates: new Map(),
            externs: externsValue,
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
            if (typeof projectID === "number")
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
                let controls = this.checkFormControlType(cl.type.name);
                let expression: Expression;
                let curControl = controls[cl.formIndex];
                expression = this.getExpressionType(
                    cl.type.name,
                    curControl as FormGroup
                );
                ast = this.getAstType(cl.type.name, expression, curControl);
                funct.body.push(ast);
            });
            //Events
            if (funcIndex === 0) {
                this.eventsArrayData.controls.forEach(
                    (event: FormGroup, evtIndex: number) => {
                        let evtClControls = this.getEventCommandLines(evtIndex)
                            .controls;
                        let evtFunction: Function = {
                            name: `${event.get("eventName").value}${
                                event.get("eventType").value
                            }Event`,
                            type: UNIT,
                            arguments: [],
                            body: [],
                        };
                        evtClControls.forEach((cl: FormGroup) => {
                            let controls = this.checkFormControlType(
                                cl.get("clTypeName").value
                            );
                            let expression: Expression;
                            let curControl =
                                controls[cl.get("originFormIndex").value];
                            expression = this.getExpressionType(
                                cl.get("clTypeName").value,
                                curControl as FormGroup
                            );
                            ast = this.getAstType(
                                cl.get("clTypeName").value,
                                expression,
                                curControl
                            );
                            evtFunction.body.push(ast);
                        });
                        functions.push(evtFunction);
                        let eventAst: AST = new Expression_(
                            new Call(new Variable("event"), [
                                new Text(event.get("eventName").value),
                                new Text(event.get("eventType").value),
                                new Call(new Variable(evtFunction.name), []),
                            ])
                        );
                        funct.body.push(eventAst);
                    }
                );
            }

            console.log(JSON.stringify(funct));
            functions.push(funct);
        });

        return functions;
    }

    private checkFormControlType(clName: string): AbstractControl[] {
        switch (clName) {
            case "decision":
                return this.decisionArrayData.controls;
            case "repetition":
                return this.repetitionArrayData.controls;
            case "declaration":
                return this.declarationArrayData.controls;
            case "call":
                return this.callFuncArrayData.controls;
            case "attribution":
                return this.attributionArrayData.controls;
            case "htmlElement":
                return this.htmlElementArrayData.controls;
            case "return":
                return this.returnArrayData.controls;
            case "console":
                return this.consoleArrayData.controls;
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
                newAst = new If(
                    expression,
                    this.createBranchToAst(
                        control.get("trueBranchAst") as FormArray
                    ),
                    this.createBranchToAst(
                        control.get("falseBranchAst") as FormArray
                    )
                );
                break;
            case "repetition":
                newAst = new While(
                    expression,
                    this.createBranchToAst(control.get("whileAst") as FormArray)
                );
                break;
            case "declaration":
                newAst = new Var(
                    control.get("varName").value,
                    this.getType(control.get("varType").value, [], null),
                    expression
                );
                break;
            case "call":
                switch (control.get("returnType").value) {
                    case "with-return":
                        newAst = new Assign(
                            new Variable(control.get("varName").value),
                            expression
                        );
                        break;
                    case "without-return":
                        newAst = new Expression_(expression);
                        break;
                }
                break;
            case "attribution":
                newAst = new Assign(
                    new Variable(control.get("varName").value.split("-")[0]),
                    expression
                );
                break;
            case "htmlElement":
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
            case "console":
                newAst = new Expression_(
                    (expression as StringExpression).value
                );
                break;
            default:
                newAst = null;
                break;
        }
        return newAst;
    }

    private createBranchToAst(branchArray: FormArray): AST[] {
        const controls: AbstractControl[] = branchArray.controls;
        let branchAst: AST[] = [];
        let formGroup: FormGroup;
        let expression: Expression;
        let ast: AST;

        controls.forEach((cl: FormGroup) => {
            formGroup = cl.get("exec") as FormGroup;
            expression = this.getExpressionType(
                cl.get("clTypeName").value,
                formGroup
            );
            ast = this.getAstType(
                cl.get("clTypeName").value,
                expression,
                formGroup
            );
            branchAst.push(ast);
        });

        return branchAst;
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

    private getExpressionType(clName: string, control: FormGroup): Expression {
        let newExpression: Expression;

        switch (clName) {
            case "decision":
                let decisionExp: FormGroup = control.get("expression").value;
                switch (decisionExp["conditionType"]) {
                    case "comparison":
                    case "booleanLogic":
                        newExpression = new BinaryOp(
                            decisionExp["leftExpression"],
                            decisionExp["symbol"],
                            decisionExp["rightExpression"]
                        );
                        break;
                    case "custom":
                        newExpression = new StringExpression(
                            decisionExp["customCondition"]
                        );
                        break;
                    default:
                        newExpression = null;
                        break;
                }
                break;
            case "repetition":
                let repetitionExp: FormGroup = control.get("expression").value;
                switch (repetitionExp["conditionType"]) {
                    case "comparison":
                    case "booleanLogic":
                        newExpression = new BinaryOp(
                            repetitionExp["leftExpression"],
                            repetitionExp["symbol"],
                            repetitionExp["rightExpression"]
                        );
                        break;
                    case "custom":
                        newExpression = new StringExpression(
                            repetitionExp["customCondition"]
                        );
                        break;
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
                newExpression = new Call(
                    new Variable(control.get("function").value),
                    args
                );
                break;
            case "attribution":
                newExpression = new Literal_(
                    this.getLiteralByVarType(
                        control.get("varName").value.split("-")[1],
                        control.get("attributionValue").value
                    )
                );
                break;
            case "htmlElement":
                newExpression = new Call(new Variable("handleHtmlElement"), [
                    new Literal_(new Text(control.get("elementName").value)),
                    new Literal_(
                        new Text(
                            this.getHtmlElementDataType(
                                control.get("elementData").value,
                                control.get("elementValue").value
                            )
                        )
                    ),
                ]);
                break;
            case "return":
                const returnValue: string = control
                    .get("returnValue")
                    .value.trim();
                const returnType: Type = this.getType(
                    control.get("returnType").value,
                    [],
                    null
                );

                if (returnValue !== "") {
                    let returnString: string =
                        returnType instanceof TextType ||
                        returnType instanceof CharType
                            ? `#"${returnValue}"`
                            : returnValue;
                    newExpression = new StringExpression(returnString);
                } else {
                    newExpression = null;
                }

                break;
            case "console":
                newExpression = new StringExpression(
                    `console.${control.get("consoleType").value}(#"${
                        control.get("consoleText").value
                    }")`
                );
                break;
            default:
                newExpression = null;
                break;
        }

        return newExpression;
    }

    private getHtmlElementDataType(data: string, value: string): string {
        switch (data) {
            case "id":
                return `attr("id", "${value}")`;
            case "addClass":
                return `addClass("${value}")`;
            case "removeClass":
                return `removeClass("${value}")`;
            case "text":
                return `text("${value}")`;
            case "value":
                return `val("${value}")`;
        }
    }

    private getValueByVarType(varType: string, value: string): number | string {
        let newValue: number | string;
        switch (varType) {
            case "integer":
                newValue = parseInt(value === "" ? "0" : value, 10);
                break;
            case "double":
                newValue = parseFloat(value === "" ? "0.0" : value);
                break;
            default:
                newValue = value;
                break;
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
                newLiteral = new Double(parseFloat(value));
                break;
            case "integer":
                newLiteral = new Integer(parseInt(value, 10));
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
                curElement = this.logicElements.find(
                    (element) => func === element.funcName
                );
                decisionControl.push(
                    this.initDecisionFormArray({
                        expression: this.initComparisonFormArray({
                            leftExpression: "",
                            symbol: "Different",
                            rightExpression: "",
                            conditionType: "comparison",
                            funcName: func,
                            index: curElement.commandLine.length - 1,
                            evtIndex: -1,
                        }),
                        funcName: func,
                        index: curElement.commandLine.length - 1,
                        evtIndex: -1,
                        conditionType: "comparison",
                    })
                );
                curElement.commandLine.push({
                    exec: null,
                    type: { name: "decision", clType: "comparison" },
                    formIndex: decisionControl.length - 1,
                });
                break;
            case "evt":
                if (this.elements.length === 0) {
                    this.alert.createConfirmDialog(
                        "Atenção!",
                        "É necessário haver algum componente antes de criar um evento!"
                    );
                } else {
                    const control: AbstractControl[] = this.eventsArrayData
                        .controls;
                    curElement = this.logicElements[0];
                    control.push(
                        this.initEvent({
                            eventName: "",
                            eventType: "click",
                        })
                    );
                    curElement.events.push({
                        eventName: "",
                        commandLine: [],
                        eventType: "click",
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
                const clTypeName: string =
                    curElement.commandLine[index].type.name;
                const formIndex: number =
                    curElement.commandLine[index].formIndex;
                this.checkFormControlType(clTypeName).splice(formIndex, 1);
                curElement.commandLine.splice(index, 1);
                break;
            case "evt":
                const control: AbstractControl[] = this.eventsArrayData
                    .controls;
                curElement = this.logicElements.find(
                    (element) => func === element.funcName
                );
                curElement.events.splice(index, 1);
                control.splice(index, 1);
                break;
        }
    }

    createEvtCl(func: string, evtIndex: number) {
        let curEvt: LogicEvent;
        const eventControl = this.getEventCommandLines(evtIndex).controls;
        curEvt = this.logicElements[0].events[evtIndex];

        const clIndex: number = eventControl.length;
        const formIndex: number = this.checkFormControlType("decision").length;

        const execFormGroup: FormGroup = this.setNewValueToCl(
            "decision",
            "comparison",
            func,
            0,
            true,
            evtIndex
        );

        eventControl.push(
            this.initBranchAstFormArray({
                exec: execFormGroup,
                clTypeName: "decision",
                clType: "comparison",
                index: clIndex,
                originFormIndex: formIndex,
            })
        );

        curEvt.commandLine.push({
            exec: execFormGroup,
            type: { name: "decision", clType: "comparison" },
            formIndex: formIndex,
        });
    }

    removeEvtCl(evtName: string, index: number, evtIndex: number) {
        let evtCls: AbstractControl[] = this.getEventCommandLines(evtIndex)
            .controls;
        let curEvt: LogicEvent = this.logicElements[0].events[evtIndex];
        const clTypeName: string = evtCls[index].get("clTypeName").value;
        const formIndex: number = evtCls[index].get("originFormIndex").value;
        this.checkFormControlType(clTypeName).splice(formIndex, 1);
        evtCls.splice(index, 1);
        curEvt.commandLine.splice(index, 1);
    }

    addClToDecisionBranch(
        branch: string,
        funcName: string,
        isEvt: boolean,
        index: number,
        evtIndex: number,
        cascade: string
    ) {
        const control: AbstractControl[] = this.getDecisionBranch(
            isEvt,
            evtIndex,
            funcName,
            cascade,
            branch
        ).controls;
        const clIndex: number = control.length;
        const formIndex: number = this.checkFormControlType("decision").length;

        const execFormGroup: FormGroup = this.setNewValueToCl(
            "decision",
            "comparison",
            funcName,
            index,
            isEvt,
            evtIndex
        );

        control.push(
            this.initBranchAstFormArray({
                exec: execFormGroup,
                clTypeName: "decision",
                clType: "comparison",
                index: clIndex,
                originFormIndex: formIndex,
            })
        );
    }

    removeClFromDecisionBranch(
        clIndex: number,
        branch: string,
        funcName: string,
        isEvt: boolean,
        evtIndex: number,
        cascade: string,
        clTypeName: string,
        formIndex: number
    ) {
        let control = this.getDecisionBranch(
            isEvt,
            evtIndex,
            funcName,
            cascade,
            branch
        ).controls;

        this.checkFormControlType(clTypeName).splice(formIndex, 1);

        control.splice(clIndex, 1);
    }

    addClToRepetition(
        funcName: string,
        isEvt: boolean,
        index: number,
        evtIndex: number,
        cascade: string
    ) {
        const control: AbstractControl[] = this.getWhileAst(
            isEvt,
            evtIndex,
            funcName,
            cascade
        ).controls;
        const clIndex: number = control.length;
        const formIndex: number = this.checkFormControlType("decision").length;

        const execFormGroup: FormGroup = this.setNewValueToCl(
            "decision",
            "comparison",
            funcName,
            index,
            isEvt,
            evtIndex
        );

        control.push(
            this.initBranchAstFormArray({
                exec: execFormGroup,
                clTypeName: "decision",
                clType: "comparison",
                index: clIndex,
                originFormIndex: formIndex,
            })
        );
    }

    removeClFromRepetition(
        clIndex: number,
        funcName: string,
        isEvt: boolean,
        evtIndex: number,
        cascade: string,
        clTypeName: string,
        formIndex: number
    ) {
        let control = this.getWhileAst(isEvt, evtIndex, funcName, cascade)
            .controls;

        this.checkFormControlType(clTypeName).splice(formIndex, 1);

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

    onChangeCallFunction(
        func: string,
        isEvt: boolean,
        evtIndex: number,
        funcName: string,
        cascade: string
    ) {
        const formGroup: FormGroup = this.getInnermostFormGroup(
            isEvt,
            evtIndex,
            funcName,
            cascade
        );

        const funcControl = this.formData.controls.find(
            (ctr) => ctr.get("funcName").value === func
        );

        const funcParams = funcControl.get("parameters") as FormArray;
        const callParams = formGroup.get("parameters") as FormArray;

        callParams.clear();

        for (let formControl of funcParams.controls) {
            callParams.controls.push(
                this.initParams(formControl as FormControl)
            );
        }
    }

    clTypeChange(
        value: string,
        isEvt: boolean,
        funcName: string,
        index: number,
        evtIndex: number,
        cl: FormGroup
    ) {
        let lastValue: string;
        let formIndex: number;

        if (cl !== null) {
            //Corpo if/else/while
            lastValue = cl.get("clTypeName").value;
            this.removeLastValueFromForm(
                lastValue,
                cl.get("originFormIndex").value
            );

            cl.removeControl("exec");
            cl.addControl(
                "exec",
                this.setNewValueToCl(
                    value,
                    cl.get("clType").value,
                    funcName,
                    index,
                    isEvt,
                    evtIndex
                )
            );
            cl.get("clTypeName").setValue(value);
            cl.get("originFormIndex").setValue(
                this.checkFormControlType(value).length - 1
            );
        } else {
            //Raíz
            let curElement: LogicFunction = this.logicElements.find(
                (element) => funcName === element.funcName
            );

            if (!isEvt) {
                lastValue = curElement.commandLine[index].type.name;
                this.removeLastValueFromForm(
                    lastValue,
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
                this.removeLastValueFromForm(
                    lastValue,
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
                curElement.events[evtIndex].commandLine[
                    index
                ].type.name = value;
                curElement.events[evtIndex].commandLine[
                    index
                ].formIndex = formIndex;
            }
        }
    }

    changeIfCondition(
        value: string,
        funcName: string,
        isEvt: boolean,
        index: number,
        evtIndex: number,
        cascade: string
    ) {
        const formGroup: FormGroup = this.getInnermostFormGroup(
            isEvt,
            evtIndex,
            funcName,
            cascade
        );

        formGroup.removeControl("expression");
        formGroup.addControl(
            "expression",
            this.getClType(value, funcName, index, isEvt, evtIndex)
        );
        formGroup.get("conditionType").setValue(value);
    }

    changeWhileLoop(
        value: string,
        funcName: string,
        isEvt: boolean,
        index: number,
        evtIndex: number,
        cascade: string
    ) {
        const formGroup: FormGroup = this.getInnermostFormGroup(
            isEvt,
            evtIndex,
            funcName,
            cascade
        );

        formGroup.removeControl("expression");
        formGroup.addControl(
            "expression",
            this.getClType(value, funcName, index, isEvt, evtIndex)
        );
        formGroup.get("conditionType").setValue(value);
    }

    private setNewValueToCl(
        value: string,
        clType: string,
        funcName: string,
        index: number,
        isEvt: boolean,
        evtIndex: number
    ): FormGroup {
        let control: AbstractControl[] = this.checkFormControlType(value);
        let formGroup: FormGroup;
        switch (value) {
            case "declaration":
                formGroup = this.initDeclarationFormArray({
                    varType: "adt",
                    varName: "",
                    varValue: "",
                    funcName,
                    index,
                    evtIndex: isEvt ? evtIndex : -1,
                });
                break;
            case "decision":
                formGroup = this.initDecisionFormArray({
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
                    conditionType: clType,
                });
                break;
            case "repetition":
                formGroup = this.initRepetitionFormArray({
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
                    conditionType: clType,
                });
                break;
            case "call":
                formGroup = this.initCallFuncFormArray({
                    returnType: "with-return",
                    varName: "",
                    function: "",
                    funcName,
                    index,
                    evtIndex: isEvt ? evtIndex : -1,
                });
                break;
            case "attribution":
                formGroup = this.initAttributionFormArray({
                    varName: "",
                    attributionValue: "",
                    funcName,
                    index,
                    evtIndex: isEvt ? evtIndex : -1,
                });
                break;
            case "htmlElement":
                formGroup = this.initHtmlElementFormArray({
                    elementName: "",
                    elementData: "",
                    elementValue: "",
                    funcName,
                    index,
                    evtIndex: isEvt ? evtIndex : -1,
                });
                break;
            case "return":
                formGroup = this.initReturnFormArray({
                    value: "",
                    type: "",
                });
                break;
            case "console":
                formGroup = this.initConsoleFormArray({
                    consoleType: "log",
                    consoleText: "",
                    funcName,
                    index,
                    evtIndex: isEvt ? evtIndex : -1,
                });
                break;
            default:
                formGroup = null;
                break;
        }

        control.push(formGroup);
        return formGroup;
    }

    private getFormIndexOnFunctionCommandLineChange(
        value: string,
        clType: string,
        funcName: string,
        index: number,
        isEvt: boolean,
        evtIndex: number
    ): number {
        let control: AbstractControl[] = this.checkFormControlType(value);
        this.setNewValueToCl(value, clType, funcName, index, isEvt, evtIndex);
        return control.length - 1;
    }

    private getClType(
        clType: string,
        funcName: string,
        index: number,
        isEvt: boolean,
        evtIndex: number
    ): FormGroup {
        let formGroup: FormGroup;
        switch (clType) {
            case "comparison":
                formGroup = this.initComparisonFormArray({
                    leftExpression: "",
                    symbol: "Different",
                    rightExpression: "",
                    conditionType: clType,
                    funcName,
                    index,
                    evtIndex: isEvt ? evtIndex : -1,
                });
                break;
            case "booleanLogic":
                formGroup = this.initBooleanLogicFormArray({
                    leftExpression: "",
                    symbol: "And",
                    rightExpression: "",
                    conditionType: clType,
                    funcName,
                    index,
                    evtIndex: isEvt ? evtIndex : -1,
                });
                break;
            case "custom":
                formGroup = this.initCustomConditionFormArray({
                    customCondition: "",
                    conditionType: clType,
                    funcName,
                    index,
                    evtIndex: isEvt ? evtIndex : -1,
                });
                break;
        }

        return formGroup;
    }

    private removeLastValueFromForm(lastValue: string, lastFormIndex: number) {
        let control: AbstractControl[] = this.checkFormControlType(lastValue);
        control.splice(lastFormIndex, 1);
    }
    // End On Change Values

    // Check if all forms are valid
    isValid(): boolean {
        return (
            this.logicForm.valid &&
            this.declarationForm.valid &&
            this.callFuncForm.valid &&
            this.attributionForm.valid &&
            this.htmlElementForm.valid &&
            this.returnForm.valid
        );
    }
}
