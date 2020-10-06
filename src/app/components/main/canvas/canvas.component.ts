import {
    Output,
    Page,
    HtmlTagOut,
    CssOut,
    HtmlOut,
    HtmlTextOut,
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
    CdkDragEnd,
    CdkDragStart,
    CdkDragRelease,
    CdkDragMove,
    CdkDragDrop,
} from "@angular/cdk/drag-drop";
import { logicEvent, logicFunction } from "../../domain/logic-components";
import { FormArray, FormBuilder, FormGroup } from "@angular/forms";
import { AlertService } from "src/app/services/alert/alert.service";
import { BehaviorSubject, Observable } from "rxjs";

declare let $: any;
declare let css: any;

@Component({
    selector: "app-canvas",
    templateUrl: "./canvas.component.html",
    styleUrls: ["./canvas.component.scss"],
})
export class CanvasComponent implements OnInit {
    logicForm: FormGroup;
    isLogicContainer: boolean = false;
    $logicContainer: BehaviorSubject<boolean>;
    public style: object = {};
    componentQtt: number = 1;
    elements: Element[] = [];
    cssObject: CssOut[] = [];
    logicElements: logicFunction[] = [
        {
            funcName: "onReady",
            readonly: false,
            events: [],
            commandLine: [],
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
        this.$logicContainer = new BehaviorSubject<boolean>(false);
    }

    ngOnInit() {
        this.spawnService.getElements().subscribe((element: Element) => {
            element.name = `component${this.elements.length}`;
            this.elements.push(element);
        });

        this.$logicContainer.subscribe((value: boolean) => {
            if (value) {
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
        });

        this.logicForm = this.formBuilder.group({
            commandLines: this.formBuilder.array([]),
        });
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

    drop(event: CdkDragEnd) {
        // console.log(event.source);
        // if (event.previousContainer === event.container) {
        //     moveItemInArray(
        //         event.container.data,
        //         event.previousIndex,
        //         event.currentIndex
        //     );
        // } else {
        //     transferArrayItem(
        //         event.previousContainer.data,
        //         event.container.data,
        //         event.previousIndex,
        //         event.currentIndex
        //     );
        // }
    }

    onDragMoved(event: CdkDragMove) {
        console.log(event);
    }

    onDrop(event: CdkDragDrop<HTMLElement>) {
        console.log(event);
    }

    onDragStart(event: CdkDragStart) {
        // console.log(event);
    }

    onDragRelease(event: CdkDragRelease) {
        // console.log(event);
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

    createComponent(
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
        pageTest.css = this.cssObject;
        let logic = {
            tag: "start",
            arguments: ["x"],
            nextAst: {
                tag: "if",
                nextAst: null,
                trueBranchAst: {
                    tag: "return",
                    expression: "1",
                    metadata: { position: [0, 0] },
                },
                falseBranchAst: {
                    tag: "return",
                    expression: "x * factorial(x - 1)",
                    metadata: { position: [0, 0] },
                },
                expression: "x = 0",
                metadata: { position: [0, 0] },
            },
            name: "factorial",
            returnType: {
                return: { type: "integer" },
                arguments: [{ type: "integer" }],
                type: "function",
            },
            metadata: { position: [0, 0] },
        };
        pageTest.logic.push(logic);
        value.ast.pages.push(pageTest);
        try {
            let projectID = await this.sendService.postCode(value);
            sessionStorage.setItem("projectID", projectID.toString());
            this.toastr.success("Aplicado com sucesso!", "Sucesso!", {
                progressBar: true,
                closeButton: true,
            });
        } catch (e) {
            if (e instanceof HttpErrorResponse) {
                switch (e.status) {
                    case 404:
                        this.toastr.error(
                            `Motivo: ${e.message}`,
                            `Erro ${e.status}`,
                            { progressBar: true, closeButton: true }
                        );
                        break;
                    default:
                        this.toastr.error(
                            `Motivo: ${e.message}`,
                            `Erro ${e.status}`,
                            { progressBar: true, closeButton: true }
                        );
                        break;
                }
            }
        } finally {
            this.spinner.hide("loadingSpinner");
        }
    }

    get formData() {
        return <FormArray>this.logicForm.get("commandLines");
    }

    addAction(component: HTMLElement) {
        this.logicElements.push({
            funcName: "",
            readonly: false,
            commandLine: [],
            events: [],
        });
    }

    createItem(component: HTMLElement, type: string) {
        const id = component.id;
        let func = id.split("-")[0];
        let curElement: logicFunction;
        switch (type) {
            case "cl":
                curElement = this.logicElements.find(
                    (element) => func === element.funcName
                );
                curElement.commandLine.push({ exec: "", type: "" });
                break;
            case "evt":
                if (this.elements.length === 0) {
                    this.alert.createConfirmDialog(
                        "Atenção",
                        "É necessário haver algum componente antes de criar um evento!"
                    );
                } else {
                    curElement = this.logicElements.find(
                        (element) => func === element.funcName
                    );
                    curElement.events.push({ eventName: "", commandLine: [] });
                }

                break;
        }
    }

    createEvtCl(component: HTMLElement) {
        const id = component.id;
        let evt = id.split("-")[0];
        let curEvt: logicEvent;
        let curElement = this.logicElements.find((element) => {
            curEvt = element.events.find((ev) => evt === ev.eventName);
        });
        curEvt.commandLine.push({ exec: "", type: "" });
    }
}
