import { Output, Page, HtmlTagOut, CssOut } from "./../../domain/output";
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

declare let $: any;
declare let css: any;

@Component({
    selector: "app-canvas",
    templateUrl: "./canvas.component.html",
    styleUrls: ["./canvas.component.scss"],
})
export class CanvasComponent implements OnInit {
    public style: object = {};
    componentQtt: number = 1;
    elements: Element[] = [];

    constructor(
        private spawnService: SpawnComponentService,
        private showInfosService: ShowComponentInfoService,
        private sendService: SendService,
        private spinner: NgxSpinnerService,
        private toastr: ToastrService
    ) {}

    ngOnInit() {
        this.spawnService.getElements().subscribe((element: Element) => {
            this.elements.push(element);
        });
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
                name: $(comp).prop("id"),
                type: $(comp).prop("tagName").toLowerCase(),
                text: $(comp).text(),
            },
            css: {
                width: parseFloat(
                    $(comp)
                        .css("width")
                        .substring(0, $(comp).css("width").indexOf("p"))
                ),
                height: parseFloat(
                    $(comp)
                        .css("height")
                        .substring(0, $(comp).css("height").indexOf("p"))
                ),
            },
        };
        this.showInfosService.setComponentInfos(infos);
    }

    apply() {
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
        let cssObject: CssOut[] = [];
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
                                    value.includes("drag") ||
                                    value.includes("ng")
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
                            cssObject.push(css);
                            break;
                        default:
                            break;
                    }
                }
                let elementPosition = $(`#${element.id}`).offset();
                let top = elementPosition.top - canvasPosition.top;
                let left = elementPosition.left - canvasPosition.left;
                top = top < 0 ? 0 : top;
                left = left < 0 ? 0 : left;
                htmlObject.attributes.push(["top", top.toString()]);
                htmlObject.attributes.push(["left", left.toString()]);
                if (element.hasChildNodes()) {
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
                        cssObject.push(localCssObj);
                    }
                });
                pageTest.html.push(htmlObject);
                pageTest.css = cssObject;
            });
        value.ast.pages.push(pageTest);
        try {
            this.sendService.postCode(value);
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
}
