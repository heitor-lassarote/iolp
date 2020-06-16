import { Info } from "./../../domain/info";
import { Element } from "src/app/components/domain/element";
import { Component, OnInit } from "@angular/core";
import { ResizeEvent } from "angular-resizable-element";
import { NgxSpinnerService } from "ngx-spinner";
import { SpawnComponentService } from "src/app/services/spawn/spawn-component.service";
import { ShowComponentInfoService } from "src/app/services/show-component-info/show-component-info.service";

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
        private spinner: NgxSpinnerService
    ) {}

    ngOnInit() {
        console.log($("#canvas").offset());
        this.spawnService.getElements().subscribe((element: Element) => {
            this.elements.push(element);
        });
    }

    validate(event: ResizeEvent): boolean {
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

    onResizeEnd(event: ResizeEvent): void {
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
        console.log(css(targ));
        // this.showInfosService.setComponentInfos(infos);
    }
}
