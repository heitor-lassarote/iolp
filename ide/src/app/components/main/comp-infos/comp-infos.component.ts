import { Component, OnInit } from "@angular/core";
import { FormGroup, FormBuilder, Validators } from "@angular/forms";
import { SetComponentService } from "src/app/services/set-component/set-component.service";
import { ShowComponentInfoService } from "src/app/services/show-component-info/show-component-info.service";
import { Info } from "../../domain/info";

declare let $: any;

@Component({
    selector: "app-comp-infos",
    templateUrl: "./comp-infos.component.html",
    styleUrls: ["./comp-infos.component.scss"],
})
export class CompInfosComponent implements OnInit {
    infosForm: FormGroup;
    originInfo: Info = null;
    haveComponent: boolean = false;

    constructor(
        private formBuilder: FormBuilder,
        private showInfosService: ShowComponentInfoService,
        private newValueService: SetComponentService
    ) {}

    ngOnInit() {
        this.infosForm = this.formBuilder.group({
            // html
            type: "",
            name: [
                { value: "", disabled: !this.haveComponent },
                Validators.required,
            ],
            text: { value: "", disabled: !this.haveComponent },
            selectOptions: { value: "", disabled: !this.haveComponent },
            imgSrc: { value: "", disabled: !this.haveComponent },

            // css
            width: [
                { value: "", disabled: !this.haveComponent },
                [Validators.required, Validators.min(1)],
            ],
            widthUnity: { value: "", disabled: !this.haveComponent },
            height: [
                { value: "", disabled: !this.haveComponent },
                [Validators.required, Validators.min(1)],
            ],
            heightUnity: { value: "", disabled: !this.haveComponent },
            textAlign: "",
        });
        this.showInfosService.getComponentInfos().subscribe((info: Info) => {
            this.originInfo = info;
            this.writeForm(info);
            this.haveComponent = true;
        });
    }

    writeForm(info: Info) {
        let form = this.infosForm;
        let html = info.html;
        let css = info.css;

        // html
        form.get("type").setValue(html.type.trim());
        form.get("name").setValue(html.name.trim());
        this.infosForm.get("name").enable();
        form.get("text").setValue(html.text.trim());
        if (html.type.trim() !== "select" && html.type.trim() !== "img") {
            this.infosForm.get("text").enable();
        } else if (html.type.trim() === "select") {
            form.get("selectOptions").setValue(
                this.buildSelectOptionsString(html.selectOptions)
            );
            this.infosForm.get("selectOptions").enable();
        } else {
            form.get("imgSrc").setValue(html.imgSrc);
            this.infosForm.get("imgSrc").enable();
        }

        //css
        let size: { value: number; unity: string } = this.getSizePart(
            css.width.trim()
        );
        form.get("width").setValue(size.value);
        form.get("widthUnity").setValue(size.unity.trim());
        this.infosForm.get("width").enable();
        this.infosForm.get("widthUnity").enable();
        size = this.getSizePart(css.height.trim());
        form.get("height").setValue(size.value);
        form.get("heightUnity").setValue(size.unity.trim());
        this.infosForm.get("height").enable();
        this.infosForm.get("heightUnity").enable();
    }

    getSizePart(str: string): { value: number; unity: string } {
        let strArray = str.split("");
        let number = "";
        let unity = "";
        strArray.forEach((letter, i) => {
            if (!Number.isNaN(parseInt(letter)) || letter === ".")
                number += letter;
            else unity += letter;
        });

        return {
            value: parseFloat(number),
            unity: unity,
        };
    }

    alignText(align: string) {
        this.infosForm.get("textAlign").setValue(align);
    }

    apply() {
        let formValue = this.infosForm.getRawValue();
        this.setNewInfo(formValue);
        this.originInfo = null;
        this.haveComponent = false;
        this.infosForm.get("name").disable();
        this.infosForm.get("text").disable();
        this.infosForm.get("selectOptions").disable();
        this.infosForm.get("width").disable();
        this.infosForm.get("widthUnity").disable();
        this.infosForm.get("height").disable();
        this.infosForm.get("heightUnity").disable();
        this.infosForm.reset();
    }

    setNewInfo(value: any) {
        let newValue: Info = {
            formIndex: this.originInfo.formIndex,
            html: {
                name: value["name"],
                text: value["text"],
                type: value["type"],
                selectOptions:
                    value["type"] === "select"
                        ? this.getSelectOptionsArray(value["selectOptions"])
                        : [],
                imgSrc: value["type"] === "img" ? value["imgSrc"] : "",
            },
            css: {
                height:
                    value["height"].toString() +
                    value["heightUnity"].toString(),
                width:
                    value["width"].toString() + value["widthUnity"].toString(),
                alignText:
                    value["textAlign"] === null ? "" : value["textAlign"],
                justifyContent:
                    value["textAlign"] === null ? "" : value["textAlign"],
            },
        };
        this.newValueService.setNewValue(newValue);
    }

    buildSelectOptionsString(selectOptions: string[]): string {
        return selectOptions.join(",");
    }

    getSelectOptionsArray(selectOptions: string): string[] {
        return selectOptions.split(",");
    }
}
