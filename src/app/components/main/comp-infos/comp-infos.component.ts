import { Component, OnInit } from "@angular/core";
import { FormGroup, FormBuilder, Validators } from "@angular/forms";
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
        private showInfosService: ShowComponentInfoService
    ) {}

    ngOnInit() {
        this.initTabs();
        this.infosForm = this.formBuilder.group({
            // html
            type: "",
            name: [
                { value: "", disabled: !this.haveComponent },
                Validators.required,
            ],
            text: { value: "", disabled: !this.haveComponent },

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
        form.get("type").setValue(html.type);
        form.get("name").setValue(html.name);
        this.infosForm.get("name").enable();
        form.get("text").setValue(html.text);
        this.infosForm.get("text").enable();

        //css
        let size: { value: number; unity: string } = this.getSizePart(
            css.width
        );
        form.get("width").setValue(size.value);
        form.get("widthUnity").setValue(size.unity);
        this.infosForm.get("width").enable();
        this.infosForm.get("widthUnity").enable();
        size = this.getSizePart(css.height);
        form.get("height").setValue(size.value);
        form.get("heightUnity").setValue(size.unity);
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

    clearFields() {
        this.infosForm.get("type").setValue("");
        $(".form-control").val("");
    }

    alignText(align: string) {
        let comp = `#${this.originInfo.html.name}`;
        $(comp).css("text-align", align);
        $(comp).css("justify-content", align);
    }

    initTabs() {
        $("#smarttab").smartTab({
            selected: 0,
            theme: "brick",
            orientation: "horizontal",
            enableURLhash: false,
            transition: {
                animation: "slide-horizontal",
                speed: "400",
            },
            keyboardSettings: {
                keyNavigation: false,
            },
        });
    }

    apply() {
        let formValue = this.infosForm.value;
        this.setNewInfo(formValue);
        this.clearFields();
        this.originInfo = null;
        this.haveComponent = false;
        this.infosForm.get("name").disable();
        this.infosForm.get("text").disable();
    }

    setNewInfo(value: any) {
        let comp = `#${this.originInfo.html.name}`;
        $(comp).text(value["text"]);
        $(comp).css("width", value["width"]);
        $(comp).css("height", value["height"]);
        $(comp).prop("id", value["name"]);
    }
}
