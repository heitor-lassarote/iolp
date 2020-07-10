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
            name: ["", Validators.required],
            text: "",

            // css
            width: ["", [Validators.required, Validators.min(1)]],
            height: ["", [Validators.required, Validators.min(1)]],
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
        form.get("text").setValue(html.text);

        //css
        form.get("width").setValue(css.width);
        form.get("height").setValue(css.height);
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
    }

    setNewInfo(value: any) {
        let comp = `#${this.originInfo.html.name}`;
        $(comp).text(value["text"]);
        $(comp).css("width", value["width"]);
        $(comp).css("height", value["height"]);
        $(comp).prop("id", value["name"]);
    }
}
