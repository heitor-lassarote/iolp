import { FormGroup, FormBuilder, Validators } from "@angular/forms";
import { Component, OnInit } from "@angular/core";
import { Router } from "@angular/router";
import { Element } from "src/app/components/domain/element";
import { SpawnComponentService } from "src/app/services/spawn/spawn-component.service";
import { SendService } from "src/app/services/send/send.service";
import { NgxSpinnerService } from "ngx-spinner";
import { AlertService } from "src/app/services/alert/alert.service";
import { HttpErrorResponse } from "@angular/common/http";
import { ToastrService } from "ngx-toastr";

declare let $: any;

@Component({
    selector: "app-side-navigation",
    templateUrl: "./side-navigation.component.html",
    styleUrls: ["./side-navigation.component.scss"],
})
export class SideNavigationComponent implements OnInit {
    username: string = "";
    projectDetailForm: FormGroup;

    constructor(
        private router: Router,
        private spawnService: SpawnComponentService,
        private formBuilder: FormBuilder,
        private sendService: SendService,
        private spinner: NgxSpinnerService,
        private alertService: AlertService,
        private toastr: ToastrService
    ) {}

    ngOnInit() {
        this.initTabs();
        $("#side-menu").metisMenu();
        this.username = "Usuário";
        sessionStorage.setItem("projectName", "Teste");
        this.projectDetailForm = this.formBuilder.group({
            project_name: ["", Validators.required],
        });
    }

    logout() {
        sessionStorage.clear();
        this.router.navigate(["/"]);
    }

    createComponent(type: string) {
        let element: Element = {
            formIndex: 0,
            type: type,
            name: "",
            text: this.getTextByType(type),
            height: "50px",
            width: "50px",
            position: {
                x: $("#canvas").offset().left,
                y: $("#canvas").offset().top,
            },
            selectOptions: [],
        };
        this.spawnService.createComponent(element);
    }

    private getTextByType(type: string): string {
        switch (type.toLowerCase()) {
            case "label":
                return "Novo Label";
            case "button":
                return "Botão";
            case "container":
            case "input":
            case "select":
                return "";
        }
    }

    initTabs() {
        $("#project-detail-tabs").smartTab({
            selected: 0,
            theme: "brick",
            orientation: "vertical",
            backButtonSupport: false,
            enableURLhash: false,
            transition: {
                animation: "slide-vertical",
            },
            keyboardSettings: {
                keyNavigation: false,
            },
        });
    }

    getBuild() {
        this.alertService.createConfirmDenyDialog(
            "Download do código",
            "Deseja fazer o download do código gerado?",
            async () => {
                this.spinner.show("loadingSpinner");
                try {
                    let zip = await this.sendService.getProjectBuild();
                    const blob = new Blob([zip], {
                        type: "application/zip",
                    });
                    const url = window.URL.createObjectURL(blob);
                    var anchor = document.createElement("a");
                    anchor.download = `${sessionStorage.getItem(
                        "projectName"
                    )}.zip`;
                    anchor.target = "_blank";
                    anchor.href = url;
                    anchor.click();
                } catch (e) {
                    if (e instanceof HttpErrorResponse) {
                        this.toastr.error(
                            `Motivo: ${e.message}`,
                            `Erro: ${e.status}`,
                            { closeButton: true, progressBar: true }
                        );
                    } else {
                        console.log(e);
                    }
                } finally {
                    this.spinner.hide("loadingSpinner");
                }
            }
        );
    }
}
