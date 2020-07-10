import { FormGroup, FormBuilder, Validators } from "@angular/forms";
import { Component, OnInit } from "@angular/core";
import { Router } from "@angular/router";
import { Element } from "src/app/components/domain/element";
import { SpawnComponentService } from "src/app/services/spawn/spawn-component.service";

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
        private formBuilder: FormBuilder
    ) {}

    ngOnInit() {
        this.initTabs();
        $("#side-menu").metisMenu();
        this.username = "admin";
        sessionStorage.setItem("projectName", "Teste");
        this.projectDetailForm = this.formBuilder.group({
            project_name: ["", Validators.required],
        });
    }

    logout() {
        console.log("Sair");
        // this.router.navigate(['/']);
    }

    createComponent(type: string) {
        let element: Element = {
            type: type,
            height: "50px",
            width: "50px",
            position: {
                x: 50,
                y: 50,
            },
        };
        this.spawnService.createComponent(element);
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
}
