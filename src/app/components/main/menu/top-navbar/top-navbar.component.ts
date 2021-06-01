import { Component, OnInit } from "@angular/core";
import { Router } from "@angular/router";
import { smoothlyMenu } from "src/app/app.helpers";

declare let $: any;

@Component({
    selector: "app-top-navbar",
    templateUrl: "./top-navbar.component.html",
    styleUrls: ["./top-navbar.component.scss"],
})
export class TopNavbarComponent implements OnInit {
    constructor(private router: Router) {}

    ngOnInit() {}

    toggleNavigation(): void {
        $("body").toggleClass("mini-navbar");
        smoothlyMenu();
    }

    logout() {
        sessionStorage.removeItem("projectID");
        sessionStorage.removeItem("projectName");
        this.router.navigate(["/dashboard"]);
    }
}
