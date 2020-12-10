import { Component, OnInit } from "@angular/core";
import { Router } from "@angular/router";
import { smoothlyMenu } from "src/app/app.helpers";

declare let $: any;

@Component({
    selector: "dashboard-top-navbar",
    templateUrl: "./top-navbar.component.html",
    styleUrls: ["./top-navbar.component.scss"],
})
export class DashboardTopNavbarComponent implements OnInit {
    constructor(private router: Router) {}

    ngOnInit() {}

    logout() {
        sessionStorage.clear();
        this.router.navigate(["/"]);
    }
}
