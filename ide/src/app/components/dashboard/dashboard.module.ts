import { NgModule } from "@angular/core";
import { ReactiveFormsModule } from "@angular/forms";
import { CommonModule } from "@angular/common";
import { RouterModule } from "@angular/router";
import { DashboardComponent } from "./dashboard.component";
import { NgxSpinnerModule } from "ngx-spinner";
import { DashboardTopNavbarComponent } from "./top-navbar/top-navbar.component";

@NgModule({
    declarations: [DashboardComponent, DashboardTopNavbarComponent],
    imports: [
        CommonModule,
        ReactiveFormsModule,
        RouterModule,
        NgxSpinnerModule,
    ],
})
export class DashboardModule {}
