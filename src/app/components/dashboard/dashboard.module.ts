import { NgModule } from "@angular/core";
import { ReactiveFormsModule } from "@angular/forms";
import { CommonModule } from "@angular/common";
import { RouterModule } from "@angular/router";
import { DashboardComponent } from "./dashboard.component";

@NgModule({
    declarations: [DashboardComponent],
    imports: [CommonModule, ReactiveFormsModule, RouterModule],
})
export class DashboardModule {}
