import { NgModule } from "@angular/core";
import { LoginComponent } from "./login.component";
import { ReactiveFormsModule } from "@angular/forms";
import { CommonModule } from "@angular/common";
import { RouterModule } from "@angular/router";

@NgModule({
    declarations: [LoginComponent],
    imports: [CommonModule, ReactiveFormsModule, RouterModule],
})
export class LoginModule {}
