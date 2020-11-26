import { NgModule } from "@angular/core";
import { SignupComponent } from "./signup.component";
import { ReactiveFormsModule } from "@angular/forms";
import { CommonModule } from "@angular/common";
import { RouterModule } from "@angular/router";

@NgModule({
    declarations: [SignupComponent],
    imports: [CommonModule, ReactiveFormsModule, RouterModule],
})
export class SignupModule {}
