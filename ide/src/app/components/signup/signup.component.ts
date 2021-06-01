import { HttpErrorResponse } from "@angular/common/http";
import { Component, OnInit } from "@angular/core";
import { FormBuilder, FormGroup, Validators } from "@angular/forms";
import { Router } from "@angular/router";
import { ToastrService } from "ngx-toastr";
import { AuthService } from "src/app/services/auth/auth.service";

@Component({
    templateUrl: "./signup.component.html",
    styleUrls: ["./signup.component.scss"],
})
export class SignupComponent implements OnInit {
    signupForm: FormGroup;

    constructor(
        private router: Router,
        private formBuilder: FormBuilder,
        private toastr: ToastrService,
        private auth: AuthService
    ) {}

    ngOnInit() {
        this.signupForm = this.formBuilder.group({
            email: ["", [Validators.required, Validators.email]],
            password: ["", Validators.required],
        });
    }

    async signUp() {
        try {
            await this.auth.signUp(
                this.signupForm.get("email").value,
                this.signupForm.get("password").value
            );
            this.toastr.success("Conta criada com sucesso", "Bem vindo!", {
                closeButton: true,
                progressBar: true,
            });
            this.router.navigate(["/"]);
        } catch (e) {
            if (e instanceof HttpErrorResponse) {
                this.toastr.error(e.error, "Erro!", {
                    closeButton: true,
                    progressBar: true,
                });
            }
        }
    }
}
