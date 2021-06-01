import { HttpErrorResponse } from "@angular/common/http";
import { Component, OnInit } from "@angular/core";
import { FormGroup, FormBuilder, Validators } from "@angular/forms";
import { Router } from "@angular/router";
import { ToastrService } from "ngx-toastr";
import { AuthService } from "src/app/services/auth/auth.service";

@Component({
    templateUrl: "./login.component.html",
    styleUrls: ["./login.component.scss"],
})
export class LoginComponent implements OnInit {
    loginForm: FormGroup;

    constructor(
        private router: Router,
        private formBuilder: FormBuilder,
        private toastr: ToastrService,
        private auth: AuthService
    ) {}

    ngOnInit() {
        this.loginForm = this.formBuilder.group({
            email: ["", [Validators.required, Validators.email]],
            password: ["", Validators.required],
        });
    }

    async login() {
        try {
            await this.auth.login(
                this.loginForm.get("email").value,
                this.loginForm.get("password").value
            );
            this.toastr.success("Login realizado com sucesso", "Bem vindo!", {
                closeButton: true,
                progressBar: true,
            });
            sessionStorage.setItem("email", this.loginForm.get("email").value);
            sessionStorage.setItem(
                "password",
                this.loginForm.get("password").value
            );
            this.router.navigate(["/dashboard"]);
        } catch (e) {
            if (e instanceof HttpErrorResponse) {
                switch (e.status) {
                    case 404:
                        this.toastr.error("Usuário não existente", "Erro!", {
                            closeButton: true,
                            progressBar: true,
                        });
                        break;
                    case 401:
                        this.toastr.error("Credenciais incorretas", "Erro!", {
                            closeButton: true,
                            progressBar: true,
                        });
                        break;
                    default:
                        this.toastr.error("Falha ao realizar login", "Erro!", {
                            closeButton: true,
                            progressBar: true,
                        });
                        break;
                }
            }
        }
    }
}
