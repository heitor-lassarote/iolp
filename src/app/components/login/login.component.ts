import { Component, OnInit } from "@angular/core";
import { FormGroup, FormBuilder, Validators } from "@angular/forms";
import { Router } from "@angular/router";
import { ToastrService } from "ngx-toastr";

@Component({
    templateUrl: "./login.component.html",
    styleUrls: ["./login.component.scss"],
})
export class LoginComponent implements OnInit {
    loginForm: FormGroup;

    constructor(
        private router: Router,
        private formBuilder: FormBuilder,
        private toastr: ToastrService
    ) {}

    ngOnInit() {
        this.loginForm = this.formBuilder.group({
            login: ["", Validators.required],
            password: ["", Validators.required],
        });
    }

    login() {
        this.toastr.success("Login realizado com sucesso", "Bem vindo!", {
            closeButton: true,
            progressBar: true,
        });
        this.router.navigate(["/canvas"]);
    }
}
