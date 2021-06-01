import { Component, OnInit } from "@angular/core";
import { FormBuilder, FormGroup, Validators } from "@angular/forms";
import { Router } from "@angular/router";
import { NgxSpinnerService } from "ngx-spinner";
import { ToastrService } from "ngx-toastr";
import { AlertService } from "src/app/services/alert/alert.service";
import { DashboardService } from "src/app/services/dashboard/dashboard.service";
import { Output } from "../domain/output";
import { Project } from "../domain/project";

declare let $: any;

@Component({
    templateUrl: "./dashboard.component.html",
    styleUrls: ["./dashboard.component.scss"],
})
export class DashboardComponent implements OnInit {
    projects: Project[] = [];
    newProjectForm: FormGroup;

    constructor(
        private dashService: DashboardService,
        private spinner: NgxSpinnerService,
        private toastr: ToastrService,
        private router: Router,
        private alert: AlertService,
        private formBuilder: FormBuilder
    ) {}

    ngOnInit() {
        this.newProjectForm = this.formBuilder.group({
            projectName: [null, [Validators.required]],
        });

        this.getProjectList();
    }

    async getProjectList() {
        this.spinner.show("loadingSpinner");
        try {
            let projectList: any = await this.dashService.getProjectList();
            this.projects = projectList.projects;
        } catch (e) {
            this.projects = [];
            this.toastr.error("Falha ao buscar projetos do usuário!", "Erro!", {
                closeButton: true,
                progressBar: true,
            });
        } finally {
            this.spinner.hide("loadingSpinner");
        }
    }

    isAuthenticated() {
        return true;
    }

    async addNewProject() {
        const value: Output = {
            name: this.newProjectForm.get("projectName").value,
            ast: {
                pages: [],
            },
        };

        try {
            this.spinner.show("loadingSpinner");
            let projectId: number = await this.dashService.createNewProject(
                value
            );
            sessionStorage.setItem("projectID", projectId.toString());
            sessionStorage.setItem(
                "projectName",
                this.newProjectForm.get("projectName").value
            );
            this.router.navigate(["/canvas"]);
        } catch (e) {
            this.toastr.error("Falha ao criar projeto", "Erro!", {
                closeButton: true,
                progressBar: true,
            });
        } finally {
            $("#add-new-project-modal").modal("hide");
            this.spinner.hide("loadingSpinner");
        }
    }

    editProject(id: number, name: string) {
        sessionStorage.setItem("projectID", id.toString());
        sessionStorage.setItem("projectName", name);
        this.router.navigate(["/canvas"]);
    }

    removeProject(id: number, arrayIndex: number) {
        this.alert.createConfirmDenyDialog(
            `Deseja deletar o projeto ${id}?`,
            "Deletar projeto!",
            async () => {
                this.spinner.show("loadingSpinner");
                try {
                    await this.dashService.deleteProject(id);
                    this.projects.splice(arrayIndex, 1);
                } catch (error) {
                    this.toastr.error("Falha ao remover projeto!", "Erro!", {
                        closeButton: true,
                        progressBar: true,
                    });
                } finally {
                    this.spinner.hide("loadingSpinner");
                }
            }
        );
    }
}
