import { HttpClient, HttpHeaders } from "@angular/common/http";
import { Injectable } from "@angular/core";
import { Output } from "src/app/components/domain/output";
import { Project } from "src/app/components/domain/project";
import { environment } from "src/environments/environment.prod";

@Injectable({
    providedIn: "root",
})
export class DashboardService {
    private getProjectsUrl: string = `${environment.baseUrl}/user`;
    private handleProjectUrl: string = `${environment.baseUrl}/editor`;
    private header: HttpHeaders;

    constructor(private http: HttpClient) {
        this.header = new HttpHeaders()
            .set(
                "Authorization",
                "Basic " +
                    btoa(
                        `${sessionStorage.getItem(
                            "email"
                        )}:${sessionStorage.getItem("password")}`
                    )
            )
            .set("Content-Type", "application/json");
    }

    getProjectList(): Promise<Project[]> {
        return this.http
            .get<Project[]>(this.getProjectsUrl, {
                headers: this.header,
                withCredentials: true,
            })
            .toPromise();
    }

    createNewProject(value: Output): Promise<number> {
        return this.http
            .post<number>(this.handleProjectUrl, value, {
                headers: this.header,
                withCredentials: true,
            })
            .toPromise();
    }

    deleteProject(projectId: number): Promise<any> {
        return this.http
            .delete<any>(`${this.handleProjectUrl}/${projectId}`, {
                headers: this.header,
                withCredentials: true,
            })
            .toPromise();
    }
}
