import { Output } from "./../../components/domain/output";
import { Injectable } from "@angular/core";
import { HttpClient, HttpHeaders } from "@angular/common/http";
import { environment } from "src/environments/environment.prod";

@Injectable({
    providedIn: "root",
})
export class SendService {
    baseUrl: string = `${environment.baseUrl}/editor`;
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

    getProject(id: any): Promise<any> {
        let url = `${this.baseUrl}/${id}`;
        return this.http
            .get<any>(url, { headers: this.header, withCredentials: true })
            .toPromise();
    }

    getProjectBuild(): Promise<ArrayBuffer> {
        let url = `${this.baseUrl}/${sessionStorage.getItem(
            "projectID"
        )}/build`;
        return this.http
            .get<ArrayBuffer>(url, {
                headers: this.header,
                withCredentials: true,
                responseType: "arraybuffer" as "json",
            })
            .toPromise();
    }

    postCode(ast: Output): Promise<any> {
        if (sessionStorage.getItem("projectID")) {
            let url = `${this.baseUrl}/${sessionStorage.getItem("projectID")}`;
            return this.http
                .put<any>(url, ast, {
                    headers: this.header,
                    withCredentials: true,
                })
                .toPromise();
        } else {
            return this.http
                .post<any>(this.baseUrl, ast, {
                    headers: this.header,
                    withCredentials: true,
                })
                .toPromise();
        }
    }
}
