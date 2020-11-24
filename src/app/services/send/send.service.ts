import { Output } from "./../../components/domain/output";
import { Injectable } from "@angular/core";
import { HttpClient, HttpHeaders } from "@angular/common/http";
import { environment } from "src/environments/environment.prod";

@Injectable({
    providedIn: "root",
})
export class SendService {
    postUrl: string = `${environment.baseUrl}/editor`;
    getUrl: string = `${environment.baseUrl}/editor`;
    private header: HttpHeaders;

    constructor(private http: HttpClient) {
        this.header = new HttpHeaders()
            .set(
                "Authorization",
                "Basic " + btoa("brunocaputo@gec.inatel.br:teste123")
            )
            .set("Content-Type", "application/json");
    }

    getProject(id: any): Promise<any> {
        let url = `${this.getUrl}/${id}`;
        return this.http
            .get<any>(url, { headers: this.header, withCredentials: true })
            .toPromise();
    }

    getProjectBuild(): Promise<ArrayBuffer> {
        let url = `${this.getUrl}/${sessionStorage.getItem("projectID")}/build`;
        return this.http
            .get<ArrayBuffer>(url, {
                headers: this.header,
                withCredentials: true,
                responseType: "arraybuffer" as "json",
            })
            .toPromise();
    }

    postCode(ast: Output): Promise<any> {
        // if (sessionStorage.getItem("projectID")) {
        //     let url = `${this.postUrl}/${sessionStorage.getItem("projectID")}`;
        //     return this.http
        //         .put<any>(url, ast, {
        //             headers: this.header,
        //             withCredentials: true,
        //         })
        //         .toPromise();
        // } else {
            return this.http
                .post<any>(this.postUrl, ast, {
                    headers: this.header,
                    withCredentials: true,
                })
                .toPromise();
        // }
    }
}
