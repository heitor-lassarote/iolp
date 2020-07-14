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
                "Basic " + btoa("heitortoledo@gec.inatel.br:bunda")
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
        return this.http
            .post<any>(this.postUrl, ast, {
                headers: this.header,
                withCredentials: true,
            })
            .toPromise();
    }
}
