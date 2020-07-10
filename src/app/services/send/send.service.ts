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
        this.header = new HttpHeaders();
        this.header.set("email", "brunocaputo@gec.inatel.br");
        this.header.set("password", "123456");
    }

    getProject(id: any): Promise<any> {
        let url = `${this.getUrl}/${id}`;
        return this.http
            .get<any>(url, { headers: this.header })
            .toPromise();
    }

    postCode(ast: Output): Promise<any> {
        return this.http
            .post<any>(this.postUrl, ast, { headers: this.header })
            .toPromise();
    }
}
