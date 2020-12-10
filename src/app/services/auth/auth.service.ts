import { HttpClient, HttpHeaders } from "@angular/common/http";
import { Injectable } from "@angular/core";
import { environment } from "src/environments/environment.prod";

@Injectable({
    providedIn: "root",
})
export class AuthService {
    private url: string = `${environment.baseUrl}/user`;
    private header: HttpHeaders;

    constructor(private http: HttpClient) {
        this.header = new HttpHeaders().set("Content-Type", "application/json");
    }

    login(email: string, password: string): Promise<number> {
        return this.http
            .put<number>(
                `${this.url}/login`,
                {},
                {
                    headers: this.header.set(
                        "Authorization",
                        "Basic " + btoa(`${email}:${password}`)
                    ),
                }
            )
            .toPromise();
    }

    signUp(email: string, password: string): Promise<any> {
        return this.http
            .post<any>(this.url, { email, password }, { headers: this.header })
            .toPromise();
    }
}
