import { Info } from "./../../components/domain/info";
import { Subject } from "rxjs";
import { Injectable } from "@angular/core";

@Injectable({
    providedIn: "root",
})
export class ShowComponentInfoService {
    infos = new Subject<any>();

    constructor() {}

    getComponentInfos() {
        return this.infos;
    }

    setComponentInfos(info: Info) {
        this.infos.next(info);
    }
}
