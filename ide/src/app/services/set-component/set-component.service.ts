import { Injectable } from "@angular/core";
import { Subject } from "rxjs";
import { Info } from "src/app/components/domain/info";

@Injectable({
    providedIn: "root",
})
export class SetComponentService {
    private newValue = new Subject<any>();

    constructor() {}

    getNewValue() {
        return this.newValue;
    }

    setNewValue(info: Info) {
        this.newValue.next(info);
    }
}
