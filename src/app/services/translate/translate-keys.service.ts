import { Injectable } from "@angular/core";

const keys = {};

const values = {};

@Injectable({
    providedIn: "root",
})
export class TranslateKeysService {
    constructor() {}

    getTranslate(objectName: string, key: string | number) {
        if (typeof keys[objectName] === "object")
            return keys[objectName][key] ? keys[objectName][key] : key;
        return keys[key] ? keys[key] : key;
    }

    getKey(value: string | number) {
        return values[value] ? values[value] : value;
    }
}
