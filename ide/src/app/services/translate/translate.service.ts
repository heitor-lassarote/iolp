import { Injectable } from "@angular/core";
import { TranslateKeysService } from "./translate-keys.service";

@Injectable({
	providedIn: "root"
})
export class TranslateResponseService {
	constructor(private translate: TranslateKeysService) {}

	getObjectTranslate(res: any) {
		const keys = Object.keys(res);
		keys.forEach(key => {
			if (typeof res[key] === "string") {
				res[key] = this.translate.getTranslate(key, res[key]);
			} else if (typeof res[key] === "object") {
				res[key] = this.getObjectTranslate(res[key]);
			}
		});
		return res;
	}

	getObjectKey(res: any) {
		const keys = Object.keys(res);
		keys.forEach(key => {
			if (typeof res[key] === "string") {
				res[key] = this.translate.getKey(res[key]);
			} else if (typeof res[key] === "object") {
				res[key] = this.getObjectKey(res[key]);
			}
		});
		return res;
	}
}
