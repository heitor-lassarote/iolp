import { Injectable } from "@angular/core";

declare let swal: any;

@Injectable({
    providedIn: "root",
})
export class AlertService {
    constructor() {}

    createConfirmDenyDialog(title: string, message: string, action: Function) {
        swal({
            title: title,
            text: message,
            icon: "warning",
            buttons: true,
        }).then((confirm: boolean) => {
            if (confirm) {
                action();
            }
        });
    }

    createConfirmDialog(title: string, message: string) {
        swal({
            title: title,
            text: message,
            icon: "warning",
        });
    }
}
