import { Component, OnInit } from '@angular/core';
import { Subject } from 'rxjs';

@Component({
    selector: 'app-main',
    templateUrl: './main-layout.component.html',
})
export class MainLayoutComponent implements OnInit {
    unsub$ = new Subject();

    constructor() {}

    isAuthenticated() {
        return true;
    }

    ngOnInit() {}

    ngOnDestroy() {
        this.unsub$.next();
        this.unsub$.complete();
    }
}
