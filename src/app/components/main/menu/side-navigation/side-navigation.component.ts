import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';

declare let $: any;

@Component({
    selector: 'app-side-navigation',
    templateUrl: './side-navigation.component.html',
    styleUrls: ['./side-navigation.component.scss'],
})
export class SideNavigationComponent implements OnInit {
    username: string = '';
    userEmail: string = '';

    constructor(private router: Router) // private auth: AuthService,
    // private cookie: CookieService
    {}

    ngOnInit() {
        $('#side-menu').metisMenu();
        this.username = "admin";
    }

    logout() {
        this.router.navigate(['/']);
    }
}
