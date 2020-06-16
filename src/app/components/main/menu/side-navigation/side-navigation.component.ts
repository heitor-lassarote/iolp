import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { Element } from 'src/app/components/domain/element';
import { SpawnComponentService } from 'src/app/services/spawn/spawn-component.service';

declare let $: any;

@Component({
    selector: 'app-side-navigation',
    templateUrl: './side-navigation.component.html',
    styleUrls: ['./side-navigation.component.scss'],
})
export class SideNavigationComponent implements OnInit {
    username: string = '';
    userEmail: string = '';

    constructor(
        private router: Router,
        private spawnService: SpawnComponentService
    ) {}

    ngOnInit() {
        $('#side-menu').metisMenu();
        this.username = 'admin';
    }

    logout() {
        this.router.navigate(['/']);
    }

    createComponent(type: string) {
        let element: Element = {
            type: type,
            height: '50px',
            width: '50px',
            position: {
                x: 50,
                y: 50,
            },
        };
        this.spawnService.createComponent(element);
    }
}
