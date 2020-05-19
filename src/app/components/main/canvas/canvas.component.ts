import { Component, OnInit } from '@angular/core';
import { ResizeEvent } from 'angular-resizable-element';

declare let $: any;

@Component({
    selector: 'app-canvas',
    templateUrl: './canvas.component.html',
    styleUrls: ['./canvas.component.scss'],
})
export class CanvasComponent implements OnInit {
    constructor() {}

    ngOnInit() {}

    onResizeEnd(event: ResizeEvent): void {
        console.log('Element was resized', event);
    }
}
