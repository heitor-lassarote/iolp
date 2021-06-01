import { Injectable } from '@angular/core';
import { Subject } from 'rxjs';
import { Element } from 'src/app/components/domain/element';

@Injectable({
    providedIn: 'root',
})
export class SpawnComponentService {
    elements = new Subject<Element>();

    constructor() {}

    createComponent(component: Element) {
        this.elements.next(component);
    }

    getElements(): Subject<Element> {
        return this.elements;
    }
}
