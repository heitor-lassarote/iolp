import { NgModule } from '@angular/core';
import { Routes, RouterModule } from '@angular/router';

import { MainLayoutComponent } from './main-layout.component';
import { CanvasComponent } from './canvas/canvas.component';

const mainRoutes: Routes = [
    {
        path: '',
        component: MainLayoutComponent,
        children: [
            { path: 'canvas', component: CanvasComponent },
        ],
    },
];

@NgModule({
    imports: [RouterModule.forChild(mainRoutes)],
    exports: [RouterModule],
})
export class MainRoutingModule {}
