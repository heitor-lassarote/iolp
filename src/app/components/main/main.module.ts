import { NgModule } from '@angular/core';
import { MainLayoutComponent } from './main-layout.component';
import { CommonModule } from '@angular/common';
import { MainRoutingModule } from './main.routing.module';
import { SideNavigationComponent } from './menu/side-navigation/side-navigation.component';
import { TopNavbarComponent } from './menu/top-navbar/top-navbar.component';
import { CanvasComponent } from './canvas/canvas.component';
import { DragDropModule } from '@angular/cdk/drag-drop';
import { ResizableModule } from 'angular-resizable-element';

@NgModule({
    declarations: [
        MainLayoutComponent,
        SideNavigationComponent,
        TopNavbarComponent,
        CanvasComponent
    ],
    imports: [
        CommonModule,
        MainRoutingModule,
        DragDropModule,
        ResizableModule
    ],
})
export class MainModule {}
