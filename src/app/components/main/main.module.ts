import { NgModule } from '@angular/core';
import { MainLayoutComponent } from './main-layout.component';
import { CommonModule } from '@angular/common';
import { MainRoutingModule } from './main.routing.module';
import { SideNavigationComponent } from './menu/side-navigation/side-navigation.component';
import { TopNavbarComponent } from './menu/top-navbar/top-navbar.component';
import { CanvasComponent } from './canvas/canvas.component';

@NgModule({
    declarations: [
        MainLayoutComponent,
        SideNavigationComponent,
        TopNavbarComponent,
        CanvasComponent
    ],
    imports: [CommonModule, MainRoutingModule],
})
export class MainModule {}
