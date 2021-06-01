import { NgModule } from "@angular/core";
import { MainLayoutComponent } from "./main-layout.component";
import { CommonModule } from "@angular/common";
import { MainRoutingModule } from "./main.routing.module";
import { SideNavigationComponent } from "./menu/side-navigation/side-navigation.component";
import { TopNavbarComponent } from "./menu/top-navbar/top-navbar.component";
import { CanvasComponent } from "./canvas/canvas.component";
import { DragDropModule } from "@angular/cdk/drag-drop";
import { ResizableModule } from "angular-resizable-element";
import { CompInfosComponent } from "./comp-infos/comp-infos.component";
import { NgxSpinnerModule } from "ngx-spinner";
import { ReactiveFormsModule } from "@angular/forms";
import { SpawnComponentService } from "src/app/services/spawn/spawn-component.service";
import { ShowComponentInfoService } from "src/app/services/show-component-info/show-component-info.service";
import { MatTabsModule } from "@angular/material/tabs";

@NgModule({
    declarations: [
        MainLayoutComponent,
        SideNavigationComponent,
        TopNavbarComponent,
        CanvasComponent,
        CompInfosComponent,
    ],
    imports: [
        CommonModule,
        MainRoutingModule,
        DragDropModule,
        ResizableModule,
        NgxSpinnerModule,
        ReactiveFormsModule,
        MatTabsModule,
    ],
    providers: [SpawnComponentService, ShowComponentInfoService],
})
export class MainModule {}
