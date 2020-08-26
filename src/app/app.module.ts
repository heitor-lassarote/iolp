import { MainModule } from "./components/main/main.module";
import { BrowserModule } from "@angular/platform-browser";
import { NgModule } from "@angular/core";

import { AppRoutingModule } from "./app-routing.module";
import { AppComponent } from "./app.component";
import { LocationStrategy, HashLocationStrategy } from "@angular/common";
import { BrowserAnimationsModule } from "@angular/platform-browser/animations";
import { HttpClientModule } from "@angular/common/http";
import { ToastrModule } from "ngx-toastr";
import { NgxSpinnerModule } from "ngx-spinner";
import { TranslateResponseService } from "./services/translate/translate.service";
import { TranslateKeysService } from "./services/translate/translate-keys.service";
import { LoginModule } from "./components/login/login.module";

@NgModule({
    declarations: [AppComponent],
    imports: [
        BrowserModule,
        BrowserAnimationsModule,
        AppRoutingModule,
        HttpClientModule,
        MainModule,
        ToastrModule.forRoot(),
        NgxSpinnerModule,
        LoginModule,
    ],
    providers: [
        { provide: LocationStrategy, useClass: HashLocationStrategy },
        TranslateResponseService,
        TranslateKeysService,
    ],
    bootstrap: [AppComponent],
})
export class AppModule {}
