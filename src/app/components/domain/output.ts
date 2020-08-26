export class Output {
    name: string;
    ast: {
        pages: Page[];
    };
}

export class Page {
    name: string;
    css: CssOut[];
    logic: any[];
    html: HtmlOut[];
}

export class CssOut {
    className: string;
    attributes: [string, string][];
}

export abstract class HtmlOut {}

export class HtmlTagOut extends HtmlOut {
    tag: string;
    ast: HtmlOut[];
    attributes: [string, string][];
}

export class HtmlTextOut extends HtmlOut {
    text: string;
}
