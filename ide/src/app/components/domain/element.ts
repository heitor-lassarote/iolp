export class Element {
    formIndex: number;
    type: string;
    name: string;
    text: string;
    width: string;
    height: string;
    position: {
        x: number;
        y: number;
    };
    selectOptions?: string[];
    imgSrc?: string;
}
