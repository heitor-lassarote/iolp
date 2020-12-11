export class Output {
    name: string;
    ast: {
        pages: Page[];
    };
}

export class Page {
    name: string;
    css: CssOut[];
    logic: Module[];
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
    constructor(text: string) {
        super();
        this.text = text;
    }

    text: string;
}

export class Module {
    adtTemplates: Map<string, Constructor<Type>[]>;
    externs: Object;
    functions: Function[];
    importedModules: string[];
    moduleName: string;
}

export class Field<T> {
    constructor(fieldName: string, fieldValue: T) {
        this.fieldName = fieldName;
        this.fieldValue = fieldValue;
    }

    fieldName: string;
    fieldValue: T;
}

export class Constructor<T> {
    constructorAdt: string;
    constructorName: string;
    constructorValue?: T;
}

export abstract class Type {
    tag: string;
}

export class AlgebraicType extends Type {
    constructor(name: string) {
        super();
        this.name = name;
    }

    tag = "adt";
    name: string;
}

export class ArrayType extends Type {
    constructor(elements: Type) {
        super();
        this.elements = elements;
    }

    tag = "array";
    elements: Type;
}

export class CharType extends Type {
    tag = "char";
}

export class DoubleType extends Type {
    tag = "double";
}

export class FunctionType extends Type {
    constructor(args: Type[], rtrn: Type) {
        super();
        this.arguments = args;
        this.return = rtrn;
    }

    tag = "function";
    arguments: Type[];
    return: Type;
}

export class IntegerType extends Type {
    tag = "integer";
}

export class RecordType extends Type {
    constructor(fields: Field<Type>[]) {
        super();
        this.fields = fields;
    }

    tag = "record";
    fields: Field<Type>[];
}

export class TextType extends Type {
    tag = "text";
}

export class Function {
    constructor(name: string, type: Type, args: string[], body: AST[]) {
        this.name = name;
        this.type = type;
        this.arguments = args;
        this.body = body;
    }

    name: string;
    type: Type;
    arguments: string[];
    body: AST[];
}

export abstract class AST {
    tag: string;
}

export class Assign extends AST {
    constructor(
        leftExpression: Expression | string,
        rightExpression: Expression | string
    ) {
        super();
        this.leftExpression = leftExpression;
        this.rightExpression = rightExpression;
    }

    tag = "assign";
    leftExpression: Expression | string;
    rightExpression: Expression | string;
}

export class End extends AST {
    tag = "end";
}

export class Expression_ extends AST {
    constructor(expression: Expression | string) {
        super();
        this.expression = expression;
    }

    tag = "expression";
    expression: Expression | string;
}

export class If extends AST {
    constructor(
        expression: Expression | string,
        trueBranchAst: AST[],
        falseBranchAst: AST[]
    ) {
        super();
        this.expression = expression;
        this.trueBranchAst = trueBranchAst;
        this.falseBranchAst = falseBranchAst;
    }

    tag = "if";
    expression: Expression | string;
    trueBranchAst: AST[];
    falseBranchAst: AST[];
}

export class Match extends AST {
    constructor(expression: Expression | string, branches: Branch[]) {
        super();
        this.expression = expression;
        this.branches = branches;
    }

    tag = "match";
    expression: Expression | string;
    branches: Branch[];
}

export class Return extends AST {
    constructor(expression?: Expression | string) {
        super();

        this.expression = expression;
    }

    tag = "return";
    expression?: Expression | string;
}

export class Var extends AST {
    constructor(name: string, type: Type, expression: Expression | string) {
        super();
        this.name = name;
        this.type = type;
        this.expression = expression;
    }

    tag = "var";
    name: string;
    type: Type;
    expression: Expression | string;
}

export class While extends AST {
    constructor(expression: Expression | string, whileAst: AST[]) {
        super();
        this.expression = expression;
        this.whileAst = whileAst;
    }

    tag = "while";
    expression: Expression | string;
    whileAst: AST[];
}

export class Branch {
    constructor(pattern: MatchPattern, ast: AST[]) {
        this.pattern = pattern;
        this.ast = ast;
    }

    pattern: MatchPattern;
    ast: AST[];
}

export abstract class MatchPattern {
    tag: string;
}

// FIXME: Change constructor_ to constructor.
export class LiteralPattern extends MatchPattern {
    constructor(value: Literal) {
        super();
        this.value = value;
    }

    tag = "literal";
    value: Literal;
}

export class NamePattern extends MatchPattern {
    constructor(name: string) {
        super();
        this.name = name;
    }

    tag = "name";
    name: string;
}

export class StructurePattern extends MatchPattern {
    constructor(struct: Structure<MatchPattern>) {
        super();
        this.struct = struct;
    }

    tag = "structure";
    struct: Structure<MatchPattern>;
}

export abstract class Expression {
    tag: string;
}

export class Access extends Expression {
    constructor(expression: Expression | string, name: string) {
        super();
        this.expression = expression;
        this.name = name;
    }

    tag = "access";
    expression: Expression | string;
    name: string;
}

//data BinarySymbol
//    = Add
//    | Divide
//    | Multiply
//    | Subtract
//    | Different
//    | Equal
//    | Greater
//    | GreaterEqual
//    | Less
//    | LessEqual
//    | And
//    | Or
export class BinaryOp extends Expression {
    constructor(
        leftExpression: Expression | string,
        symbol: string,
        rightExpression: Expression | string
    ) {
        super();
        this.leftExpression = leftExpression;
        this.symbol = symbol;
        this.rightExpression = rightExpression;
    }

    tag = "binaryOp";
    leftExpression: Expression | string;
    symbol: string;
    rightExpression: Expression | string;
}

export class Call extends Expression {
    constructor(
        expression: Expression | string,
        args: Expression[] | string[]
    ) {
        super();
        this.expression = expression;
        this.arguments = args;
    }

    tag = "call";
    expression: Expression | string;
    arguments: Expression[] | string[];
}

export class Index extends Expression {
    constructor(
        leftExpression: Expression | string,
        rightExpression: Expression | string
    ) {
        super();
        this.leftExpression = leftExpression;
        this.rightExpression = rightExpression;
    }

    tag = "index";
    leftExpression: Expression | string;
    rightExpression: Expression | string;
}

export class Interpolated extends Expression {
    constructor(value: InterpolatedElement[]) {
        super();
        this.value = value;
    }

    tag = "interpolated";
    value: InterpolatedElement[];
}

export abstract class InterpolatedElement {}

export class InterpolatedText extends InterpolatedElement {
    constructor(text: string) {
        super();
        this.text = text;
    }

    text: string;
}

export class InterpolatedExpression extends InterpolatedElement {
    constructor(expression: Expression | string) {
        super();
        this.expression = expression;
    }
    expression: Expression | string;
}

export class Literal_ extends Expression {
    constructor(value: Literal) {
        super();
        this.value = value;
    }

    tag = "literal";
    value: Literal;
}

export class Parenthesis extends Expression {
    constructor(expression: Expression | string) {
        super();
        this.expression = expression;
    }

    tag = "parenthesis";
    expression: Expression | string;
}

export class Structure_ extends Expression {
    constructor(structure: Structure<Expression>) {
        super();
        this.structure = structure;
    }

    tag = "structure";
    structure: Structure<Expression>;
}

// data UnarySymbol
//    = Negate
//    | Not
export class UnaryOp extends Expression {
    constructor(symbol: string, expression: Expression | string) {
        super();
        this.symbol = symbol;
        this.expression = expression;
    }

    tag = "unaryOp";
    symbol: string;
    expression: Expression | string;
}

export class Variable extends Expression {
    constructor(name: string) {
        super();
        this.name = name;
    }

    tag = "variable";
    name: string;
}

//Neutral Expression
export class StringExpression extends Expression {
    constructor(value: string) {
        super();
        this.value = value;
    }

    value: string;
}

export abstract class Literal {
    tag: string;
}

export class Char extends Literal {
    constructor(value: string) {
        super();
        this.value = value;
    }

    tag = "char";
    value: string;
}

export class Double extends Literal {
    constructor(value: number) {
        super();
        this.value = value;
    }

    tag = "double";
    value: number;
}

export class Integer extends Literal {
    constructor(value: number) {
        super();
        this.value = value;
    }

    tag = "integer";
    value: number;
}

export class Text extends Literal {
    constructor(value: string) {
        super();
        this.value = value;
    }

    tag = "text";
    value: string;
}

export abstract class Structure<T> {
    tag: string;
}

// FIXME: Change constructor_ to constructor.
export class Algebraic<T> extends Structure<T> {
    constructor(construc: Constructor<T>) {
        super();
        this.constructor_ = construc;
    }

    tag = "adt";
    constructor_: Constructor<T>;
}

export class Array<T> extends Structure<T> {
    constructor(positions: T[]) {
        super();
        this.positions = positions;
    }

    tag = "array";
    positions: T[];
}

export class Record<T> extends Structure<T> {
    constructor(fields: Field<T>[]) {
        super();
        this.fields = fields;
    }

    tag = "record";
    fields: Field<T>[];
}
