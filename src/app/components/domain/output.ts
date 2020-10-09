export class Output {
    name: string;
    ast: {
        mainModule: Module;
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
    text: string;
}

export class Module {
    adtTemplates: Map<string, Constructor<Type>[]>;
    externs: Map<string, Type>;
    functions: Function[];
    importedModules: string[];
    moduleName: string;
}

export class Field<T> {
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
    tag = "adt";
    name: string;
}

export class ArrayType extends Type {
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
    tag = "function";
    arguments: Type[];
    return: Type;
}

export class IntegerType extends Type {
    tag = "integer";
}

export class RecordType extends Type {
    tag = "record";
    fields: Field<Type>[];
}

export class TextType extends Type {
    tag = "text";
}

export class Function {
    metadata: any = [];
    name: string;
    type: Type;
    arguments: string[];
    body: AST[];
}

export abstract class AST {
    tag: string;
}

export class Assign extends AST {
    tag = "assign";
    leftExpression: Expression;
    rightExpression: Expression;
}

export class End extends AST {
    tag = "end";
}

export class Expression_ extends AST {
    tag = "expression";
    expression: Expression;
}

export class If extends AST {
    tag = "if";
    expression: Expression;
    trueBranchAst: AST[];
    falseBranchAst: AST[];
}

export class Match extends AST {
    tag = "match";
    expression: Expression;
    branches: Branch[];
}

export class Return extends AST {
    tag = "return";
    expression?: Expression;
}

export class Var extends AST {
    tag = "var";
    name: string;
    type: Type;
    expression: Expression;
}

export class While extends AST {
    tag = "while";
    expression: Expression;
    whileAst: AST[];
}

export class Branch {
    pattern: MatchPattern;
    ast: AST[];
}

export abstract class MatchPattern {
    tag: string;
}

// FIXME: Change constructor_ to constructor.
export class LiteralPattern extends MatchPattern {
    tag = "literal";
    value: Literal;
}

export class NamePattern extends MatchPattern {
    tag = "name";
    name: string;
}

export class StructurePattern extends MatchPattern {
    tag = "structure";
    struct: Structure<MatchPattern>;
}

export abstract class Expression {
    tag: string;
}

export class Access extends Expression {
    tag = "access";
    expression: Expression;
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
    tag = "binaryOp";
    leftExpression: Expression;
    symbol: string;
    rightExpression: Expression;
}

export class Call extends Expression {
    tag = "call";
    expression: Expression;
    arguments: Expression[];
}

export class Index extends Expression {
    tag = "index";
    leftExpression: Expression;
    rightExpression: Expression;
}

export class Literal_ extends Expression {
    tag = "literal";
    value: Literal;
}

export class Parenthesis extends Expression {
    tag = "parenthesis";
    expression: Expression;
}

export class Structure_ extends Expression {
    tag = "structure";
    structure: Structure<Expression>;
}

// data UnarySymbol
//    = Negate
//    | Not
export class UnaryOp extends Expression {
    tag = "unaryOp";
    symbol: string;
    expression: Expression;
}

export class Variable extends Expression {
    tag = "variable";
    name: string;
}

export abstract class Literal {
    tag: string;
}

export class Char extends Literal {
    tag = "char";
    value: string;
}

export class Double extends Literal {
    tag = "double";
    value: number;
}

export class Integer extends Literal {
    tag = "integer";
    value: number;
}

export class Text extends Literal {
    tag = "text";
    value: string;
}

export abstract class Structure<T> {
    tag: string;
}

// FIXME: Change constructor_ to constructor.
export class Algebraic<T> extends Structure<T> {
    tag = "adt";
    constructor_: Constructor<T>;
}

export class Array<T> extends Structure<T> {
    tag = "array";
    positions: T[];
}

export class Record<T> extends Structure<T> {
    tag = "record";
    fields: Field<T>[];
}
