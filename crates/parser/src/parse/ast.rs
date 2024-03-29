use serde::{Deserialize, Serialize};
use crate::lex::token::{Name, NumberLiteral, StringLiteral};

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct Chunk {
    pub statements: Vec<Statement>,
    pub last_statement: Option<LastStatement>,
}

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct Block {
    pub chunk: Chunk,
}

#[derive(Debug, Serialize, Deserialize)]
pub enum Statement {
    Set {
        variables: VariableList,
        expressions: ExpressionList,
    },
    FunctionCall(FunctionCall),
    Do {
        block: Box<Block>,
    },
    While {
        expression: Box<Expression>,
        block: Box<Block>,
    },
    Repeat {
        expression: Box<Expression>,
        block: Box<Block>,
    },
    If {
        expression: Box<Expression>,
        block: Box<Block>,

        else_if_clauses: Vec<(Box<Expression>, Box<Block>)>,
        else_clause: Option<(Box<Expression>, Box<Block>)>,
    },
    For {
        variable: Name,

        initial: Box<Expression>,
        limit: Box<Expression>,
        increment: Option<Box<Expression>>,
    },
    ForIn {
        variables: NameList,
        expressions: ExpressionList,
        block: Box<Block>,
    },
    Function {
        name: FunctionName,
        body: FunctionBody,
    },
    LocalFunction {
        name: Name,
        body: FunctionBody,
    },
    LocalSet {
        names: NameList,
        expressions: Option<ExpressionList>,
    },
}

#[derive(Debug, Serialize, Deserialize)]
pub enum LastStatement {
    Return(ExpressionList),
    Break,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct FunctionName {
    root: Name,
    additional: Vec<Name>,
    last: Option<Name>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct VariableList(Vec<Variable>);

#[derive(Debug, Serialize, Deserialize)]
pub enum Variable {
    Name(Name),
    Index {
        prefix: Box<PrefixExpression>,
        index: Box<Expression>,
    },
    Dot {
        prefix: Box<PrefixExpression>,
        name: Name,
    },
}

#[derive(Debug, Serialize, Deserialize)]
pub struct NameList(pub Vec<Name>);

#[derive(Debug, Serialize, Deserialize)]
pub struct ExpressionList(pub Vec<Expression>);

#[derive(Debug, Serialize, Deserialize)]
pub enum Expression {
    Nil,
    Bool(bool),
    Number(NumberLiteral),
    String(StringLiteral),
    Spread,
    AnonymousFunction(AnonymousFunction),
    Prefix(Box<PrefixExpression>),
    TableConstructor(TableConstructor),
    Binary {
        left: Box<Expression>,
        operator: BinaryOperator,
        right: Box<Expression>,
    },
    Unary {
        operator: UnaryOperator,
        expression: Box<Expression>,
    },
}

#[derive(Debug, Serialize, Deserialize)]
pub enum PrefixExpression {
    Variable(Variable),
    FunctionCall(FunctionCall),
    Expression(Box<Expression>),
}

#[derive(Debug, Serialize, Deserialize)]
pub enum FunctionCall {
    Normal {
        prefix: Box<PrefixExpression>,
        args: Args,
    },
    This {
        prefix: Box<PrefixExpression>,
        name: Name,
        args: Args,
    },
}

#[derive(Debug, Serialize, Deserialize)]
pub enum Args {
    List(ExpressionList),
    TableConstructor(TableConstructor),
    String(StringLiteral),
}

#[derive(Debug, Serialize, Deserialize)]
pub struct AnonymousFunction {
    pub body: FunctionBody,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct FunctionBody {
    pub parameters: ParameterList,
    pub block: Block,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ParameterList {
    pub names: NameList,
    pub spread: bool,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct TableConstructor(pub FieldList);

#[derive(Debug, Serialize, Deserialize)]
pub struct FieldList(pub Vec<Field>);

#[derive(Debug, Serialize, Deserialize)]
pub enum Field {
    Index {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Name {
        left: Name,
        right: Box<Expression>,
    },
    Expression(Box<Expression>),
}

#[derive(Debug, Serialize, Deserialize)]
pub enum BinaryOperator {}

#[derive(Debug, Serialize, Deserialize)]
pub enum UnaryOperator {}
