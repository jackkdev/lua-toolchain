/// Represents a "token" or building block of the language syntax.
#[derive(Debug, PartialEq)]
pub enum Token {
    StringLiteral(StringLiteral),
    NumberLiteral(NumberLiteral),
    BooleanLiteral(bool),
    NilLiteral,

    Identifier(String),
    Keyword(Keyword),
    Operator(Operator),

    Comma,
    SemiColon,
    Colon,
    Assign,

    Vararg,

    Paren(bool),
    SquareBrace(bool),
    CurlyBrace(bool),

    Comment(Comment),
}

/// Represents any string literal token.
#[derive(Debug, PartialEq)]
pub enum StringLiteral {
    Single(String),
    Double(String),
    Long(String, usize),
}

/// Represents any number literal token.
#[derive(Debug, PartialEq)]
pub enum NumberLiteral {
    Decimal(f64),
    Hexadecimal(f64),
    Scientific(f64, f64),
}

impl NumberLiteral {
    /// Returns the internal value of the literal.
    pub fn value(&self) -> f64 {
        match self {
            NumberLiteral::Hexadecimal(value) | NumberLiteral::Decimal(value) => *value,
            NumberLiteral::Scientific(value, base) => value * (10f64.powf(*base)),
        }
    }
}

/// Represents any comment.
#[derive(Debug, PartialEq)]
pub enum Comment {
    Single(String),
    Block(String, usize),
}

/// Represents one of the reserved keyword tokens.
#[derive(Debug, PartialEq)]
pub enum Keyword {
    If,
    Else,
    ElseIf,
    Then,
    Function,
    Do,
    For,
    Repeat,
    While,
    Local,
    Break,
    Return,
}

impl Keyword {
    pub(super) fn from_bytes(value: &[u8]) -> Option<Self> {
        Some(match value {
            b"if" => Self::If,
            b"else" => Self::Else,
            b"elseif" => Self::ElseIf,
            b"then" => Self::Then,
            b"function" => Self::Function,
            b"do" => Self::Do,
            b"for" => Self::For,
            b"repeat" => Self::Repeat,
            b"while" => Self::While,
            b"local" => Self::Local,
            b"break" => Self::Break,
            b"return" => Self::Return,
            _ => return None,
        })
    }
}

/// Represents any of the binary or unary operators.
#[derive(Debug, PartialEq)]
pub enum Operator {
    And,
    Or,
    Not,

    Length,

    Equals,
    NotEquals,

    GreaterThan,
    GreaterThanEquals,

    LessThan,
    LessThanEquals,

    Concat,

    Add,
    SubtractOrNegate,
    Multiply,
    FloatDivide,
    FloorDivide,
    Exponential,
    Modulo,

    BitwiseAnd,
    BitwiseOr,
    BitwiseXorOrNegate,
    BitwiseRightShift,
    BitwiseLeftShift,
}

impl Operator {
    pub(super) fn from_bytes(value: &[u8]) -> Option<Self> {
        Some(match value {
            b"and" => Self::And,
            b"or" => Self::Or,
            b"not" => Self::Not,

            b"#" => Self::Length,

            b"==" => Self::Equals,
            b"~=" => Self::NotEquals,

            b">" => Self::GreaterThan,
            b">=" => Self::GreaterThanEquals,

            b"<" => Self::LessThan,
            b"<=" => Self::LessThanEquals,

            b".." => Self::Concat,

            b"+" => Self::Add,
            b"-" => Self::SubtractOrNegate,
            b"*" => Self::Multiply,
            b"/" => Self::FloatDivide,
            b"//" => Self::FloorDivide,
            b"^" => Self::Exponential,
            b"%" => Self::Modulo,

            b"&" => Self::BitwiseAnd,
            b"|" => Self::BitwiseOr,
            b"~" => Self::BitwiseXorOrNegate,
            b">>" => Self::BitwiseRightShift,
            b"<<" => Self::BitwiseLeftShift,

            _ => return None,
        })
    }
}
