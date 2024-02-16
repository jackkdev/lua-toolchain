/// Represents a "token" or building block of the language syntax.
#[derive(Debug, PartialEq)]
pub enum Token {
    String(StringLiteral),
    Number(NumberLiteral),
    Boolean(bool),

    Identifier(String),
    Keyword(Keyword),
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
    pub(crate) fn from_bytes(value: &[u8]) -> Option<Keyword> {
        Some(match value {
            b"if" => Keyword::If,
            b"else" => Keyword::Else,
            b"elseif" => Keyword::ElseIf,
            b"then" => Keyword::Then,
            b"function" => Keyword::Function,
            b"do" => Keyword::Do,
            b"for" => Keyword::For,
            b"repeat" => Keyword::Repeat,
            b"while" => Keyword::While,
            b"local" => Keyword::Local,
            b"break" => Keyword::Break,
            b"return" => Keyword::Return,
            _ => return None,
        })
    }
}
