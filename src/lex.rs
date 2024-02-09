use crate::iter::Iter;

pub struct Lex<'data> {
    iter: Iter<'data, u8>,
}

impl<'data> Lex<'data> {
    pub fn from_slice(data: &'data [u8]) -> Self {
        Self {
            iter: Iter::from_slice(data),
        }
    }

    pub fn take(&mut self) -> Option<Token> {
        // Skip white-space.
        self.iter.take_while(|c| *c == b' ');

        let peek = match self.iter.peek() {
            Some(peek) => peek,
            None => return None,
        };

        // Keywords, identifiers.
        if is::keyword_or_identifier(peek) {
            let value = self.iter.take_while(is::keyword_or_identifier);

            match
        }

        None
    }
}

pub enum Token {
    String(StringLiteral),
    Number(NumberLiteral),
    Boolean(bool),

    Keyword(Keyword),
}

pub enum StringLiteral {
    Single(String),
    Double(String),
    Long(String, usize),
}

pub enum NumberLiteral {
    Decimal(f64),
    Hexadecimal(f64),
    Scientific(f64, f64),
}

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

}

mod is {
    pub fn whitespace(c: &u8) -> bool {
        match *c {
            b' ' | b'\t' => true,
            _ => false,
        }
    }

    pub fn keyword_or_identifier(c: &u8) -> bool {
        match *c {
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => true,
            _ => false,
        }
    }
}