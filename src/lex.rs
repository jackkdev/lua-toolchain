use crate::iter::Iter;
use crate::span::{Pos, Span};
use std::string::FromUtf8Error;
use thiserror::Error;

/// An error type used for [`Lex`].
#[derive(Debug, Error)]
pub enum LexError {
    #[error("UTF-8 Parse Error: {from}")]
    Utf8 {
        #[from]
        from: FromUtf8Error,
    },

    #[error("Unexpected EOF")]
    Eof,

    #[error("Invalid Escape Character: '{value}'")]
    InvalidEscape { value: u8 },

    #[error("Malformed Number: '{value}'")]
    MalformedNumber { value: String },
}

/// A result type used for [`Lex`].
pub type LexResult<T> = Result<T, LexError>;

/// The lexical analyzer implementation.
///
/// This struct wraps a slice of bytes and provides [`Lex::take`] for iterating over tokens.
pub struct Lex<'data> {
    iter: Iter<'data>,
}

impl<'data> Lex<'data> {
    /// Returns a [`Lex`] whose data is sourced from `data`.
    pub fn from_slice(data: &'data [u8]) -> Self {
        Self {
            iter: Iter::from_slice(data),
        }
    }

    /// Returns the next token found in the data.
    pub fn take(&mut self) -> LexResult<Option<(Token, Span)>> {
        // Skip white-space.
        self.iter.take_while(|c| c == b' ');

        let peek = match self.iter.peek() {
            Some(peek) => peek,
            None => return Ok(None),
        };

        // String literals, long string literals.
        if is::quote(peek) {
            return self
                .parse_string_literal()
                .map(|spanned_token| Some(spanned_token));
        }

        // Number literals.
        // if is::number(peek) {
        //     return self.parse_number_literal()
        //         .map(|literal| Some(Token::Number(literal)));
        // }

        // Boolean literals.

        // Keywords, identifiers.
        if is::keyword_or_identifier(peek, false) {
            let start = self.iter.pos();
            let (value, end) = self.iter.take_while(|c| is::keyword_or_identifier(c, true));

            return Ok(Some((
                match Keyword::from_bytes(value.as_slice()) {
                    Some(keyword) => Token::Keyword(keyword),
                    None => Token::Identifier(String::from_utf8(value)?),
                },
                Span::new(start, end),
            )));
        }

        Ok(None)
    }

    fn parse_string_literal(&mut self) -> LexResult<(Token, Span)> {
        let mut value = vec![];

        let start = self.iter.pos();
        let mut end = Pos::default();

        let quote = self.iter.take().unwrap();

        let mut is_escaping = false;

        loop {
            end = self.iter.pos();

            let c = match self.iter.take() {
                Some(c) => c,
                None => return Err(LexError::Eof),
            };

            if is_escaping {
                let real = match c {
                    b'\\' => b'\\',
                    b'\'' => b'\'',
                    b'"' => b'"',
                    b't' => b'\t',
                    b'n' => b'\n',
                    b'r' => b'\r',
                    _ => {
                        return Err(LexError::InvalidEscape { value: c });
                    }
                };

                value.push(real);

                is_escaping = false;

                continue;
            }

            if c == b'\\' {
                is_escaping = true;

                continue;
            }

            if c == quote {
                break;
            }

            value.push(c);
        }

        let value = String::from_utf8(value)?;

        return Ok((
            match quote {
                b'\'' => Token::String(StringLiteral::Single(value)),
                b'\"' => Token::String(StringLiteral::Double(value)),
                _ => panic!("An unknown error has occurred."),
            },
            Span::new(start, end),
        ));
    }

    fn parse_number_literal(&mut self) -> LexResult<NumberLiteral> {
        let (digits, mut end) = self.iter.take_while(is::alphanumeric);

        Ok(NumberLiteral::Decimal(0.0))
    }

    // fn parse_number_literal_old(&mut self) -> LexResult<NumberLiteral> {
    //     let leading = self.iter.take().unwrap();
    //
    //     let next = match self.iter.peek() {
    //         Some(next) => next,
    //         None => {
    //             return Ok(NumberLiteral::Decimal((leading - b'0') as f64));
    //         }
    //     };
    //
    //     // Hexadecimal literals.
    //     if next == b'x' {
    //         self.iter.advance();
    //
    //         let digits = self.iter.take_while(|c| is::hexadecimal(c));
    //
    //         let mut value = 0.0;
    //
    //         for (index, digit) in digits.iter().enumerate() {
    //             let real_index = digits.len() - index - 1;
    //
    //             let n = character_to_number(*digit);
    //
    //             value += n * 16f64.powi(real_index as i32);
    //         }
    //
    //         Ok(NumberLiteral::Hexadecimal(value))
    //     } else { // Decimal, scientific literals.
    //         let mut integer_digits = self.iter.take_while(|c| is::number(c));
    //         integer_digits.insert(0, leading);
    //
    //         let mut value = digits_to_integer(integer_digits.as_slice());
    //
    //         let decimal_digits = match self.iter.peek() {
    //             Some(b'.') => {
    //                 self.iter.advance();
    //                 self.iter.take_while(|c| is::number(c))
    //             }
    //             _ => vec![],
    //         };
    //         value += digits_to_decimal(decimal_digits.as_slice());
    //
    //         Ok(match self.iter.peek() {
    //             Some(b'e' | b'E') => {
    //                 self.iter.advance();
    //
    //                 let sign = match self.iter.peek() {
    //                     Some(b'+') => {
    //                         self.iter.advance();
    //                         1.0
    //                     }
    //                     Some(b'-') => {
    //                         self.iter.advance();
    //                         -1.0
    //                     }
    //                     _ => 1.0,
    //                 };
    //
    //                 let power_digits = self.iter.take_while(|c| is::number(c));
    //                 let power = digits_to_integer(power_digits.as_slice());
    //
    //                 NumberLiteral::Scientific(value, power * sign)
    //             }
    //             _ => NumberLiteral::Decimal(value)
    //         })
    //     }
    // }
}

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
    fn from_bytes(value: &[u8]) -> Option<Keyword> {
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

fn character_to_number(c: u8) -> f64 {
    let value = match c {
        b'0'..=b'9' => c - b'0',
        b'a'..=b'f' => c - b'a' + 10,
        b'A'..=b'F' => c - b'A' + 10,
        _ => 0,
    };

    value as f64
}

fn digits_to_integer(digits: &[u8]) -> f64 {
    let mut value = 0f64;

    for (index, digit) in digits.iter().enumerate() {
        let real_index = digits.len() - index - 1;

        let n = character_to_number(*digit);
        value += n * 10f64.powi(real_index as i32);
    }

    value
}

fn digits_to_decimal(digits: &[u8]) -> f64 {
    let mut value = 0f64;

    for (index, digit) in digits.iter().enumerate() {
        let n = character_to_number(*digit);
        value += n * 10f64.powi(-(index as i32 + 1));
    }

    value
}

/// This private module contains "matching" functions which are loosely used when checking the first
/// character of a word or when attempting to parse the entirety of a word.
mod is {
    pub fn whitespace(c: u8) -> bool {
        match c {
            b' ' | b'\t' => true,
            _ => false,
        }
    }

    pub fn keyword_or_identifier(c: u8, next: bool) -> bool {
        match c {
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => true,
            b'0'..=b'9' => next,
            _ => false,
        }
    }

    pub fn quote(c: u8) -> bool {
        match c {
            b'\'' | b'\"' => true,
            _ => false,
        }
    }

    pub fn long_quote(c: u8) -> bool {
        match c {
            b'[' => true,
            _ => false,
        }
    }

    pub fn number(c: u8) -> bool {
        match c {
            b'0'..=b'9' => true,
            _ => false,
        }
    }

    pub fn hexadecimal(c: u8) -> bool {
        match c {
            b'0'..=b'9' | b'A'..=b'F' | b'a'..=b'f' => true,
            _ => false,
        }
    }

    pub fn alphanumeric(c: u8) -> bool {
        match c {
            b'0'..=b'9' | b'A'..=b'Z' | b'a'..=b'z' => true,
            _ => false,
        }
    }

    pub fn delimiter(c: u8) -> bool {
        match c {
            b']' | b')' | b'+' | b'-' | b'*' | b'/' => true,
            _ => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use anyhow::Result;

    struct Test {
        input: String,
        output: Option<(Token, Span)>,
    }

    fn run_tests(tests: &[Test]) -> Result<()> {
        for test in tests {
            let mut lex = Lex::from_slice(test.input.as_bytes());

            let token = lex.take()?;
            assert_eq!(token, test.output);
        }

        Ok(())
    }

    #[test]
    fn test_lex_keywords() -> Result<()> {
        let tests = vec![
            Test {
                input: String::from("if"),
                output: Some((
                    Token::Keyword(Keyword::If),
                    Span::new(Pos::new(0, 0, 0), Pos::new(1, 1, 0)),
                )),
            },
            Test {
                input: String::from("else"),
                output: Some((
                    Token::Keyword(Keyword::Else),
                    Span::new(Pos::new(0, 0, 0), Pos::new(3, 3, 0)),
                )),
            },
        ];

        run_tests(tests.as_slice())?;

        Ok(())
    }

    #[test]
    fn test_lex_string_literal() -> Result<()> {
        let tests = vec![
            Test {
                input: String::from("\"hello, world!\""),
                output: Some((
                    Token::String(StringLiteral::Double(String::from("hello, world!"))),
                    Span::new(Pos::new(0, 0, 0), Pos::new(14, 14, 0)),
                )),
            },
            Test {
                input: String::from("\"\\\"hello, world!\\\"\""),
                output: Some((
                    Token::String(StringLiteral::Double(String::from("\"hello, world!\""))),
                    Span::new(Pos::new(0, 0, 0), Pos::new(18, 18, 0)),
                )),
            },
            Test {
                input: String::from("\"\\\\\\\\\""),
                output: Some((
                    Token::String(StringLiteral::Double(String::from("\\\\"))),
                    Span::new(Pos::new(0, 0, 0), Pos::new(5, 5, 0)),
                )),
            },
        ];

        run_tests(tests.as_slice())?;

        Ok(())
    }

    // #[test]
    // fn test_lex_number_literal() -> Result<()> {
    //     let tests = vec![
    //         Test {
    //             input: String::from("0xABCD"),
    //             output: Some(Token::Number(NumberLiteral::Hexadecimal(0xABCD as f64))),
    //         },
    //         Test {
    //             input: String::from("0xa012bd"),
    //             output: Some(Token::Number(NumberLiteral::Hexadecimal(0xA012BD as f64))),
    //         },
    //         Test {
    //             input: String::from("15.154"),
    //             output: Some(Token::Number(NumberLiteral::Decimal(15.154))),
    //         },
    //         Test {
    //             input: String::from("15"),
    //             output: Some(Token::Number(NumberLiteral::Decimal(15.0))),
    //         },
    //         Test {
    //             input: String::from("00000000015.0000000000"),
    //             output: Some(Token::Number(NumberLiteral::Decimal(15.0))),
    //         },
    //         Test {
    //             input: String::from("0.45e32"),
    //             output: Some(Token::Number(NumberLiteral::Scientific(0.45, 32.0))),
    //         },
    //         Test {
    //             input: String::from("0.45e+32"),
    //             output: Some(Token::Number(NumberLiteral::Scientific(0.45, 32.0))),
    //         },
    //         Test {
    //             input: String::from("0.45e-32"),
    //             output: Some(Token::Number(NumberLiteral::Scientific(0.45, -32.0))),
    //         },
    //     ];
    //
    //     run_tests(tests.as_slice())?;
    //
    //     Ok(())
    // }
}
