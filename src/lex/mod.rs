/// Implements the lexical analyzer via [`Lex`].
pub mod token;
mod util;

use thiserror::Error;
use util::{is_alphanumeric, is_hexadecimal};

use crate::{
    iter::Iter,
    lex::{
        token::{NumberLiteral, StringLiteral, Token},
        util::{
            character_to_number, characters_to_decimal, characters_to_integer, is_decimal,
            is_whitespace,
        },
        LexError::{MalformedLongString, MalformedNumber, Utf8},
    },
    span::{Pos, Span},
};

/// Error type for [`Lex`].
#[derive(Debug, Error, PartialEq)]
pub enum LexError {
    #[error("Failed to parse UTF-8 data from file.")]
    Utf8 { span: Span },

    #[error("Encountered a malformed string.")]
    MalformedString { span: Span },

    #[error("Encountered a malformed long string.")]
    MalformedLongString { span: Span },

    #[error("Encountered an invalid escape sequence.")]
    InvalidEscapeSequence { span: Span },

    #[error("Encountered a malformed number.")]
    MalformedNumber { span: Span },
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
        self.iter.take_while(is_whitespace);

        // We'll peek ahead at the next two characters.
        let c1 = self.iter.peekn(0);
        let c2 = self.iter.peekn(1);

        match (c1.map(|c| c.0), c2.map(|c| c.0)) {
            (Some(b'\'' | b'\"'), _) => self.parse_string_literal().map(|inner| Some(inner)),
            (Some(b'['), Some(b'[') | Some(b'=')) => {
                self.parse_long_string_literal().map(|inner| Some(inner))
            }
            (Some(b'.'), Some(b'0'..=b'9')) | (Some(b'0'..=b'9'), _) => {
                self.parse_number_literal().map(|inner| Some(inner))
            }
            _ => Ok(None),
        }
    }

    fn parse_string_literal(&mut self) -> LexResult<(Token, Span)> {
        let mut value = vec![];

        let (quote_char, start) = self.iter.take().unwrap();
        let mut end = Pos::default();

        let mut escaping = false;
        let mut escaping_start = Pos::default();

        loop {
            let (char, pos) = match self.iter.take() {
                Some((b'\n', pos)) => {
                    return Err(LexError::MalformedString {
                        span: Span::new(start, pos),
                    })
                }
                Some(c) => c,
                None => {
                    return Err(LexError::MalformedString {
                        span: Span::new(start, end),
                    })
                }
            };

            if escaping {
                let real_char = match char {
                    b'\\' => b'\\',
                    b'\'' => b'\'',
                    b'"' => b'"',
                    b't' => b'\t',
                    b'n' => b'\n',
                    b'r' => b'\r',
                    _ => {
                        return Err(LexError::InvalidEscapeSequence {
                            span: Span::new(escaping_start, pos),
                        });
                    }
                };

                value.push(real_char);

                escaping = false;
            } else {
                end = pos;

                if char == quote_char {
                    break;
                }

                if char == b'\\' {
                    escaping_start = pos;
                    escaping = true;
                } else {
                    value.push(char);
                }
            }
        }

        let span = Span::new(start, end);

        let value = String::from_utf8(value).map_err(|_| LexError::Utf8 { span })?;

        return Ok((
            match quote_char {
                b'\'' => Token::String(StringLiteral::Single(value)),
                b'\"' => Token::String(StringLiteral::Double(value)),
                _ => panic!("An unknown error has occurred."),
            },
            span,
        ));
    }

    fn parse_long_string_literal(&mut self) -> LexResult<(Token, Span)> {
        let c1 = self.iter.take().unwrap();
        let c2 = self.iter.peek().unwrap();

        let mut n = 0;
        if c2.0 == b'=' {
            n = self.iter.take_while(|c| c == b'=').0.len();
        }

        let pos = self.iter.pos();
        match self.iter.take() {
            Some((b'[', _)) => {}
            _ => {
                return Err(MalformedLongString {
                    span: Span::new(c1.1, pos),
                })
            }
        }

        let mut characters = vec![];

        let mut end = pos;

        loop {
            let c = match self.iter.take() {
                Some(c) => c,
                None => {
                    return Err(MalformedLongString {
                        span: Span::new(c1.1, end),
                    })
                }
            };

            end = c.1;

            if c.0 == b']' {
                let mut maybe_end = true;

                for i in 0..n {
                    match self.iter.peekn(i) {
                        Some((b'=', _)) => {}
                        _ => {
                            maybe_end = false;
                        }
                    }
                }

                if maybe_end {
                    match self.iter.peekn(n) {
                        Some((b']', _)) => {
                            let (_, ending_span) = self.iter.take_many(n + 1);
                            end = ending_span.end();

                            break;
                        }
                        _ => {}
                    }
                }
            }

            characters.push(c.0);
        }

        let span = Span::new(c1.1, end);

        Ok((
            Token::String(StringLiteral::Long(
                String::from_utf8(characters).map_err(|_| Utf8 { span })?,
                n,
            )),
            span,
        ))
    }

    fn parse_number_literal(&mut self) -> LexResult<(Token, Span)> {
        let c1 = self.iter.peek().unwrap();

        let c2 = match self.iter.peekn(1) {
            Some(c2) => c2,
            None => {
                return Ok((
                    Token::Number(NumberLiteral::Decimal(character_to_number(c1.0))),
                    Span::new(c1.1, c1.1),
                ))
            }
        };

        match (c1.0, c2.0) {
            (b'0', b'x') => self.parse_hexadecimal_literal(),
            (_, b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' | b'.') => self.parse_decimal_literal(),
            _ => {
                return Ok((
                    Token::Number(NumberLiteral::Decimal(character_to_number(c1.0))),
                    Span::new(c1.1, c1.1),
                ))
            }
        }
    }

    fn parse_hexadecimal_literal(&mut self) -> LexResult<(Token, Span)> {
        let (_, leading_span) = self.iter.take_many(2);

        let (characters, characters_span) = self.iter.take_while(is_alphanumeric);

        let end = if characters.len() == 0 {
            leading_span.end()
        } else {
            characters_span.end()
        };

        let span = Span::new(leading_span.start(), end);

        if characters.len() == 0 {
            return Err(MalformedNumber { span });
        }

        let mut in_decimal = false;

        let mut integer_digits = vec![];
        let mut decimal_digits = vec![];

        for character in characters {
            if (!is_hexadecimal(character) && !character == b'.')
                || (character == b'.' && in_decimal)
            {
                return Err(MalformedNumber { span });
            }

            if character == b'.' {
                in_decimal = true;

                continue;
            }

            if in_decimal {
                decimal_digits.push(character);
            } else {
                integer_digits.push(character);
            }
        }

        if in_decimal && decimal_digits.len() == 0 {
            return Err(MalformedNumber { span });
        }

        let value = characters_to_integer(16f64, &integer_digits)
            + characters_to_decimal(16f64, &decimal_digits);
        Ok((Token::Number(NumberLiteral::Hexadecimal(value)), span))
    }

    fn parse_decimal_literal(&mut self) -> LexResult<(Token, Span)> {
        let (characters, span) = self.iter.take_while(is_alphanumeric);

        let mut in_decimal = false;
        let mut integer_digits = vec![];
        let mut decimal_digits = vec![];

        for character in characters {
            if (!is_decimal(character) && !character == b'.') || (character == b'.' && in_decimal) {
                return Err(MalformedNumber { span });
            }

            if character == b'.' {
                in_decimal = true;
                continue;
            }

            if in_decimal {
                decimal_digits.push(character);
            } else {
                integer_digits.push(character);
            }
        }

        let value = characters_to_integer(10f64, &integer_digits)
            + characters_to_decimal(10f64, &decimal_digits);
        Ok((Token::Number(NumberLiteral::Decimal(value)), span))
    }
}

#[cfg(test)]
mod tests {
    use super::{token::*, *};
    use crate::lex::token::Token::Number;

    struct Test {
        input: String,
        output: Result<Option<(Token, Span)>, LexError>,
    }

    fn run_tests(tests: &[Test]) {
        for test in tests {
            let mut lex = Lex::from_slice(test.input.as_bytes());

            let result = lex.take();
            assert_eq!(result, test.output);
        }
    }

    // #[test]
    fn test_lex_keywords() {
        let tests = vec![
            Test {
                input: String::from("if"),
                output: Ok(Some((
                    Token::Keyword(Keyword::If),
                    Span::new(Pos::new(0, 0, 0), Pos::new(1, 1, 0)),
                ))),
            },
            Test {
                input: String::from("else"),
                output: Ok(Some((
                    Token::Keyword(Keyword::Else),
                    Span::new(Pos::new(0, 0, 0), Pos::new(3, 3, 0)),
                ))),
            },
        ];

        run_tests(tests.as_slice());
    }

    #[test]
    fn test_lex_string_literal() {
        let tests = vec![
            Test {
                input: String::from("\"hello, world!\""),
                output: Ok(Some((
                    Token::String(StringLiteral::Double(String::from("hello, world!"))),
                    Span::new(Pos::new(0, 0, 0), Pos::new(14, 14, 0)),
                ))),
            },
            Test {
                input: String::from("\"\\\"hello, world!\\\"\""),
                output: Ok(Some((
                    Token::String(StringLiteral::Double(String::from("\"hello, world!\""))),
                    Span::new(Pos::new(0, 0, 0), Pos::new(18, 18, 0)),
                ))),
            },
            Test {
                input: String::from("\"\\\\\\\\\""),
                output: Ok(Some((
                    Token::String(StringLiteral::Double(String::from("\\\\"))),
                    Span::new(Pos::new(0, 0, 0), Pos::new(5, 5, 0)),
                ))),
            },
            Test {
                input: String::from("\"hello\n"),
                output: Err(LexError::MalformedString {
                    span: Span::new(Pos::new(0, 0, 0), Pos::new(6, 6, 0)),
                }),
            },
            Test {
                input: String::from("\"hello"),
                output: Err(LexError::MalformedString {
                    span: Span::new(Pos::new(0, 0, 0), Pos::new(5, 5, 0)),
                }),
            },
        ];

        run_tests(tests.as_slice());
    }

    #[test]
    fn test_lex_long_string_literal() {
        let tests = vec![
            Test {
                input: String::from("[[example]]"),
                output: Ok(Some((
                    Token::String(StringLiteral::Long(String::from("example"), 0)),
                    Span::new(Pos::new(0, 0, 0), Pos::new(10, 10, 0)),
                ))),
            },
            Test {
                input: String::from("[===[example]=]]===]"),
                output: Ok(Some((
                    Token::String(StringLiteral::Long(String::from("example]=]"), 3)),
                    Span::new(Pos::new(0, 0, 0), Pos::new(19, 19, 0)),
                ))),
            },
            Test {
                input: String::from("[["),
                output: Err(MalformedLongString {
                    span: Span::new(Pos::new(0, 0, 0), Pos::new(1, 1, 0)),
                }),
            },
        ];

        run_tests(tests.as_slice());
    }

    #[test]
    fn test_lex_number_literal() {
        let tests = vec![
            Test {
                input: String::from("0xABCD"),
                output: Ok(Some((
                    Number(NumberLiteral::Hexadecimal(0xABCD as f64)),
                    Span::new(Pos::new(0, 0, 0), Pos::new(5, 5, 0)),
                ))),
            },
            Test {
                input: String::from("0x00.1"),
                output: Ok(Some((
                    Number(NumberLiteral::Hexadecimal(0.0625)),
                    Span::new(Pos::new(0, 0, 0), Pos::new(5, 5, 0)),
                ))),
            },
            Test {
                input: String::from("0."),
                output: Ok(Some((
                    Number(NumberLiteral::Decimal(0.0)),
                    Span::new(Pos::new(0, 0, 0), Pos::new(1, 1, 0)),
                ))),
            },
            Test {
                input: String::from("0x.1"),
                output: Ok(Some((
                    Number(NumberLiteral::Hexadecimal(0.0625)),
                    Span::new(Pos::new(0, 0, 0), Pos::new(3, 3, 0)),
                ))),
            },
            Test {
                input: String::from("12.257"),
                output: Ok(Some((
                    Number(NumberLiteral::Decimal(12.257)),
                    Span::new(Pos::new(0, 0, 0), Pos::new(5, 5, 0)),
                ))),
            },
            Test {
                input: String::from("0x"),
                output: Err(MalformedNumber {
                    span: Span::new(Pos::new(0, 0, 0), Pos::new(1, 1, 0)),
                }),
            },
            Test {
                input: String::from("0x."),
                output: Err(MalformedNumber {
                    span: Span::new(Pos::new(0, 0, 0), Pos::new(2, 2, 0)),
                }),
            },
        ];

        run_tests(tests.as_slice());
    }
}
