//! Implements the lexical analyzer via [`Lex`].

/// Contains the file iter implementation.
mod file_iter;

/// Provides the [`Token`] type alongside various implementations for the enumerations.
pub mod token;

use thiserror::Error;

pub use crate::lex::token::*;
use crate::{
    lex::{
        file_iter::FileIter,
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

    #[error("Encountered a malformed number: {message}")]
    MalformedNumber { span: Span, message: String },
}

/// A result type used for [`Lex`].
pub type LexResult<T> = Result<T, LexError>;

/// The lexical analyzer implementation.
///
/// This struct wraps a slice of bytes and provides [`Lex::take`] for iterating over tokens.
#[derive(Debug)]
pub struct Lex<'data> {
    iter: FileIter<'data>,
    buffer: Vec<(Token, Span)>,
}

impl<'data> Lex<'data> {
    /// Returns a [`Lex`] whose data is sourced from `data`.
    pub fn from_slice(data: &'data [u8]) -> Self {
        Self {
            iter: FileIter::from_slice(data),
            buffer: vec![],
        }
    }

    pub fn peek(&mut self) -> LexResult<Option<(Token, Span)>> {
        if self.buffer.len() > 0 {
            Ok(Some(self.buffer[0].clone()))
        } else {
            let spanned_token = self.take_internal()?;

            let spanned_token = match spanned_token {
                Some(inner) => {
                    self.buffer.push(inner.clone());
                    inner
                }
                None => return Ok(None),
            };

            Ok(Some(spanned_token))
        }
    }

    pub fn take(&mut self) -> LexResult<Option<(Token, Span)>> {
        if self.buffer.len() > 0 {
            let token = self.buffer.remove(0);
            Ok(Some(token))
        } else {
            self.take_internal()
        }
    }

    fn take_internal(&mut self) -> LexResult<Option<(Token, Span)>> {
        // Skip white-space.
        self.iter.take_while(|c| c.is_ascii_whitespace());

        // We'll peek ahead at the next two characters.
        let c1 = self.iter.peekn(0);
        let c2 = self.iter.peekn(1);

        match (c1, c2) {
            (Some(b'\'' | b'\"'), _) => self.parse_string_literal().map(|inner| Some(inner)),
            (Some(b'['), Some(b'[' | b'=')) => {
                self.parse_long_string_literal().map(|inner| Some(inner))
            }
            (Some(b'.'), Some(b'0'..=b'9')) | (Some(b'0'..=b'9'), _) => {
                self.parse_number_literal().map(|inner| Some(inner))
            }
            (Some(b'-'), Some(b'-')) => self.parse_comment().map(|inner| Some(inner)),
            (Some(b'a'..=b'z' | b'A'..=b'Z' | b'_'), _) => {
                let (contents, span) = self
                    .iter
                    .take_while(|c| c.is_ascii_alphanumeric() || c == b'_');

                let value = String::from_utf8(contents).map_err(|_| Utf8 { span })?;

                match value.as_str() {
                    "true" => return Ok(Some((Token::BooleanLiteral(true), span))),
                    "false" => return Ok(Some((Token::BooleanLiteral(false), span))),
                    "nil" => return Ok(Some((Token::NilLiteral, span))),
                    _ => {}
                }

                if let Some(keyword) = Keyword::from_bytes(value.as_bytes()) {
                    return Ok(Some((Token::Keyword(keyword), span)));
                }

                Ok(Some((Token::Identifier(Name(value)), span)))
            }
            (Some(b'.'), _) => {
                if Some(b'.') == c2 {
                    if Some(b'.') == self.iter.peekn(2) {
                        let (_, span) = self.iter.take_many(3);
                        Ok(Some((Token::Other(Other::Vararg), span)))
                    } else {
                        let (_, span) = self.iter.take_many(2);
                        Ok(Some((Token::Other(Other::Concat), span)))
                    }
                } else {
                    let (_, pos) = self.iter.take().unwrap();
                    Ok(Some((Token::Other(Other::Dot), Span::new(pos, pos))))
                }
            }
            (Some(c1c), _) => {
                let (_, pos) = self.iter.take().unwrap();

                if let Some(c2c) = c2 {
                    if let Some(operator) = Other::from_bytes(c1c, c2c) {
                        let (_, pos2) = self.iter.take().unwrap();
                        let t2_span = Span::new(pos, pos2);

                        return Ok(Some((Token::Other(operator), t2_span)));
                    }
                }

                let t1_span = Span::new(pos, pos);
                if let Some(operator) = Other::from_byte(c1c) {
                    return Ok(Some((Token::Other(operator), t1_span)));
                }

                let span = Span::new(pos, pos);
                if let Some(inner) = match c1c {
                    _ => None,
                } {
                    return Ok(Some((inner, span)));
                }

                Ok(None)
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
                    });
                }
                Some(c) => c,
                None => {
                    return Err(LexError::MalformedString {
                        span: Span::new(start, end),
                    });
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
                b'\'' => Token::StringLiteral(StringLiteral::Single(value)),
                b'\"' => Token::StringLiteral(StringLiteral::Double(value)),
                _ => panic!("An unknown error has occurred."),
            },
            span,
        ));
    }

    fn parse_long_string_literal(&mut self) -> LexResult<(Token, Span)> {
        let c1 = self.iter.take().unwrap();
        let c2 = self.iter.peek().unwrap();

        let mut n = 0;
        if c2 == b'=' {
            n = self.iter.take_while(|c| c == b'=').0.len();
        }

        let pos = self.iter.pos();
        match self.iter.take() {
            Some((b'[', _)) => {}
            _ => {
                return Err(MalformedLongString {
                    span: Span::new(c1.1, pos),
                });
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
                    });
                }
            };

            end = c.1;

            if c.0 == b']' {
                let mut maybe_end = true;

                for i in 0..n {
                    match self.iter.peekn(i) {
                        Some(b'=') => {}
                        _ => {
                            maybe_end = false;
                        }
                    }
                }

                if maybe_end {
                    match self.iter.peekn(n) {
                        Some(b']') => {
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
            Token::StringLiteral(StringLiteral::Long(
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
                let (_, pos) = self.iter.take().unwrap();

                return Ok((
                    Token::NumberLiteral(NumberLiteral::Decimal(character_to_number(c1))),
                    Span::new(pos, pos),
                ));
            }
        };

        match (c1, c2) {
            (b'0', b'x') => self.parse_hexadecimal_literal(),
            (_, b'0'..=b'9' | b'a'..=b'z' | b'A'..=b'Z' | b'.') => self.parse_decimal_literal(),
            _ => {
                let (_, pos) = self.iter.take().unwrap();

                return Ok((
                    Token::NumberLiteral(NumberLiteral::Decimal(character_to_number(c1))),
                    Span::new(pos, pos),
                ));
            }
        }
    }

    fn parse_hexadecimal_literal(&mut self) -> LexResult<(Token, Span)> {
        let (_, leading_span) = self.iter.take_many(2);

        let (characters, characters_span) = self
            .iter
            .take_while(|c| c.is_ascii_alphanumeric() || c == b'.' || c == b'_');

        let end = if characters.len() == 0 {
            leading_span.end()
        } else {
            characters_span.end()
        };

        let span = Span::new(leading_span.start(), end);

        if characters.len() == 0 {
            return Err(MalformedNumber {
                span,
                message: String::from("Missing hexadecimal digits."),
            });
        }

        let mut in_decimal = false;

        let mut integer_digits = vec![];
        let mut decimal_digits = vec![];

        for character in characters {
            if (!character.is_ascii_hexdigit() && !character == b'.')
                || (character == b'.' && in_decimal)
            {
                return Err(MalformedNumber {
                    span,
                    message: format!("Unexpected character, '{}'.", character as char),
                });
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
            return Err(MalformedNumber {
                span,
                message: String::from("Trailing decimal point."),
            });
        }

        let value = characters_to_integer(16f64, &integer_digits)
            + characters_to_decimal(16f64, &decimal_digits);
        Ok((
            Token::NumberLiteral(NumberLiteral::Hexadecimal(value)),
            span,
        ))
    }

    fn parse_decimal_literal(&mut self) -> LexResult<(Token, Span)> {
        // Read up until not alphanumeric, or if scientific, attempt to read sign.
        let mut chars = vec![];

        let start = self.iter.pos();
        let mut end = start;

        let mut accepts_sign = false;

        loop {
            let ch = match self.iter.peek() {
                Some(ch) => ch,
                None => break,
            };

            if !(ch.is_ascii_alphanumeric()
                || ch == b'_'
                || ch == b'.'
                || (accepts_sign && ch == b'-'))
            {
                break;
            }

            if accepts_sign {
                accepts_sign = false;
            }

            if ch == b'E' || ch == b'e' {
                accepts_sign = true;
            }

            let (ch, pos) = self.iter.take().unwrap();
            end = pos;

            chars.push(ch);
        }

        let span = Span::new(start, end);

        // Index into the characters vec.
        let mut i = 0;

        // Parse the integer portion of the number.
        let mut integer_digits = vec![];

        let mut try_decimal = false;

        loop {
            if i == chars.len() {
                break;
            }

            let ch = chars[i];
            i += 1;

            if ch == b'.' {
                try_decimal = true;
                break;
            }

            if ch == b'e' {
                break;
            }

            if !ch.is_ascii_digit() {
                return Err(MalformedNumber {
                    span,
                    message: format!("{} is not a valid ascii digit", ch as char),
                });
            }

            integer_digits.push(ch);
        }

        // Parse the decimal portion of the number.
        let mut decimal_digits = vec![];

        loop {
            if !try_decimal {
                break;
            }

            if i == chars.len() {
                break;
            }

            let ch = chars[i];
            i += 1;

            if ch == b'e' || ch == b'E' {
                break;
            }

            if !ch.is_ascii_digit() {
                return Err(MalformedNumber {
                    span,
                    message: format!("{} is not a valid ascii digit", ch as char),
                });
            }

            decimal_digits.push(ch);
        }

        // Parse the scientific portion of the number.
        let mut scientific_digits = vec![];
        let mut sign = 1.0;
        let mut first = true;

        loop {
            if i == chars.len() {
                break;
            }

            let ch = chars[i];
            i += 1;

            if first {
                first = false;

                let mut cont = true;
                match ch {
                    b'-' => sign = -1.0,
                    b'+' => sign = 1.0,
                    _ => {
                        cont = false;
                    }
                };

                if cont {
                    continue;
                }
            }

            if !ch.is_ascii_digit() {
                return Err(MalformedNumber {
                    span,
                    message: format!("{} is not a valid ascii digit", ch as char),
                });
            }

            scientific_digits.push(ch);
        }

        let result = characters_to_integer(10.0, &integer_digits)
            + characters_to_decimal(10.0, &decimal_digits);

        Ok((
            if scientific_digits.len() > 0 {
                Token::NumberLiteral(NumberLiteral::Scientific(
                    result,
                    characters_to_integer(10.0, &scientific_digits) * sign,
                ))
            } else {
                Token::NumberLiteral(NumberLiteral::Decimal(result))
            },
            span,
        ))
    }

    fn parse_comment(&mut self) -> LexResult<(Token, Span)> {
        // Check for a block/long comment delimiter.
        let mut is_block = false;
        let mut block_n = 0;

        match self.iter.peekn(2) {
            Some(b'[') => {
                let mut i = 3;
                loop {
                    let c = self.iter.peekn(i);
                    i += 1;

                    match c {
                        Some(b'=') => block_n += 1,
                        Some(b'[') => {
                            is_block = true;
                            break;
                        }
                        _ => {
                            break;
                        }
                    }
                }
            }
            _ => {}
        };

        if is_block {
            let mut contents = vec![];

            let start = self.iter.pos();
            let mut end = start;

            self.iter.take_many(block_n + 4);

            loop {
                let c = match self.iter.take() {
                    Some(c) => c,
                    None => {
                        return Err(MalformedLongString {
                            span: Span::new(start, end),
                        });
                    }
                };

                end = c.1;

                if c.0 == b']' {
                    let mut maybe_end = true;

                    for i in 0..block_n {
                        match self.iter.peekn(i) {
                            Some(b'=') => {}
                            _ => {
                                maybe_end = false;
                            }
                        }
                    }

                    if maybe_end {
                        match self.iter.peekn(block_n) {
                            Some(b']') => {
                                let (_, ending_span) = self.iter.take_many(block_n + 1);
                                end = ending_span.end();

                                break;
                            }
                            _ => {}
                        }
                    }
                }

                contents.push(c.0);
            }

            let span = Span::new(start, end);

            Ok((
                Token::Comment(Comment::Block(
                    String::from_utf8(contents).map_err(|_| Utf8 { span })?,
                    block_n,
                )),
                span,
            ))
        } else {
            let (_, leading_span) = self.iter.take_many(2);
            let (contents, contents_span) = self.iter.take_while(|c| c != b'\n');
            let span = Span::new(leading_span.start(), contents_span.end());

            Ok((
                Token::Comment(Comment::Single(
                    String::from_utf8(contents).map_err(|_| Utf8 { span })?,
                )),
                span,
            ))
        }
    }
}

pub(super) fn character_to_number(c: u8) -> f64 {
    let value = match c {
        b'0'..=b'9' => c - b'0',
        b'a'..=b'f' => c - b'a' + 10,
        b'A'..=b'F' => c - b'A' + 10,
        _ => 0,
    };

    value as f64
}

pub(super) fn characters_to_integer(base: f64, digits: &[u8]) -> f64 {
    let mut value = 0f64;

    for (index, digit) in digits.iter().enumerate() {
        let real_index = digits.len() - index - 1;

        let n = character_to_number(*digit);
        value += n * base.powi(real_index as i32);
    }

    value
}

pub(super) fn characters_to_decimal(base: f64, digits: &[u8]) -> f64 {
    let mut value = 0f64;

    for (index, digit) in digits.iter().enumerate() {
        let n = character_to_number(*digit);
        value += n * base.powf(-(index as f64 + 1.0));
    }

    value
}

#[cfg(test)]
mod tests {
    use super::*;

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

    #[test]
    fn test_lex_string_literal() {
        let tests = vec![
            Test {
                input: String::from("\"hello, world!\""),
                output: Ok(Some((
                    Token::StringLiteral(StringLiteral::Double(String::from("hello, world!"))),
                    Span::new(Pos::new(0, 0, 0), Pos::new(14, 14, 0)),
                ))),
            },
            Test {
                input: String::from("\"\\\"hello, world!\\\"\""),
                output: Ok(Some((
                    Token::StringLiteral(StringLiteral::Double(String::from("\"hello, world!\""))),
                    Span::new(Pos::new(0, 0, 0), Pos::new(18, 18, 0)),
                ))),
            },
            Test {
                input: String::from("\"\\\\\\\\\""),
                output: Ok(Some((
                    Token::StringLiteral(StringLiteral::Double(String::from("\\\\"))),
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
                    Token::StringLiteral(StringLiteral::Long(String::from("example"), 0)),
                    Span::new(Pos::new(0, 0, 0), Pos::new(10, 10, 0)),
                ))),
            },
            Test {
                input: String::from("[===[example]=]]===]"),
                output: Ok(Some((
                    Token::StringLiteral(StringLiteral::Long(String::from("example]=]"), 3)),
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
                    Token::NumberLiteral(NumberLiteral::Hexadecimal(0xABCD as f64)),
                    Span::new(Pos::new(0, 0, 0), Pos::new(5, 5, 0)),
                ))),
            },
            Test {
                input: String::from("0x00.1"),
                output: Ok(Some((
                    Token::NumberLiteral(NumberLiteral::Hexadecimal(0.0625)),
                    Span::new(Pos::new(0, 0, 0), Pos::new(5, 5, 0)),
                ))),
            },
            Test {
                input: String::from("0.1e-1"),
                output: Ok(Some((
                    Token::NumberLiteral(NumberLiteral::Scientific(0.1, -1.0)),
                    Span::new(Pos::new(0, 0, 0), Pos::new(5, 5, 0)),
                ))),
            },
            Test {
                input: String::from("0x.1"),
                output: Ok(Some((
                    Token::NumberLiteral(NumberLiteral::Hexadecimal(0.0625)),
                    Span::new(Pos::new(0, 0, 0), Pos::new(3, 3, 0)),
                ))),
            },
            Test {
                input: String::from("12.257"),
                output: Ok(Some((
                    Token::NumberLiteral(NumberLiteral::Decimal(12.257)),
                    Span::new(Pos::new(0, 0, 0), Pos::new(5, 5, 0)),
                ))),
            },
            Test {
                input: String::from("10e1"),
                output: Ok(Some((
                    Token::NumberLiteral(NumberLiteral::Scientific(10f64, 1f64)),
                    Span::new(Pos::new(0, 0, 0), Pos::new(3, 3, 0)),
                ))),
            },
        ];

        run_tests(tests.as_slice());
    }

    #[test]
    fn test_lex_single_comment() {
        let tests = vec![Test {
            input: String::from("--example"),
            output: Ok(Some((
                Token::Comment(Comment::Single(String::from("example"))),
                Span::new(Pos::new(0, 0, 0), Pos::new(8, 8, 0)),
            ))),
        }];

        run_tests(tests.as_slice());
    }

    #[test]
    fn test_lex_multi_comment() {
        let tests = vec![
            Test {
                input: String::from("--[[example]]"),
                output: Ok(Some((
                    Token::Comment(Comment::Block(String::from("example"), 0)),
                    Span::new(Pos::new(0, 0, 0), Pos::new(12, 12, 0)),
                ))),
            },
            Test {
                input: String::from("--[==[example]=]]==]"),
                output: Ok(Some((
                    Token::Comment(Comment::Block(String::from("example]=]"), 2)),
                    Span::new(Pos::new(0, 0, 0), Pos::new(19, 19, 0)),
                ))),
            },
        ];
    }

    #[test]
    fn test_all_tokens() {
        let data = include_str!("../../test.lua");
        let mut lex = Lex::from_slice(data.as_bytes());

        macro_rules! ident {
            ($ident:expr) => {
                Token::Identifier(Name(String::from($ident)))
            };
        }

        macro_rules! dec {
            ($value:expr) => {
                Token::NumberLiteral(NumberLiteral::Decimal($value))
            };
        }

        macro_rules! hex {
            ($value:expr) => {
                Token::NumberLiteral(NumberLiteral::Hexadecimal($value))
            };
        }

        macro_rules! sci {
            ($value:expr, $exp:expr) => {
                Token::NumberLiteral(NumberLiteral::Scientific($value, $exp))
            };
        }

        macro_rules! str {
            ($value:expr) => {
                Token::StringLiteral(StringLiteral::Double(String::from($value)))
            };
        }

        macro_rules! lstr {
            ($value:expr, $size:expr) => {
                Token::StringLiteral(StringLiteral::Long(String::from($value), $size))
            };
        }

        macro_rules! kw {
            ($v:ident) => {
                Token::Keyword(Keyword::$v)
            };
        }

        macro_rules! ot {
            ($v:ident($value:expr)) => {
                Token::Other(Other::$v($value))
            };
            ($v:ident) => {
                Token::Other(Other::$v)
            };
        }

        #[rustfmt::skip]
        let expected = vec![
            kw!(Local), ident!("t"), ot!(Assign), ot!(CurlyBrace(true)),
            dec!(1.0), ot!(Comma), dec!(2.0), ot!(SemiColon), dec!(3.0), ot!(CurlyBrace(false)),
            
            kw!(Local), ident!("var1"), ot!(Assign), dec!(0.0),
            ot!(Add), dec!(0.0), ot!(SubtractOrNegate),
            sci!(0.0, -1.0), ot!(FloatDivide), hex!(0.0),
            ot!(Multiply), hex!(0.0), ot!(Modulo),
            dec!(0.12345), ot!(Exponential), sci!(0.31416000000000005, 1.0),
            ot!(Add), ot!(Length), ident!("t"),
            ot!(Add), ident!("t"), ot!(Dot), ident!("a"),
            ot!(Add), ident!("t"), ot!(SquareBrace(true)), str!("a"),
            ot!(SquareBrace(false)), ot!(Add), ident!("t"), ot!(Colon),
            ident!("a"), ot!(Paren(true)), ot!(Paren(false)),

            kw!(Local), ident!("var2"), ot!(Assign), str!("Hello, world!"),
            ot!(Concat), lstr!("Hello, world!", 0), 
            ot!(Concat), lstr!("Hello, world!", 1),
            ot!(Concat), lstr!("Hello, world!", 4),
            
            kw!(Local), ident!("var3"), ot!(Assign), kw!(Not),
            ot!(Paren(true)), Token::BooleanLiteral(true), ot!(Equals), 
            Token::BooleanLiteral(false), ot!(Paren(false)), kw!(And),
            ot!(Paren(true)), dec!(0.0), ot!(GreaterThanEquals), dec!(0.0),
            ot!(Paren(false)), kw!(Or), ot!(Paren(true)), dec!(0.0), 
            ot!(LessThanEquals), dec!(0.0), ot!(Paren(false)), 
            kw!(And), ot!(Paren(true)), dec!(0.0), 
            ot!(GreaterThan), dec!(0.0), ot!(Paren(false)),
            kw!(Or), ot!(Paren(true)), dec!(0.0),
            ot!(LessThan), dec!(0.0), ot!(Paren(false)),
            kw!(Or),
            ot!(Paren(true)), Token::BooleanLiteral(false), ot!(NotEquals),
            Token::BooleanLiteral(true), ot!(Paren(false)),
            
            kw!(If), ident!("t"), kw!(Then),
            kw!(ElseIf), ident!("t"), ot!(Add), dec!(1.0), kw!(Then),
            kw!(Else),
            kw!(End),
            
            kw!(For), ident!("k"), ot!(Comma), ident!("v"), kw!(In), ident!("ipairs"),
            ot!(Paren(true)), ident!("t"), ot!(Paren(false)), kw!(Do),
            kw!(Break),
            kw!(End),
            
            kw!(While), Token::BooleanLiteral(true), kw!(Do),
            kw!(Return),
            kw!(End),
            
            kw!(Repeat),
            kw!(Until),
            Token::BooleanLiteral(true),
            
            kw!(Function), ident!("example"), ot!(Paren(true)),
            ident!("v1"), ot!(Comma), ident!("v2"), ot!(Comma), ident!("v3"), ot!(Paren(false)),
            kw!(End),
        ];

        let mut i = 0;
        loop {
            let result = lex.take().expect("failed to take token");
            match result {
                Some((token, _)) => assert_eq!(token, expected[i]),
                None => break,
            };
            i += 1;
        }
    }
}
