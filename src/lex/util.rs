/// Utility functions for the lexer.

pub fn is_whitespace(c: u8) -> bool {
    match c {
        b' ' | b'\t' => true,
        _ => false,
    }
}

pub fn is_keyword_or_identifier(c: u8, next: bool) -> bool {
    match c {
        b'a'..=b'z' | b'A'..=b'Z' | b'_' => true,
        b'0'..=b'9' => next,
        _ => false,
    }
}

pub fn is_quote(c: u8) -> bool {
    match c {
        b'\'' | b'\"' => true,
        _ => false,
    }
}

pub fn is_decimal(c: u8) -> bool {
    match c {
        b'0'..=b'9' => true,
        _ => false,
    }
}

pub fn is_alphanumeric(c: u8) -> bool {
    match c {
        b'0'..=b'9' | b'A'..=b'Z' | b'a'..=b'z' | b'.' => true,
        _ => false,
    }
}

pub fn is_hexadecimal(c: u8) -> bool {
    match c {
        b'0'..=b'9' | b'A'..=b'F' | b'a'..=b'f' => true,
        _ => false,
    }
}

pub fn character_to_number(c: u8) -> f64 {
    let value = match c {
        b'0'..=b'9' => c - b'0',
        b'a'..=b'f' => c - b'a' + 10,
        b'A'..=b'F' => c - b'A' + 10,
        _ => 0,
    };

    value as f64
}

pub fn characters_to_integer(base: f64, digits: &[u8]) -> f64 {
    let mut value = 0f64;

    for (index, digit) in digits.iter().enumerate() {
        let real_index = digits.len() - index - 1;

        let n = character_to_number(*digit);
        value += n * base.powi(real_index as i32);
    }

    value
}

pub fn characters_to_decimal(base: f64, digits: &[u8]) -> f64 {
    let mut value = 0f64;

    for (index, digit) in digits.iter().enumerate() {
        let n = character_to_number(*digit);
        value += n * base.powi(-(index as i32 + 1));
    }

    value
}
