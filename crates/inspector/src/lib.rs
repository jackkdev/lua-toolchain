use log::{info, Level};
use serde::{Deserialize, Serialize};
use wasm_bindgen::prelude::*;
use parser::{lex::Lex};

#[wasm_bindgen]
pub fn init() {
    console_log::init_with_level(Level::Debug).expect("failed to init log");
    console_error_panic_hook::set_once();
}

#[wasm_bindgen]
pub fn lex(input: String) -> Result<JsValue, JsValue> {
    let mut lex = Lex::from_slice(input.as_bytes());
    let mut tokens = vec![];

    loop {
        match lex.take().expect("failed to take token") {
            Some(token) => tokens.push(token),
            None => break,
        };
    }

    Ok(serde_wasm_bindgen::to_value(&tokens)?)
}