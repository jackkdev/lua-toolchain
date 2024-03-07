use anyhow::Result;
use tracing_subscriber::{fmt, prelude::*, EnvFilter};

pub mod lex;
pub mod parse;
pub mod span;

use crate::lex::Lex;

fn main() -> Result<()> {
    tracing_subscriber::registry()
        .with(EnvFilter::from_default_env())
        .with(fmt::layer())
        .init();

    let data = include_str!("../../../example.lua");

    let mut lex = Lex::from_slice(data.as_bytes());

    loop {
        let token = lex.take()?;

        match token {
            Some((token, _)) => println!("{token:?}"),
            None => break,
        }
    }

    Ok(())
}
