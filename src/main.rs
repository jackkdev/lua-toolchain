use anyhow::Result;

use crate::lex::Lex;

mod lex;
mod span;

fn main() -> Result<()> {
    let data = include_str!("../example.lua");

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
