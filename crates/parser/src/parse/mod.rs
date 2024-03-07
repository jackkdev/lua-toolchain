pub mod ast;

use ast::*;
use thiserror::Error;

use crate::lex::{Lex, LexError};

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("An error has occurred while performing lexical analysis: {from}")]
    Lex {
        #[from]
        from: LexError,
    },
}

pub type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug)]
pub struct Parser<'input> {
    lex: Lex<'input>,
}

impl<'input> Parser<'input> {
    pub fn new(lex: Lex<'input>) -> Self {
        Self { lex }
    }

    pub fn run(&self) -> ParseResult<Chunk> {
        self.parse_chunk()
    }

    fn parse_chunk(&self) -> ParseResult<Chunk> {
        let mut chunk = Chunk::default();

        loop {
            self.parse_statement();
        }
    }

    fn parse_statement(&self) -> ParseResult<Statement> {
        todo!();
    }
}
