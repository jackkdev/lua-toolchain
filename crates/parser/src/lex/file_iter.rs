use crate::span::{Pos, Span};

#[derive(Debug)]
pub struct FileIter<'data> {
    data: &'data [u8],

    index: usize,

    col: usize,
    row: usize,
}

impl<'data> FileIter<'data> {
    pub fn from_slice(data: &'data [u8]) -> Self {
        Self {
            data,

            index: 0,

            col: 0,
            row: 0,
        }
    }

    pub fn advance(&mut self) {
        if self.data[self.index] == b'\n' {
            self.row += 1;
            self.col = 0;
        } else {
            self.col += 1;
        }

        self.index += 1;
    }

    pub fn take_many(&mut self, n: usize) -> (Vec<u8>, Span) {
        let mut items = vec![];

        let start = self.pos();
        let mut end = start;

        for _ in 0..n {
            let c = match self.take() {
                Some(c) => c,
                None => break,
            };

            end = c.1;

            items.push(c.0);
        }

        (items, Span::new(start, end))
    }

    pub fn take(&mut self) -> Option<(u8, Pos)> {
        if self.index >= self.data.len() {
            None
        } else {
            let item = self.data[self.index].clone();
            let pos = self.pos();

            self.advance();

            Some((item, pos))
        }
    }

    pub fn peekn(&self, n: usize) -> Option<u8> {
        if self.index + n >= self.data.len() {
            None
        } else {
            Some(self.data[self.index + n])
        }
    }

    pub fn peek(&self) -> Option<u8> {
        self.peekn(0)
    }

    pub fn take_while<F>(&mut self, f: F) -> (Vec<u8>, Span)
    where
        F: Fn(u8) -> bool,
    {
        let mut items = vec![];

        let start = self.pos();
        let mut end = start;

        loop {
            if self.index >= self.data.len() {
                break;
            }

            let item = self.data[self.index];

            if !f(item) {
                break;
            }

            end = self.pos();

            self.advance();

            items.push(item.clone())
        }

        (items, Span::new(start, end))
    }

    pub fn pos(&self) -> Pos {
        Pos::new(self.index, self.col, self.row)
    }
}
