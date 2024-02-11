use crate::span::Pos;

pub struct Iter<'data> {
    data: &'data [u8],

    index: usize,

    col: usize,
    row: usize,
}

impl<'data> Iter<'data>
{
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

    pub fn take(&mut self) -> Option<u8> {
        if self.index >= self.data.len() {
            None
        } else {
            let item = self.data[self.index].clone();
            self.advance();

            Some(item)
        }
    }

    pub fn peek(&self) -> Option<u8> {
        if self.index >= self.data.len() {
            None
        } else {
            Some(self.data[self.index])
        }
    }

    pub fn take_while<F>(&mut self, f: F) -> (Vec<u8>, Pos)
        where
            F: Fn(u8) -> bool,
    {
        let mut items = vec![];
        let mut end = Pos::default();

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

        (items, end)
    }


    pub fn col(&self) -> usize {
        self.col
    }

    pub fn row(&self) -> usize {
        self.row
    }

    pub fn pos(&self) -> Pos {
        Pos::new(self.index, self.col, self.row)
    }
}
