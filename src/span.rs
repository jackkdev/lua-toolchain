#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Copy, Clone)]
pub struct Span {
    start: Pos,
    end: Pos,
}

impl Span {
    pub fn new(start: Pos, end: Pos) -> Self {
        Self {
            start,
            end,
        }
    }

    pub fn start(&self) -> Pos {
        self.start
    }

    pub fn end(&self) -> Pos {
        self.end
    }
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Copy, Clone)]
pub struct Pos {
    offset: usize,

    col: usize,
    row: usize,
}

impl Pos {
    pub fn new(offset: usize, col: usize, row: usize) -> Self {
        Self {
            offset,

            col,
            row
        }
    }

    pub fn offset(&self) -> usize {
        self.offset
    }

    pub fn col(&self) -> usize {
        self.col
    }

    pub fn row(&self) -> usize {
        self.row
    }
}

impl Default for Pos {
    fn default() -> Self {
        Self {
            offset: 0,
            
            col: 0,
            row: 0,
        }
    }
}