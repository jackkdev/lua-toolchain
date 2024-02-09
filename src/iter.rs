pub struct Iter<'data, T> {
    index: usize,
    data: &'data [T],
}

impl<'data, T> Iter<'data, T>
    where
        T: Clone,
{
    pub fn from_slice(data: &'data [T]) -> Self {
        Self { index: 0, data }
    }

    pub fn take(&mut self) -> Option<T> {
        if self.index >= self.data.len() {
            None
        } else {
            let item = self.data[self.index].clone();
            self.index += 1;

            Some(item)
        }
    }

    pub fn peek(&self) -> Option<&T> {
        if self.index >= self.data.len() {
            None
        } else {
            Some(&self.data[self.index])
        }
    }

    pub fn take_while<F>(&mut self, f: F) -> Vec<T>
        where
            F: Fn(&T) -> bool,
    {
        let mut items = vec![];

        loop {
            if self.index >= self.data.len() {
                break;
            }

            let item = &self.data[self.index];

            if !f(item) {
                break;
            }

            self.index += 1;

            items.push(item.clone())
        }

        items
    }
}
