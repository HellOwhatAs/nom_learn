use std::collections::HashMap;

#[derive(Debug)]
pub struct Mem<T: Clone + Copy> {
    pub mem: Vec<Option<T>>,
    pub malloced: HashMap<usize, usize>,
}

impl<T: Clone + Copy> Mem<T> {
    pub fn new() -> Self {
        Mem {
            mem: vec![None],
            malloced: HashMap::new(),
        }
    }

    pub fn malloc(&mut self, size: usize, init: T) -> usize {
        let (mut count, mut start) = (0, 1);
        for (idx, elem) in self.mem[1..].iter_mut().enumerate() {
            match elem {
                None => {
                    count += 1;
                    if count == size {
                        self.malloced.insert(start, size);
                        for i in self.mem[start..start + size].iter_mut() {
                            *i = Some(init);
                        }
                        return start;
                    }
                }
                Some(_) => {
                    count = 0;
                    start = idx + 1;
                }
            }
        }
        start = self.mem.len();
        self.malloced.insert(start, size);
        self.mem.append(&mut vec![Some(init); size]);
        return start;
    }

    pub fn free(&mut self, start: usize) -> bool {
        match self.malloced.get(&start) {
            Some(size) => {
                for i in self.mem[start..start + size].iter_mut() {
                    *i = None;
                }
                self.malloced.remove(&start);
                true
            }
            None => false,
        }
    }
}
