use std::collections::HashMap;

#[derive(Debug)]
pub struct Mem {
    pub mem: Vec<Option<i128>>,
    pub malloced: HashMap<usize, usize>
}

impl Mem {
    pub fn new() -> Self {
        Mem { mem: vec![], malloced: HashMap::new() }
    }

    pub fn malloc(&mut self, size: usize, init: Option<i128>) -> usize {
        let (mut count, mut start) = (0, 0);
        for (idx, elem) in self.mem.iter_mut().enumerate() {
            match elem {
                None => {
                    count += 1;
                    if count == size {
                        self.malloced.insert(start, size);
                        for i in self.mem[start..start+size].iter_mut() {
                            *i = Some(init.unwrap_or(0));
                        }
                        return start;
                    }
                },
                Some(_) => {
                    count = 0;
                    start = idx + 1;
                },
            }
        }
        start = self.mem.len();
        self.malloced.insert(start, size);
        self.mem.append(&mut vec![Some(init.unwrap_or(0)); size]);
        return start;
    }

    pub fn free(&mut self, start: usize) -> bool {
        match self.malloced.get(&start) {
            Some(size) => {
                for i in self.mem[start..start+size].iter_mut() {
                    *i = None;
                }
                self.malloced.remove(&start);
                true
            },
            None => false,
        }
    }
}