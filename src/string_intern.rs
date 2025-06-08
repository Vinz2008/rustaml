use rustc_hash::FxHashMap;

// TODO : use this (will need code for debug displaying, look at the code for the flatten AST, because they have the same problem)
// TODO : optimize this (https://matklad.github.io/2020/03/22/fast-simple-rust-interner.html)

pub struct Interner {
    map : FxHashMap<String, u32>,
    strs : Vec<String>,
}

#[derive(Debug)]
pub struct StringRef(u32);

impl Interner {
    pub fn intern(&mut self, name : &str) -> StringRef {
        if let Some(idx) = self.map.get(name) {
            return StringRef(*idx);
        }

        let idx = self.strs.len() as u32;
        self.map.insert(name.to_owned(), idx);
        self.strs.push(name.to_owned());

        StringRef(idx)
    }

    pub fn lookup(&self, idx : StringRef) -> &str {
        self.strs[idx.0 as usize].as_str()
    }
}