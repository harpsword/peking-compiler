use std::collections::HashMap;


pub(crate) struct ConstTable {
    const_table: HashMap<String, i32>,
}

impl ConstTable {
    pub(crate) fn new() -> Self {
        Self {
            const_table: HashMap::new(),
        }
    }

    pub(crate) fn insert_const(&mut self, name: String, value: i32) {
        self.const_table.insert(name, value);
    }

    pub(crate) fn get_const(&self, name: &str) -> Option<i32> {
        self.const_table.get(name).copied()
    }
}