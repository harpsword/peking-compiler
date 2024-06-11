use std::collections::HashMap;

use koopa::ir::Value;

#[derive(Debug, Clone)]
pub enum Symbol {
    Const(i32),
    Var(Value),
}

#[derive(Debug)]
pub(crate) struct SymbolTable {
    symbol_table: HashMap<String, Symbol>,
}

impl SymbolTable {
    pub(crate) fn new() -> Self {
        Self {
            symbol_table: HashMap::new(),
        }
    }

    pub(crate) fn get_symbol(&self, name: &str) -> Option<&Symbol> {
        self.symbol_table.get(name)
    }

    pub(crate) fn insert_const(&mut self, name: String, value: i32) {
        self.symbol_table.insert(name, Symbol::Const(value));
    }

    pub(crate) fn get_const(&self, name: &str) -> Option<i32> {
        let symbol = self.symbol_table.get(name)?;

        if let Symbol::Const(value) = symbol {
            return Some(*value);
        }
        None
    }

    pub(crate) fn insert_var(&mut self, name: String, value: Value) {
        self.symbol_table.insert(name, Symbol::Var(value));
    }

    pub(crate) fn get_var(&self, name: &str) -> Option<Value> {
        let symbol = self.symbol_table.get(name)?;
        if let Symbol::Var(value) = symbol {
            return Some(*value);
        }
        None
    }
}
