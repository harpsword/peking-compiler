use koopa::ir::Program;

use crate::{ast, sysy};
use std::{fs::read_to_string, io::Result, path::PathBuf};

pub mod semantic_analysis;
pub mod symbol_table;

pub struct SysyCompiler {
    file: String,

    // interval result
    const_symbols: symbol_table::ConstTable,

    // result
    pub ast: Option<ast::CompUnit>,
    ir: Option<koopa::ir::Program>,
}

impl SysyCompiler {
    pub fn new(input_file_name: PathBuf) -> Result<Self> {
        let file = read_to_string(input_file_name)?;

        Ok(Self {
            file: file,

            const_symbols: symbol_table::ConstTable::new(),

            ast: Option::None,
            ir: Option::None,
        })
    }

    pub fn semantic_analysis(&mut self) {
        // const value computation, should return a const value table
    }

    pub fn generate_ast(&mut self) {
        let ast = sysy::CompUnitParser::new().parse(&self.file).unwrap();
        self.ast = Some(ast);
    }

    pub fn get_ir(&self) -> Option<Program> {
        self.ast.as_ref().map(|ast| ast.build_ir())
    }
}
