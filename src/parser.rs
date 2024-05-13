use koopa::ir::Program;

use crate::{ast, sysy};
use std::{fs::read_to_string, io::Result};



pub struct SysyParser {
    mode: String,
    file: String,

    // result
    pub ast: Option<ast::CompUnit>,
    ir: Option<koopa::ir::Program>,
}

impl SysyParser {
    pub fn new(mode: String, input_file_name: String) -> Result<Self> {
        let file = read_to_string(input_file_name)?;

        Ok(Self {
            mode: mode,
            file: file,

            ast: Option::None,
            ir: Option::None,
        })
    }

    pub fn generate_ast(&mut self) {
        let ast = sysy::CompUnitParser::new().parse(&self.file).unwrap();
        self.ast = Some(ast);
    }

    pub fn get_ir(&self) -> Option<Program> {
        self.ast.as_ref().map(|ast| ast.build_ir())
    }
}