use std::os::unix::process;

use koopa::ir::{builder::BasicBlockBuilder, entities::BasicBlockData, BasicBlock, Function, FunctionData, Program, Type};



#[derive(Debug)]
pub struct CompUnit {
    pub func_def: FuncDef,
}

impl CompUnit {
    pub fn build_ir(&self) -> Program {
        let mut program = Program::new();
        let func = self.func_def.build_ir();

        program.new_func(func);
        
        return program;
    }
}

#[derive(Debug)]
pub struct FuncDef {
    pub func_type: FuncType,
    pub ident: String,
    pub block: Block,
}

impl FuncDef {
    pub fn build_ir(&self) -> FunctionData {
        FunctionData::with_param_names("@".to_owned()+&self.ident, vec![], Type::get_i32())
    }
}

#[derive(Debug)]
pub enum FuncType {
    Int,
}

#[derive(Debug)]
pub struct Block {
    pub stmt: Stmt,
}

impl Block {
    pub fn build_ir(&self) -> BasicBlockData {
        unimplemented!()
    }
}

#[derive(Debug)]
pub struct Stmt {
    pub num: i32,
}
