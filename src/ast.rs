
// use koopa::ir::{builder::{BasicBlockBuilder, ValueBuilder}, entities::BasicBlockData, BasicBlock, Function, FunctionData, Program, Type};

use koopa::ir::{builder::EntityInfoQuerier, builder_traits::*, *};

#[derive(Debug)]
pub struct CompUnit {
    pub func_def: FuncDef,
}

impl CompUnit {
    pub fn build_ir(&self) -> Program {
        let mut program = Program::new();

        self.func_def.build_ir(&mut program);
        
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
    pub fn build_ir(&self, program: &mut Program) {
        let func = program.new_func(
            FunctionData::with_param_names("@".to_owned()+&self.ident, vec![], Type::get_i32())
        );
        let mut func_data = program.func_mut(func);
        self.block.build_ir(&mut func_data);
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
    pub fn build_ir(&self, func_data: &mut FunctionData) {

        let bb = func_data.dfg_mut().new_bb().basic_block(Some("@entry".to_owned()));
        func_data.layout_mut().bbs_mut().push_key_back(bb);

        let stmt = func_data.dfg_mut().new_value().integer(self.stmt.num);
        let ret = func_data.dfg_mut().new_value().ret(Some(stmt));

        func_data.layout_mut().bb_mut(bb).insts_mut().extend([ret]);
    }
}

#[derive(Debug)]
pub struct Stmt {
    pub num: i32,
}
