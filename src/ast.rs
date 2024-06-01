use crate::ir_enhance::ir_builder::*;
use koopa::ir::{builder_traits::*, *};
use log::info;

use self::const_part::*;
use self::expr::*;

pub mod const_part;
pub mod expr;

#[derive(Debug)]
pub struct CompUnit {
    pub func_def: FuncDef,
}

impl CompUnit {
    pub fn build_ir(&self) -> Program {
        let mut program_builder = ProgramBuilder::new();

        self.func_def.build_ir(&mut program_builder);

        return program_builder.build();
    }
}

#[derive(Debug)]
pub struct FuncDef {
    pub func_type: FuncType,
    pub ident: String,
    pub block: Block,
}

impl FuncDef {
    pub fn build_ir(&self, program_builder: &mut ProgramBuilder) {
        let func_data =
            FunctionData::with_param_names("@".to_owned() + &self.ident, vec![], Type::get_i32());

        let mut func_builder = program_builder.new_function(func_data);

        self.block.build_ir(&mut func_builder);
    }
}

#[derive(Debug)]
pub enum FuncType {
    Int,
}

#[derive(Debug)]
pub struct Block {
    pub block_items: Vec<BlockItem>,
}

impl Block {
    pub fn build_ir(&self, func_builder: &mut FunctionBuilder) {
        let mut block_builder = func_builder.new_block(Some("@entry".to_owned()));

        for block_item in self.block_items.iter() {
            block_item.build_ir(&mut block_builder);
        }
    }
}

#[derive(Debug)]
pub enum BlockItem {
    Stmt(Stmt),
    Decl(Decl),
}

impl BlockItem {
    pub fn build_ir(&self, block_builder: &mut BlockBuilder) {
        match self {
            BlockItem::Stmt(stmt) => stmt.build_ir(block_builder),
            BlockItem::Decl(decl) => decl.build_ir(block_builder),
        }
    }
}

#[derive(Debug)]
pub enum Decl {
    ConstDecl(ConstDecl),
}

impl Decl {
    pub fn build_ir(&self, block_builder: &mut BlockBuilder) {
        match self {
            Decl::ConstDecl(const_decl) => {}
        }
    }
}

#[derive(Debug)]
pub struct Stmt {
    pub exp: Box<Exp>,
}

impl Stmt {
    pub fn build_ir(&self, block_builder: &mut BlockBuilder) {
        // currently only `return xx` is supported

        let return_value = self.exp.build_ir(block_builder);

        info!("return value: {:?}", return_value);

        let ret = block_builder.new_value().ret(Some(return_value));
        block_builder.extend([ret]);
    }
}
