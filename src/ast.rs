use crate::ir_enhance::ir_builder::*;
use koopa::ir::{builder_traits::*, *};
use log::info;

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
    pub stmt: Stmt,
}

impl Block {
    pub fn build_ir(&self, func_builder: &mut FunctionBuilder) {
        let mut block_builder = func_builder.new_block(Some("@entry".to_owned()));

        self.stmt.build_ir(&mut block_builder);
    }
}

#[derive(Debug)]
pub enum UnaryOp {
    Plus,
    Minus,
    Not,
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

#[derive(Debug)]
pub struct Exp {
    pub unary_exp: Box<UnaryExp>,
}

impl Exp {
    pub fn build_ir(&self, block_builder: &mut BlockBuilder) -> Value {
        self.unary_exp.build_ir(block_builder)
    }
}

#[derive(Debug)]
pub enum UnaryExp {
    PrimaryExp(Box<PrimaryExp>),
    UnaryOpAndExp(UnaryOp, Box<UnaryExp>),
}

impl UnaryExp {
    pub fn build_ir(&self, block_builder: &mut BlockBuilder) -> Value {
        match self {
            UnaryExp::PrimaryExp(pe) => {
                return pe.build_ir(block_builder);
            }
            UnaryExp::UnaryOpAndExp(op, op_exp) => {
                let op_exp = op_exp.build_ir(block_builder);
                match op {
                    UnaryOp::Plus => {
                        return op_exp;
                    }
                    UnaryOp::Minus => {
                        let lhs = block_builder.new_value().integer(0);
                        let minus = block_builder.new_value().binary(BinaryOp::Sub, lhs, op_exp);
                        block_builder.extend([minus]);
                        return minus;
                    }
                    UnaryOp::Not => {
                        let rhs = block_builder.new_value().integer(0);
                        let not = block_builder.new_value().binary(BinaryOp::Eq, op_exp, rhs);
                        block_builder.extend([not]);
                        return not;
                    }
                }
            }
        }
    }
}

#[derive(Debug)]
pub enum PrimaryExp {
    // ( Expr )
    Exp(Box<Exp>),
    Number(i32),
}

impl PrimaryExp {
    pub fn build_ir(&self, block_builder: &mut BlockBuilder) -> Value {
        match self {
            PrimaryExp::Exp(exp) => exp.build_ir(block_builder),
            PrimaryExp::Number(v) => block_builder.new_value().integer(*v),
        }
    }
}
