use crate::ir_enhance::ir_builder::*;
use koopa::ir::{builder_traits::*, *};
use log::info;

use self::const_part::*;

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
            Decl::ConstDecl(const_decl) => {},
        }
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
    pub l_or_exp: Box<LOrExp>,
}

impl Exp {
    pub fn build_ir(&self, block_builder: &mut BlockBuilder) -> Value {
        self.l_or_exp.build_ir(block_builder)
    }
}

#[derive(Debug)]
pub enum LOrExp {
    LAndExp(Box<LAndExp>),
    // op always be "||"
    LOrExpOpLAndExp(Box<LOrExp>, Box<LAndExp>),
}

impl LOrExp {
    pub fn build_ir(&self, block_builder: &mut BlockBuilder) -> Value {
        match self {
            LOrExp::LAndExp(l_and_exp) => l_and_exp.build_ir(block_builder),
            LOrExp::LOrExpOpLAndExp(l_or_exp, l_and_exp) => {
                let l_or_exp = l_or_exp.build_ir(block_builder);
                let l_and_exp = l_and_exp.build_ir(block_builder);
                let exp = block_builder
                    .new_value()
                    .binary(BinaryOp::Or, l_or_exp, l_and_exp);
                block_builder.extend([exp]);
                return exp;
            }
        }
    }
}

#[derive(Debug)]
pub enum LAndExp {
    EqExp(Box<EqExp>),
    // op always be "&&"
    LAndExpOpEqExp(Box<LAndExp>, Box<EqExp>),
}

impl LAndExp {
    pub fn build_ir(&self, block_builder: &mut BlockBuilder) -> Value {
        match self {
            LAndExp::EqExp(eq_exp) => eq_exp.build_ir(block_builder),
            LAndExp::LAndExpOpEqExp(l_and_exp, eq_exp) => {
                let l_and_exp = l_and_exp.build_ir(block_builder);
                let eq_exp = eq_exp.build_ir(block_builder);
                let exp = block_builder
                    .new_value()
                    .binary(BinaryOp::And, l_and_exp, eq_exp);
                block_builder.extend([exp]);
                return exp;
            }
        }
    }
}

#[derive(Debug)]
pub enum EqExp {
    RelExp(Box<RelExp>),
    EqExpOpRelExp(Box<EqExp>, EqExpOp, Box<RelExp>),
}

impl EqExp {
    pub fn build_ir(&self, block_builder: &mut BlockBuilder) -> Value {
        match self {
            EqExp::RelExp(rel_exp) => rel_exp.build_ir(block_builder),
            EqExp::EqExpOpRelExp(eq_exp, op, rel_exp) => {
                let eq_exp = eq_exp.build_ir(block_builder);
                let rel_exp = rel_exp.build_ir(block_builder);
                let op = op.clone().into();
                let exp = block_builder.new_value().binary(op, eq_exp, rel_exp);
                block_builder.extend([exp]);
                return exp;
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum EqExpOp {
    Eq, // ==
    Ne, // !=
}

impl Into<BinaryOp> for EqExpOp {
    fn into(self) -> BinaryOp {
        match self {
            EqExpOp::Eq => BinaryOp::Eq,
            EqExpOp::Ne => BinaryOp::NotEq,
        }
    }
}

#[derive(Debug)]
pub enum RelExp {
    AddExp(Box<AddExp>),
    RelExpOpAddExp(Box<RelExp>, RelExpOp, Box<AddExp>),
}

impl RelExp {
    pub fn build_ir(&self, block_builder: &mut BlockBuilder) -> Value {
        match self {
            RelExp::AddExp(add_exp) => add_exp.build_ir(block_builder),
            RelExp::RelExpOpAddExp(rel_exp, op, add_exp) => {
                let rel_exp = rel_exp.build_ir(block_builder);
                let add_exp = add_exp.build_ir(block_builder);
                let op = op.clone().into();
                let exp = block_builder.new_value().binary(op, rel_exp, add_exp);
                block_builder.extend([exp]);
                return exp;
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum RelExpOp {
    Gt, // >
    Lt, // <
    Ge, // >=
    Le, // <=
}

impl Into<BinaryOp> for RelExpOp {
    fn into(self) -> BinaryOp {
        match self {
            RelExpOp::Gt => BinaryOp::Gt,
            RelExpOp::Lt => BinaryOp::Lt,
            RelExpOp::Ge => BinaryOp::Ge,
            RelExpOp::Le => BinaryOp::Le,
        }
    }
}

#[derive(Debug)]
pub enum AddExp {
    MulExp(Box<MulExp>),
    AddExpOpMulExp(Box<AddExp>, AddOp, Box<MulExp>),
}

impl AddExp {
    pub fn build_ir(&self, block_builder: &mut BlockBuilder) -> Value {
        match self {
            AddExp::MulExp(exp) => {
                return exp.build_ir(block_builder);
            }
            AddExp::AddExpOpMulExp(lhs, op, rhs) => {
                let lhs = lhs.build_ir(block_builder);
                let rhs = rhs.build_ir(block_builder);
                let op = op.clone().into();
                let exp = block_builder.new_value().binary(op, lhs, rhs);
                block_builder.extend([exp]);
                return exp;
            }
        }
    }
}

#[derive(Debug)]
pub enum MulExp {
    UnaryExp(Box<UnaryExp>),

    MulExpOpUnaryExp(Box<MulExp>, MulOp, Box<UnaryExp>),
}

impl MulExp {
    pub fn build_ir(&self, block_builder: &mut BlockBuilder) -> Value {
        match self {
            MulExp::UnaryExp(unary) => {
                return unary.build_ir(block_builder);
            }
            MulExp::MulExpOpUnaryExp(lhs, op, rhs) => {
                let lhs = lhs.build_ir(block_builder);
                let rhs = rhs.build_ir(block_builder);
                let op = op.clone().into();
                let exp = block_builder.new_value().binary(op, lhs, rhs);
                block_builder.extend([exp]);
                return exp;
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum MulOp {
    Mul, // *
    Div, // /
    Mod, // %
}

impl Into<BinaryOp> for MulOp {
    fn into(self) -> BinaryOp {
        match self {
            MulOp::Mul => BinaryOp::Mul,
            MulOp::Div => BinaryOp::Div,
            MulOp::Mod => BinaryOp::Mod,
        }
    }
}

#[derive(Debug, Clone)]
pub enum AddOp {
    Plus,  // +
    Minus, // -
}

impl Into<BinaryOp> for AddOp {
    fn into(self) -> BinaryOp {
        match self {
            AddOp::Plus => BinaryOp::Add,
            AddOp::Minus => BinaryOp::Sub,
        }
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
    LVal(LVal)
}

impl PrimaryExp {
    pub fn build_ir(&self, block_builder: &mut BlockBuilder) -> Value {
        match self {
            PrimaryExp::Exp(exp) => exp.build_ir(block_builder),
            PrimaryExp::Number(v) => block_builder.new_value().integer(*v),
            PrimaryExp::LVal(_) => todo!(),
        }
    }
}

#[derive(Debug)]
pub struct LVal {
    pub ident: String,
}