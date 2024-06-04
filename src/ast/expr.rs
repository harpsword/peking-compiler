use crate::ir_enhance::ir_builder::*;
use koopa::ir::{builder_traits::*, *};

use super::{AstNode, TraversalStep};

#[derive(Debug)]
pub struct Exp {
    pub l_or_exp: Box<LOrExp>,
}

impl Exp {
    pub fn traversal(&self, sink: &mut dyn FnMut(&TraversalStep)) {
        sink(&TraversalStep::Enter(AstNode::Exp(self)));
        self.l_or_exp.traversal(sink);
        sink(&TraversalStep::Leave(AstNode::Exp(self)));
    }

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
    pub fn traversal(&self, sink: &mut dyn FnMut(&TraversalStep)) {
        sink(&TraversalStep::Enter(AstNode::LOrExp(self)));
        match self {
            LOrExp::LAndExp(l_and_exp) => l_and_exp.traversal(sink),
            LOrExp::LOrExpOpLAndExp(l_or_exp, l_and_exp) => {
                l_or_exp.traversal(sink);
                l_and_exp.traversal(sink);
            }
        }
        sink(&TraversalStep::Leave(AstNode::LOrExp(self)));
    }

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
    pub fn traversal(&self, sink: &mut dyn FnMut(&TraversalStep)) {
        sink(&TraversalStep::Enter(AstNode::LAndExp(self)));
        match self {
            LAndExp::EqExp(eq_exp) => eq_exp.traversal(sink),
            LAndExp::LAndExpOpEqExp(l_and_exp, eq_exp) => {
                l_and_exp.traversal(sink);
                eq_exp.traversal(sink);
            }
        }
        sink(&TraversalStep::Leave(AstNode::LAndExp(self)));
    }

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
    pub fn traversal(&self, sink: &mut dyn FnMut(&TraversalStep)) {
        sink(&TraversalStep::Enter(AstNode::EqExp(self)));
        match self {
            EqExp::RelExp(rel_exp) => rel_exp.traversal(sink),
            EqExp::EqExpOpRelExp(eq_exp, op, rel_exp) => {
                eq_exp.traversal(sink);
                rel_exp.traversal(sink);
            }
        }
        sink(&TraversalStep::Leave(AstNode::EqExp(self)));
    }

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
    pub fn traversal(&self, sink: &mut dyn FnMut(&TraversalStep)) {
        sink(&TraversalStep::Enter(AstNode::RelExp(self)));
        match self {
            RelExp::AddExp(add_exp) => add_exp.traversal(sink),
            RelExp::RelExpOpAddExp(rel_exp, op, add_exp) => {
                rel_exp.traversal(sink);
                add_exp.traversal(sink);
            }
        }
        sink(&TraversalStep::Leave(AstNode::RelExp(self)));
    }

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
    pub fn traversal(&self, sink: &mut dyn FnMut(&TraversalStep)) {
        sink(&TraversalStep::Enter(AstNode::AddExp(self)));
        match self {
            AddExp::MulExp(mul_exp) => mul_exp.traversal(sink),
            AddExp::AddExpOpMulExp(add_exp, op, mul_exp) => {
                add_exp.traversal(sink);
                mul_exp.traversal(sink);
            }
        }
        sink(&TraversalStep::Leave(AstNode::AddExp(self)));
    }

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
    pub fn traversal(&self, sink: &mut dyn FnMut(&TraversalStep)) {
        sink(&TraversalStep::Enter(AstNode::MulExp(self)));
        match self {
            MulExp::UnaryExp(unary_exp) => unary_exp.traversal(sink),
            MulExp::MulExpOpUnaryExp(mul_exp, op, unary_exp) => {
                mul_exp.traversal(sink);
                unary_exp.traversal(sink);
            }
        }
        sink(&TraversalStep::Leave(AstNode::MulExp(self)));
    }

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
pub enum UnaryOp {
    Plus,
    Minus,
    Not,
}

#[derive(Debug)]
pub enum UnaryExp {
    PrimaryExp(Box<PrimaryExp>),
    UnaryOpAndExp(UnaryOp, Box<UnaryExp>),
}

impl UnaryExp {
    pub fn traversal(&self, sink: &mut dyn FnMut(&TraversalStep)) {
        sink(&TraversalStep::Enter(AstNode::UnaryExp(self)));
        match self {
            UnaryExp::PrimaryExp(pe) => pe.traversal(sink),
            UnaryExp::UnaryOpAndExp(op, exp) => {
                exp.traversal(sink);
            }
        }
        sink(&TraversalStep::Leave(AstNode::UnaryExp(self)));
    }

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
    LVal(LVal),
}

impl PrimaryExp {
    pub fn traversal(&self, sink: &mut dyn FnMut(&TraversalStep)) {
        sink(&TraversalStep::Enter(AstNode::PrimaryExp(self)));
        match self {
            PrimaryExp::Exp(exp) => exp.traversal(sink),
            PrimaryExp::Number(n) => {
                sink(&TraversalStep::Enter(AstNode::Number(n)));
                sink(&TraversalStep::Leave(AstNode::Number(n)));
            }
            PrimaryExp::LVal(l) => l.traversal(sink),
        }
        sink(&TraversalStep::Leave(AstNode::PrimaryExp(self)));
    }

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

impl LVal {
    pub fn traversal(&self, sink: &mut dyn FnMut(&TraversalStep)) {
        sink(&TraversalStep::Enter(AstNode::LVal(self)));
        sink(&TraversalStep::Leave(AstNode::LVal(self)));
    }
}