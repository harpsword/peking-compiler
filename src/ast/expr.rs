use crate::ir_enhance::ir_builder::*;
use koopa::ir::{builder_traits::*, *};

use super::{AstNode, Traversal, TraversalStep};

#[derive(Debug)]
pub struct Exp {
    pub l_or_exp: Box<LOrExp>,
}

impl Traversal for Exp {
    fn traversal(&self, sink: &mut dyn FnMut(&TraversalStep)) {
        sink(&TraversalStep::Enter(AstNode::Exp(self)));
        self.l_or_exp.traversal(sink);
        sink(&TraversalStep::Leave(AstNode::Exp(self)));
    }
}

#[derive(Debug)]
pub enum LOrExp {
    LAndExp(Box<LAndExp>),
    // op always be "||"
    LOrExpOpLAndExp(Box<LOrExp>, Box<LAndExp>),
}

impl Traversal for LOrExp {
    fn traversal(&self, sink: &mut dyn FnMut(&TraversalStep)) {
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
}

#[derive(Debug)]
pub enum LAndExp {
    EqExp(Box<EqExp>),
    // op always be "&&"
    LAndExpOpEqExp(Box<LAndExp>, Box<EqExp>),
}

impl Traversal for LAndExp {
    fn traversal(&self, sink: &mut dyn FnMut(&TraversalStep)) {
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
}

#[derive(Debug)]
pub enum EqExp {
    RelExp(Box<RelExp>),
    EqExpOpRelExp(Box<EqExp>, EqExpOp, Box<RelExp>),
}

impl Traversal for EqExp {
    fn traversal(&self, sink: &mut dyn FnMut(&TraversalStep)) {
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

impl Traversal for RelExp {
    fn traversal(&self, sink: &mut dyn FnMut(&TraversalStep)) {
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

impl Traversal for AddExp {
    fn traversal(&self, sink: &mut dyn FnMut(&TraversalStep)) {
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
}

#[derive(Debug)]
pub enum MulExp {
    UnaryExp(Box<UnaryExp>),

    MulExpOpUnaryExp(Box<MulExp>, MulOp, Box<UnaryExp>),
}

impl Traversal for MulExp {
    fn traversal(&self, sink: &mut dyn FnMut(&TraversalStep)) {
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
    FuncCall(Box<FuncCall>),
}

impl Traversal for UnaryExp {
    fn traversal(&self, sink: &mut dyn FnMut(&TraversalStep)) {
        sink(&TraversalStep::Enter(AstNode::UnaryExp(self)));
        match self {
            UnaryExp::PrimaryExp(pe) => pe.traversal(sink),
            UnaryExp::UnaryOpAndExp(op, exp) => {
                exp.traversal(sink);
            }
            UnaryExp::FuncCall(func_call) => func_call.traversal(sink),
        }
        sink(&TraversalStep::Leave(AstNode::UnaryExp(self)));
    }
}

#[derive(Debug)]
pub struct FuncCall {
    pub ident: String,
    pub args: Vec<Box<Exp>>,
}

impl Traversal for FuncCall {
    fn traversal(&self, sink: &mut dyn FnMut(&TraversalStep)) {
        sink(&TraversalStep::Enter(AstNode::FuncCall(self)));
        for arg in self.args.iter() {
            arg.traversal(sink);
        }
        sink(&TraversalStep::Leave(AstNode::FuncCall(self)));
    }
}

#[derive(Debug)]
pub enum PrimaryExp {
    // ( Expr )
    Exp(Box<Exp>),
    Number(i32),
    LVal(LVal),
}

impl Traversal for PrimaryExp {
    fn traversal(&self, sink: &mut dyn FnMut(&TraversalStep)) {
        sink(&TraversalStep::Enter(AstNode::PrimaryExp(self)));
        match self {
            PrimaryExp::Exp(exp) => exp.traversal(sink),
            PrimaryExp::Number(n) => {
                sink(&TraversalStep::Enter(AstNode::Number(n)));
                sink(&TraversalStep::Leave(AstNode::Number(n)));
            }
            PrimaryExp::LVal(l_val) => {
                l_val.traversal(sink);
            }
        }
        sink(&TraversalStep::Leave(AstNode::PrimaryExp(self)));
    }
}

#[derive(Debug)]
pub struct LVal {
    pub ident: String,
}

impl Traversal for LVal {
    fn traversal(&self, sink: &mut dyn FnMut(&TraversalStep)) {
        sink(&TraversalStep::Enter(AstNode::LVal(self)));
        sink(&TraversalStep::Leave(AstNode::LVal(self)));
    }
}
