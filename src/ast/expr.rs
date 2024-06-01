use crate::{ir_enhance::ir_builder::*, main};
use koopa::ir::{builder_traits::*, *};

trait ValueQuery {
    fn get_value(&self, name: &str) -> Option<i32>;
}

#[derive(Debug)]
pub struct Exp {
    pub l_or_exp: Box<LOrExp>,
}

impl Exp {
    pub fn build_ir(&self, block_builder: &mut BlockBuilder) -> Value {
        self.l_or_exp.build_ir(block_builder)
    }

    pub fn get_value<T: ValueQuery>(&self, vq: &T) -> i32 {
        self.l_or_exp.get_value(vq)
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

    pub fn get_value<T: ValueQuery>(&self, vq: &T) -> i32 {
        match self {
            LOrExp::LAndExp(l_and_exp) => l_and_exp.get_value(vq),
            LOrExp::LOrExpOpLAndExp(l_or_exp, l_and_exp) => {
                let lhs = l_or_exp.get_value(vq);
                let rhs = l_and_exp.get_value(vq);
                (lhs != 0 || rhs != 0) as i32
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
    pub fn get_value<T: ValueQuery>(&self, vq: &T) -> i32 {
        match self {
            LAndExp::EqExp(eq_exp) => eq_exp.get_value(vq),
            LAndExp::LAndExpOpEqExp(l_and_exp, eq_exp) => {
                let lhs = l_and_exp.get_value(vq);
                let rhs = eq_exp.get_value(vq);
                (lhs != 0 && rhs != 0) as i32
            }
        }
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
    pub fn get_value<T: ValueQuery>(&self, vq: &T) -> i32 {
        match self {
            EqExp::RelExp(rel_exp) => rel_exp.get_value(vq),
            EqExp::EqExpOpRelExp(eq_exp, op, rel_exp) => {
                let lhs = eq_exp.get_value(vq);
                let rhs = rel_exp.get_value(vq);
                match op {
                    EqExpOp::Eq => (lhs == rhs) as i32,
                    EqExpOp::Ne => (lhs != rhs) as i32,
                }
            }
        }
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
    pub fn get_value<T: ValueQuery>(&self, vq: &T) -> i32 {
        match self {
            RelExp::AddExp(add_exp) => add_exp.get_value(vq),
            RelExp::RelExpOpAddExp(rel_exp, op, add_exp) => {
                let lhs = rel_exp.get_value(vq);
                let rhs = add_exp.get_value(vq);
                match op {
                    RelExpOp::Gt => (lhs > rhs) as i32,
                    RelExpOp::Lt => (lhs < rhs) as i32,
                    RelExpOp::Ge => (lhs >= rhs) as i32,
                    RelExpOp::Le => (lhs <= rhs) as i32,
                }
            }
        }
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
    pub fn get_value<T: ValueQuery>(&self, vq: &T) -> i32 {
        match self {
            AddExp::MulExp(mul_exp) => mul_exp.get_value(vq),
            AddExp::AddExpOpMulExp(add_exp, op, mul_exp) => {
                let lhs = add_exp.get_value(vq);
                let rhs = mul_exp.get_value(vq);
                match op {
                    AddOp::Plus => lhs + rhs,
                    AddOp::Minus => lhs - rhs,
                }
            }
        }
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
    pub fn get_value<T: ValueQuery>(&self, vq: &T) -> i32 {
        match self {
            MulExp::UnaryExp(unary_exp) => unary_exp.get_value(vq),
            MulExp::MulExpOpUnaryExp(mul_exp, op, unary_exp) => {
                let lhs = mul_exp.get_value(vq);
                let rhs = unary_exp.get_value(vq);
                match op {
                    MulOp::Mul => lhs * rhs,
                    MulOp::Div => lhs / rhs,
                    MulOp::Mod => lhs % rhs,
                }
            }
        }
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
    pub fn get_value<T: ValueQuery>(&self, vq: &T) -> i32 {
        match self {
            UnaryExp::PrimaryExp(pe) => pe.get_value(vq),
            UnaryExp::UnaryOpAndExp(op, exp) => {
                let rhs = exp.get_value(vq);
                match op {
                    UnaryOp::Plus => rhs,
                    UnaryOp::Minus => -rhs,
                    UnaryOp::Not => {
                        if rhs == 0 {
                            1
                        } else {
                            0
                        }
                    }
                }
            }
        }
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
    pub fn get_value<T: ValueQuery>(&self, vq: &T) -> i32 {
        match self {
            PrimaryExp::Exp(exp) => exp.get_value(vq),
            PrimaryExp::Number(v) => *v,
            PrimaryExp::LVal(name) => {
                let name = name.ident.clone();
                vq.get_value(&name)
                    .expect("should has value when calc exp value")
            }
        }
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
