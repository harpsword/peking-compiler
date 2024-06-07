use expr::AddExp;
use expr::EqExp;
use expr::LAndExp;
use expr::LOrExp;
use expr::MulExp;
use expr::RelExp;
use expr::UnaryExp;
use log::info;

use crate::ast;
use crate::ast::AstNode;
use crate::ast::TraversalStep;
use crate::ast::*;

use super::symbol_table::ConstTable;

struct ConstCalculation {
    values: Vec<i32>,

    const_table: ConstTable,
}

impl ConstCalculation {
    fn new() -> Self {
        Self {
            values: Vec::new(),
            const_table: ConstTable::new(),
        }
    }

    fn append(&mut self, value: i32) {
        self.values.push(value);
    }

    fn const_table(&mut self) -> &mut ConstTable {
        &mut self.const_table
    }

    fn get_index(&self, index: usize) -> i32 {
        assert!(index < self.values.len());
        self.values[index]
    }

    fn set_to_value(&mut self, value: i32) {
        self.values.clear();
        self.values.push(value);
    }
}

pub fn const_calculate(ast_node: &ast::CompUnit) -> ConstTable {
    let mut const_calc = ConstCalculation::new();

    let sink = &mut |s: &TraversalStep| {
        if let TraversalStep::Leave(leave) = s {
            match leave {
                AstNode::CompUnit(_) => {}
                AstNode::FuncDef(_) => {}
                AstNode::Block(_) => {}
                AstNode::BlockItem(_) => {}
                AstNode::Stmt(_) => {}
                AstNode::Decl(_) => {}
                AstNode::ConstDecl(_) => {}
                AstNode::BType(_) => {}
                AstNode::ConstDef(const_def) => {
                    let value = const_calc.get_index(0);
                    const_calc.values.clear();
                    const_calc
                        .const_table
                        .insert_const(const_def.ident.clone(), value);
                }
                AstNode::ConstInitVal(_) => {}
                AstNode::ConstExp(_) => {}
                AstNode::Exp(_) => {}
                AstNode::LOrExp(exp) => match exp {
                    LOrExp::LOrExpOpLAndExp(_, _) => {
                        let lhs = const_calc.get_index(0);
                        let rhs = const_calc.get_index(1);
                        let value = (lhs != 0) || (rhs != 0);
                        const_calc.set_to_value(value as i32);
                    }
                    _ => {}
                },
                AstNode::LAndExp(exp) => match exp {
                    LAndExp::LAndExpOpEqExp(_, _) => {
                        let lhs = const_calc.get_index(0);
                        let rhs = const_calc.get_index(1);
                        let value = (lhs != 0) && (rhs != 0);
                        const_calc.set_to_value(value as i32);
                    }
                    _ => {}
                },
                AstNode::EqExp(exp) => match exp {
                    EqExp::EqExpOpRelExp(_, op, _) => {
                        let lhs = const_calc.get_index(0);
                        let rhs = const_calc.get_index(1);
                        let value = match op {
                            expr::EqExpOp::Eq => lhs == rhs,
                            expr::EqExpOp::Ne => lhs != rhs,
                        };
                        const_calc.set_to_value(value as i32);
                    }
                    _ => {}
                },
                AstNode::RelExp(exp) => match exp {
                    RelExp::RelExpOpAddExp(_, op, _) => {
                        let lhs = const_calc.get_index(0);
                        let rhs = const_calc.get_index(1);
                        let value = match op {
                            expr::RelExpOp::Lt => lhs < rhs,
                            expr::RelExpOp::Gt => lhs > rhs,
                            expr::RelExpOp::Le => lhs <= rhs,
                            expr::RelExpOp::Ge => lhs >= rhs,
                        };
                        const_calc.set_to_value(value as i32);
                    }
                    _ => {}
                },
                AstNode::AddExp(exp) => match exp {
                    AddExp::AddExpOpMulExp(_, op, _) => {
                        let lhs = const_calc.get_index(0);
                        let rhs = const_calc.get_index(1);
                        let value = match op {
                            expr::AddOp::Plus => lhs + rhs,
                            expr::AddOp::Minus => lhs - rhs,
                        };
                        const_calc.set_to_value(value);
                    }
                    _ => {}
                },
                AstNode::MulExp(exp) => match exp {
                    MulExp::MulExpOpUnaryExp(_, op, _) => {
                        let lhs = const_calc.get_index(0);
                        let rhs = const_calc.get_index(1);

                        let value = match op {
                            expr::MulOp::Mul => lhs * rhs,
                            expr::MulOp::Div => lhs / rhs,
                            expr::MulOp::Mod => lhs % rhs,
                        };
                        const_calc.set_to_value(value);
                    }
                    _ => {}
                },
                AstNode::UnaryExp(exp) => match exp {
                    UnaryExp::UnaryOpAndExp(op, _) => {
                        let mut value = const_calc.get_index(0);
                        value = match op {
                            expr::UnaryOp::Plus => value,
                            expr::UnaryOp::Minus => -value,
                            expr::UnaryOp::Not => {
                                if value == 0 {
                                    1
                                } else {
                                    0
                                }
                            }
                        };
                        const_calc.set_to_value(value);
                    }
                    _ => {}
                },
                AstNode::PrimaryExp(_) => {}
                AstNode::LVal(name) => {
                    let value = const_calc.const_table().get_const(&name.ident);
                    assert!(value.is_some());

                    const_calc.append(value.expect("should has value when calc exp value"));
                }
                AstNode::Number(&v) => {
                    const_calc.append(v);
                }
            }
        }
    };

    ast_node.traversal(sink);

    info!("const table: {:#?}", const_calc.const_table);

    const_calc.const_table
}
