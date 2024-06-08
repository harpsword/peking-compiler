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
use crate::ir_enhance::generate_riscv;

use super::symbol_table::SymbolTable;

struct ConstCalculation {
    values: Vec<i32>,

    const_table: SymbolTable,

    ast_node_kind_stack: Vec<AstNodeKind>,
}

impl ConstCalculation {
    fn new() -> Self {
        Self {
            values: Vec::new(),
            const_table: SymbolTable::new(),

            ast_node_kind_stack: Vec::new(),
        }
    }

    fn append(&mut self, value: i32) {
        self.values.push(value);
    }

    fn const_table(&mut self) -> &mut SymbolTable {
        &mut self.const_table
    }

    fn pop_return_value(&mut self) -> i32 {
        self.values.pop().expect("should have value")
    }

    fn push_return_value(&mut self, value: i32) {
        self.values.push(value);
    }

    // TODO opt, duplicate code
    /// ast kind
    fn append_ast_kind(&mut self, kind: AstNodeKind) {
        self.ast_node_kind_stack.push(kind)
    }

    /// ast kind
    fn pop_ast_kind(&mut self) -> AstNodeKind {
        self.ast_node_kind_stack
            .pop()
            .expect("should have ast node kind")
    }

    fn ast_kind_stack_has(&self, kind: AstNodeKind) -> bool {
        self.ast_node_kind_stack.contains(&kind)
    }
}

pub fn const_calculate(ast_node: &ast::CompUnit) -> SymbolTable {
    let mut const_calc = ConstCalculation::new();

    let sink = &mut |s: &TraversalStep| {
        if let TraversalStep::Enter(enter) = s {
            const_calc.append_ast_kind(enter.get_kind());
        }

        if let TraversalStep::Leave(leave) = s {
            const_calc.pop_ast_kind();
            if !const_calc.ast_kind_stack_has(AstNodeKind::ConstDecl) {
                return;
            }
            match leave {
                AstNode::ConstDef(const_def) => {
                    let value = const_calc.pop_return_value();
                    const_calc.values.clear();
                    const_calc
                        .const_table
                        .insert_const(const_def.ident.clone(), value);
                }
                AstNode::LOrExp(exp) => match exp {
                    LOrExp::LOrExpOpLAndExp(_, _) => {
                        let rhs = const_calc.pop_return_value();
                        let lhs = const_calc.pop_return_value();
                        let value = (lhs != 0) || (rhs != 0);
                        const_calc.push_return_value(value as i32);
                    }
                    _ => {}
                },
                AstNode::LAndExp(exp) => match exp {
                    LAndExp::LAndExpOpEqExp(_, _) => {
                        let rhs = const_calc.pop_return_value();
                        let lhs = const_calc.pop_return_value();
                        let value = (lhs != 0) && (rhs != 0);
                        const_calc.push_return_value(value as i32);
                    }
                    _ => {}
                },
                AstNode::EqExp(exp) => match exp {
                    EqExp::EqExpOpRelExp(_, op, _) => {
                        let rhs = const_calc.pop_return_value();
                        let lhs = const_calc.pop_return_value();
                        let value = match op {
                            expr::EqExpOp::Eq => lhs == rhs,
                            expr::EqExpOp::Ne => lhs != rhs,
                        };
                        const_calc.push_return_value(value as i32);
                    }
                    _ => {}
                },
                AstNode::RelExp(exp) => match exp {
                    RelExp::RelExpOpAddExp(_, op, _) => {
                        let rhs = const_calc.pop_return_value();
                        let lhs = const_calc.pop_return_value();
                        let value = match op {
                            expr::RelExpOp::Lt => lhs < rhs,
                            expr::RelExpOp::Gt => lhs > rhs,
                            expr::RelExpOp::Le => lhs <= rhs,
                            expr::RelExpOp::Ge => lhs >= rhs,
                        };
                        const_calc.push_return_value(value as i32);
                    }
                    _ => {}
                },
                AstNode::AddExp(exp) => match exp {
                    AddExp::AddExpOpMulExp(_, op, _) => {
                        let rhs = const_calc.pop_return_value();
                        let lhs = const_calc.pop_return_value();
                        let value = match op {
                            expr::AddOp::Plus => lhs + rhs,
                            expr::AddOp::Minus => lhs - rhs,
                        };
                        const_calc.push_return_value(value);
                    }
                    _ => {}
                },
                AstNode::MulExp(exp) => match exp {
                    MulExp::MulExpOpUnaryExp(_, op, _) => {
                        let rhs = const_calc.pop_return_value();
                        let lhs = const_calc.pop_return_value();

                        let value = match op {
                            expr::MulOp::Mul => lhs * rhs,
                            expr::MulOp::Div => lhs / rhs,
                            expr::MulOp::Mod => lhs % rhs,
                        };
                        const_calc.push_return_value(value);
                    }
                    _ => {}
                },
                AstNode::UnaryExp(exp) => match exp {
                    UnaryExp::UnaryOpAndExp(op, _) => {
                        let mut value = const_calc.pop_return_value();
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
                        const_calc.push_return_value(value);
                    }
                    _ => {}
                },
                AstNode::PrimaryExp(_) => {}
                AstNode::LVal(name) => {
                    let value = const_calc.const_table().get_const(&name.ident);
                    info!("name: {:?}, value: {:?}", name.ident, value);
                    assert!(value.is_some());

                    const_calc.append(value.expect("should has value when calc exp value"));
                }
                AstNode::Number(&v) => {
                    const_calc.append(v);
                }
                _ => {}
            }
        }
    };

    ast_node.traversal(sink);

    info!("const table: {:#?}", const_calc.const_table);

    const_calc.const_table
}
