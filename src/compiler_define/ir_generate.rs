use std::{cell::RefCell, fs::OpenOptions, rc::Rc};

use expr::{AddExp, EqExp, LAndExp, LOrExp, MulExp, RelExp, UnaryExp};
use koopa::{
    front::ast::Ast,
    ir::{
        builder::{BasicBlockBuilder, LocalBuilder, LocalInstBuilder, ValueBuilder},
        values, BasicBlock, BinaryOp, Function, FunctionData, Program, Type, Value,
    },
};

use crate::{
    ast::{self, *},
    ir_enhance::ir_builder::{BlockBuilder, FunctionBuilder, ProgramBuilder},
};

use super::symbol_table::ConstTable;

struct IRGenerator {
    program: Program,
    function: Option<Function>,
    block: Option<BasicBlock>,
    return_values: Vec<Value>,
}

impl IRGenerator {
    fn new() -> Self {
        Self {
            program: Program::new(),
            function: Option::None,
            block: Option::None,
            return_values: Vec::new(),
        }
    }

    fn func_data(&mut self) -> &mut FunctionData {
        self.program
            .func_mut(self.function.expect("function not found"))
    }

    fn new_function(&mut self, func_data: FunctionData) {
        self.function = Some(self.program.new_func(func_data));
    }

    fn new_block(&mut self, name: Option<String>) {
        let func_data = self.func_data();
        let bb = func_data.dfg_mut().new_bb().basic_block(name);
        let _ = func_data.layout_mut().bbs_mut().push_key_back(bb);

        self.block = Some(bb);
    }

    fn extend<I: IntoIterator<Item = Value>>(&mut self, iter: I) {
        let bb = self.block.expect("block not found");
        self.func_data()
            .layout_mut()
            .bb_mut(bb)
            .insts_mut()
            .extend(iter);
    }

    fn new_value(&mut self) -> LocalBuilder {
        self.func_data().dfg_mut().new_value()
    }

    fn pop_return_value_option(&mut self) -> Option<Value> {
        self.return_values.pop()
    }

    fn pop_return_value(&mut self) -> Value {
        self.return_values.pop().expect("should have return value")
    }

    fn append_return_value(&mut self, value: Value) {
        self.return_values.push(value);
    }
}

fn common_expression(generator: &mut IRGenerator, op: impl Into<BinaryOp>) {
    let rhs = generator.pop_return_value();
    let lhs = generator.pop_return_value();
    let or = generator.new_value().binary(op.into(), lhs, rhs);
    generator.extend([or]);

    generator.append_return_value(or);
}

fn exp_ir_generate(generator: &mut IRGenerator, ast: &AstNode, value_table: &ConstTable) {
    if !ast.get_kind().is_expression() {
        return;
    }
    match ast {
        AstNode::LOrExp(exp) => match exp {
            LOrExp::LOrExpOpLAndExp(_, _) => {
                common_expression(generator, BinaryOp::Or);
            }
            _ => {}
        },

        AstNode::LAndExp(exp) => match exp {
            LAndExp::LAndExpOpEqExp(_, _) => {
                common_expression(generator, BinaryOp::And);
            }
            _ => {}
        },
        AstNode::EqExp(exp) => match exp {
            EqExp::EqExpOpRelExp(_, op, _) => {
                common_expression(generator, op.clone());
            }
            _ => {}
        },
        AstNode::RelExp(exp) => match exp {
            RelExp::RelExpOpAddExp(_, op, _) => {
                common_expression(generator, op.clone());
            }
            _ => {}
        },
        AstNode::AddExp(exp) => match exp {
            AddExp::AddExpOpMulExp(_, op, _) => {
                common_expression(generator, op.clone());
            }
            _ => {}
        },
        AstNode::MulExp(exp) => match exp {
            MulExp::MulExpOpUnaryExp(_, op, _) => {
                common_expression(generator, op.clone());
            }
            _ => {}
        },

        AstNode::UnaryExp(exp) => match exp {
            UnaryExp::UnaryOpAndExp(op, _) => match op {
                expr::UnaryOp::Plus => {}
                expr::UnaryOp::Minus => {
                    let op_exp = generator.pop_return_value();
                    let lhs = generator.new_value().integer(0);
                    let minus = generator.new_value().binary(BinaryOp::Sub, lhs, op_exp);
                    generator.extend([minus]);

                    generator.append_return_value(minus);
                }
                expr::UnaryOp::Not => {
                    let op_exp = generator.pop_return_value();
                    let rhs = generator.new_value().integer(0);
                    let not = generator.new_value().binary(BinaryOp::Eq, op_exp, rhs);
                    generator.extend([not]);

                    generator.append_return_value(not);
                }
            },
            _ => {}
        },
        AstNode::Number(v) => {
            let value = generator.new_value().integer(**v);
            generator.append_return_value(value);
        }
        AstNode::LVal(name) => {
            let value = value_table
                .get_const(&name.ident)
                .expect("should define it before");
            let value = generator.new_value().integer(value);
            generator.append_return_value(value);
        }

        _ => {}
    }
}

pub fn ir_generate(ast_node: &ast::CompUnit, value_table: &ConstTable) -> koopa::ir::Program {
    let mut generator = IRGenerator::new();

    let sink = &mut |s: &ast::TraversalStep| {
        if let ast::TraversalStep::Enter(enter) = s {
            match enter {
                AstNode::FuncDef(func_def) => {
                    let func_data = FunctionData::with_param_names(
                        "@".to_owned() + &func_def.ident,
                        vec![],
                        Type::get_i32(),
                    );

                    generator.new_function(func_data);
                }
                AstNode::Block(_) => {
                    generator.new_block(Some("@entry".to_owned()));
                }
                _ => {}
            }
        }

        if let TraversalStep::Leave(leave) = s {
            match leave {
                AstNode::Stmt(_) => {
                    let return_value = generator.pop_return_value_option();
                    let ret = generator.new_value().ret(return_value);
                    generator.extend([ret]);
                }

                // for experission
                AstNode::LOrExp(exp) => match exp {
                    LOrExp::LOrExpOpLAndExp(_, _) => {
                        common_expression(&mut generator, BinaryOp::Or);
                    }
                    _ => {}
                },

                AstNode::LAndExp(exp) => match exp {
                    LAndExp::LAndExpOpEqExp(_, _) => {
                        common_expression(&mut generator, BinaryOp::And);
                    }
                    _ => {}
                },
                AstNode::EqExp(exp) => match exp {
                    EqExp::EqExpOpRelExp(_, op, _) => {
                        common_expression(&mut generator, op.clone());
                    }
                    _ => {}
                },
                AstNode::RelExp(exp) => match exp {
                    RelExp::RelExpOpAddExp(_, op, _) => {
                        common_expression(&mut generator, op.clone());
                    }
                    _ => {}
                },
                AstNode::AddExp(exp) => match exp {
                    AddExp::AddExpOpMulExp(_, op, _) => {
                        common_expression(&mut generator, op.clone());
                    }
                    _ => {}
                },
                AstNode::MulExp(exp) => match exp {
                    MulExp::MulExpOpUnaryExp(_, op, _) => {
                        common_expression(&mut generator, op.clone());
                    }
                    _ => {}
                },

                AstNode::UnaryExp(exp) => match exp {
                    UnaryExp::UnaryOpAndExp(op, _) => match op {
                        expr::UnaryOp::Plus => {}
                        expr::UnaryOp::Minus => {
                            let op_exp = generator.pop_return_value();
                            let lhs = generator.new_value().integer(0);
                            let minus = generator.new_value().binary(BinaryOp::Sub, lhs, op_exp);
                            generator.extend([minus]);

                            generator.append_return_value(minus);
                        }
                        expr::UnaryOp::Not => {
                            let op_exp = generator.pop_return_value();
                            let rhs = generator.new_value().integer(0);
                            let not = generator.new_value().binary(BinaryOp::Eq, op_exp, rhs);
                            generator.extend([not]);

                            generator.append_return_value(not);
                        }
                    },
                    _ => {}
                },
                AstNode::Number(v) => {
                    let value = generator.new_value().integer(**v);
                    generator.append_return_value(value);
                }
                AstNode::LVal(name) => {
                    let value = value_table
                        .get_const(&name.ident)
                        .expect("should define it before");
                    let value = generator.new_value().integer(value);
                    generator.append_return_value(value);
                }

                _ => {}
            }
        }
    };

    ast_node.traversal(sink);

    generator.program
}
