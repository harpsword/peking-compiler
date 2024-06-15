use core::panic;
use std::mem;

use expr::{AddExp, EqExp, LAndExp, LOrExp, MulExp, PrimaryExp, RelExp, UnaryExp};
use koopa::ir::{
    builder::{BasicBlockBuilder, LocalBuilder, LocalInstBuilder, ValueBuilder},
    BasicBlock, BinaryOp, Function, FunctionData, Program, Type, Value,
};

use crate::ast::{self, *};

use super::{
    semantic_analysis,
    symbol_table::{self, SymbolTable},
};

struct IRGenerator {
    program: Program,
    function: Option<Function>,
    // first in, last out
    // the last one is current block
    blocks: Vec<BasicBlock>,
    return_values: Vec<Value>,
    ast_node_kind_stack: Vec<AstNodeKind>,

    // when get a new symbol table, append it
    // when exit a symbol table, pop it
    // when search a symbol, start from the tail
    symbol_tables: Vec<SymbolTable>,

    // for if elsed
    // contains: (then_block, else_block)
    if_else_then_stack: Vec<(BasicBlock, BasicBlock, BasicBlock)>,
}

impl IRGenerator {
    fn new() -> Self {
        Self {
            program: Program::new(),
            function: Option::None,
            blocks: Vec::new(),
            return_values: Vec::new(),
            ast_node_kind_stack: Vec::new(),

            symbol_tables: vec![SymbolTable::new()],
            if_else_then_stack: Vec::new(),
        }
    }

    fn append_ast_kind(&mut self, kind: AstNodeKind) {
        self.ast_node_kind_stack.push(kind)
    }

    fn pop_ast_kind(&mut self) -> AstNodeKind {
        self.ast_node_kind_stack
            .pop()
            .expect("should have ast node kind")
    }

    fn ast_kind_stack_has(&self, kind: AstNodeKind) -> bool {
        self.ast_node_kind_stack.contains(&kind)
    }

    fn ast_kind_stack_check_last(&self, kind: AstNodeKind) -> bool {
        self.ast_node_kind_stack
            .last()
            .map_or(false, |k| *k == kind)
    }
}

impl IRGenerator {
    fn current_symbol_table(&mut self) -> &mut SymbolTable {
        self.symbol_tables
            .last_mut()
            .expect("should have symbol table")
    }

    // when enter a new block, append current_symbol_table, and create a new one
    fn new_current_symbol_table(&mut self) {
        self.symbol_tables.push(SymbolTable::new());
    }

    // when close a block, pop the symbol table to current_symbol_table
    fn pop_current_symbol_table(&mut self) {
        let _ = self.symbol_tables.pop().expect("should have symbol table");
    }

    fn get_symbol(&self, name: &str) -> Option<&symbol_table::Symbol> {
        for symbol_table in self.symbol_tables.iter().rev() {
            if let Some(symbol) = symbol_table.get_symbol(name) {
                return Some(symbol);
            }
        }
        None
    }

    fn insert_const(&mut self, name: String, value: i32) {
        self.current_symbol_table().insert_const(name, value);
    }

    fn get_const(&self, name: &str) -> Option<i32> {
        for symbol_table in self.symbol_tables.iter().rev() {
            if let Some(symbol) = symbol_table.get_const(name) {
                return Some(symbol);
            }
        }
        None
    }

    fn insert_var(&mut self, name: String, value: Value) {
        self.current_symbol_table().insert_var(name, value);
    }

    fn get_var(&mut self, name: &str) -> Option<Value> {
        for symbol_table in self.symbol_tables.iter().rev() {
            if let Some(symbol) = symbol_table.get_var(name) {
                return Some(symbol);
            }
        }
        None
    }
}

impl IRGenerator {
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

impl IRGenerator {
    fn push_if_else(&mut self) {
        let if_else_count = &format!("{}", self.if_else_then_stack.len());
        let then_block = self.new_block(Some("@then".to_string() + if_else_count));
        let else_block = self.new_block(Some("@else".to_string() + if_else_count));
        let end_block = self.new_block(Some("@end".to_string() + if_else_count));
        self.if_else_then_stack
            .push((then_block, else_block, end_block));
    }

    fn pop_if_else(&mut self) {
        self.if_else_then_stack.pop();
    }

    fn current_if_else_then(&mut self) -> (BasicBlock, BasicBlock, BasicBlock) {
        self.if_else_then_stack
            .last()
            .map(|(then_block, else_block, end_block)| {
                (then_block.clone(), else_block.clone(), end_block.clone())
            })
            .expect("should have if-else then")
    }
}

/// Adapt to the koopa IR
impl IRGenerator {
    fn func_data(&mut self) -> &mut FunctionData {
        self.program
            .func_mut(self.function.expect("function not found"))
    }

    fn new_function(&mut self, func_data: FunctionData) {
        self.function = Some(self.program.new_func(func_data));
    }

    fn new_block(&mut self, name: Option<String>) -> BasicBlock {
        let func_data = self.func_data();
        let bb = func_data.dfg_mut().new_bb().basic_block(name);
        let _ = func_data.layout_mut().bbs_mut().push_key_back(bb);
        bb
    }

    fn new_block_and_append(&mut self, name: Option<String>) {
        let bb = self.new_block(name);

        self.blocks.push(bb);
    }

    fn push_block(&mut self, bb: BasicBlock) {
        self.blocks.push(bb);
    }

    fn pop_block(&mut self) -> Option<BasicBlock> {
        self.blocks.pop()
    }

    fn current_block(&mut self) -> BasicBlock {
        self.blocks.last().expect("block not found").clone()
    }

    fn extend<I: IntoIterator<Item = Value>>(&mut self, iter: I) {
        let bb = self.current_block();
        self.func_data()
            .layout_mut()
            .bb_mut(bb)
            .insts_mut()
            .extend(iter);
    }

    fn extend_to_specified_block<I: IntoIterator<Item = Value>>(
        &mut self,
        bb: BasicBlock,
        iter: I,
    ) {
        self.func_data()
            .layout_mut()
            .bb_mut(bb)
            .insts_mut()
            .extend(iter);
    }

    fn new_value(&mut self) -> LocalBuilder {
        self.func_data().dfg_mut().new_value()
    }
}

fn common_expression(generator: &mut IRGenerator, op: impl Into<BinaryOp>) {
    let rhs = generator.pop_return_value();
    let lhs = generator.pop_return_value();
    let or = generator.new_value().binary(op.into(), lhs, rhs);
    generator.extend([or]);

    generator.append_return_value(or);
}

fn exp_ir_generate(generator: &mut IRGenerator, ast: &AstNode) {
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
        AstNode::PrimaryExp(_) => {}
        AstNode::Number(v) => {
            let value = generator.new_value().integer(**v);
            generator.append_return_value(value);
        }
        AstNode::LVal(name) => {
            if generator.ast_kind_stack_check_last(AstNodeKind::PrimaryExp) {
                // only deal with LVal inside Expression
                let symbol = generator
                    .get_symbol(&name.ident)
                    .expect(format!("should define {}", name.ident).as_str())
                    .clone();
                let value = match symbol {
                    super::symbol_table::Symbol::Const(value) => {
                        generator.new_value().integer(value)
                    }
                    super::symbol_table::Symbol::Var(variable) => {
                        let load = generator.new_value().load(variable);
                        generator.extend([load]);
                        load
                    }
                };
                generator.append_return_value(value);
            }
        }

        _ => {}
    }
}

pub fn ir_generate(ast_node: &ast::CompUnit) -> koopa::ir::Program {
    let mut generator = IRGenerator::new();

    let sink = &mut |s: &ast::TraversalStep| {
        if let ast::TraversalStep::Enter(enter) = s {
            generator.append_ast_kind(enter.get_kind());
            match enter {
                AstNode::FuncDef(func_def) => {
                    let func_data = FunctionData::with_param_names(
                        "@".to_owned() + &func_def.ident,
                        vec![],
                        Type::get_i32(),
                    );

                    generator.new_function(func_data);
                    generator.new_block_and_append(Some("@entry".to_owned()));
                }
                AstNode::Block(_) => {
                    generator.new_current_symbol_table();
                }
                AstNode::ConstDecl(const_decl) => {
                    semantic_analysis::const_calculate(
                        *const_decl,
                        generator.current_symbol_table(),
                    );
                }
                AstNode::Stmt(Stmt::IfElseStmt(_, _, _)) => {
                    generator.push_if_else();
                }
                AstNode::ThenStmt(_) => {
                    let (then_block, _, _) = generator.current_if_else_then();
                    generator.push_block(then_block);
                }
                AstNode::ElseStmt(_) => {
                    let (_, else_block, _) = generator.current_if_else_then();
                    generator.push_block(else_block);
                }
                // TODO change exp to evaluate at this time
                // and in this time cal exp.traversal function to
                // calculate it like const calculation
                _ => {}
            }
        }

        if let TraversalStep::Leave(leave) = s {
            generator.pop_ast_kind();
            match leave {
                AstNode::Stmt(stmt) => match stmt {
                    Stmt::ReturnExp(_) => {
                        let return_value = generator.pop_return_value_option();
                        let ret = generator.new_value().ret(return_value);
                        generator.extend([ret]);
                    }
                    Stmt::AssignStmt(l_val, _) => {
                        let rhs = generator.pop_return_value();

                        let lhs_name = &l_val.ident;
                        let symbol = generator
                            .get_symbol(lhs_name)
                            .expect(format!("should define {}", lhs_name).as_str());
                        let dest = match symbol {
                            super::symbol_table::Symbol::Const(_) => {
                                panic!("const cannot be modified");
                            }
                            super::symbol_table::Symbol::Var(var) => var.clone(),
                        };

                        let store = generator.new_value().store(rhs, dest);
                        generator.extend([store]);
                    }
                    Stmt::ExpStmt(_) => {}
                    Stmt::BlockStmt(_) => {}
                    Stmt::IfElseStmt(_, _, _) => {
                        generator.pop_if_else();
                    }
                },

                AstNode::IfCond(_) => {
                    let origin_block = generator.pop_block().expect("should have origin block");
                    let cond_exp = generator.pop_return_value();
                    let (then_block, else_block, end_block) = generator.current_if_else_then();
                    let br = generator
                        .new_value()
                        .branch(cond_exp, then_block, else_block);
                    generator.extend_to_specified_block(origin_block, [br]);

                    generator.push_block(end_block);
                }
                AstNode::ThenStmt(_) => {
                    let then_block = generator.pop_block().expect("should have then block");
                    let current_end_block = generator.current_block();
                    let jump = generator.new_value().jump(current_end_block);
                    generator.extend_to_specified_block(then_block, [jump]);
                }
                AstNode::ElseStmt(_) => {
                    let else_block = generator.pop_block().expect("should have else block");
                    let current_end_block = generator.current_block();
                    let jump = generator.new_value().jump(current_end_block);
                    generator.extend_to_specified_block(else_block, [jump]);
                }

                AstNode::VarDef(var_def) => match var_def {
                    decl::VarDef::IdentDefine(ident) => {
                        let alloc = generator.new_value().alloc(Type::get_i32());
                        generator.extend([alloc]);
                        generator.insert_var(ident.clone(), alloc);
                    }
                    decl::VarDef::IdentInitVal(ident, _) => {
                        let rhs = generator.pop_return_value();
                        let alloc = generator.new_value().alloc(Type::get_i32());
                        let store = generator.new_value().store(rhs, alloc);
                        generator.extend([alloc, store]);
                        generator.insert_var(ident.clone(), alloc);
                    }
                },
                AstNode::Block(_) => {
                    generator.pop_current_symbol_table();
                }

                _ => {}
            }

            // expression in const decl shouldn't generate ir
            if !generator.ast_kind_stack_has(AstNodeKind::ConstDecl) {
                exp_ir_generate(&mut generator, leave);
            }
        }
    };

    ast_node.traversal(sink);

    generator.program
}
