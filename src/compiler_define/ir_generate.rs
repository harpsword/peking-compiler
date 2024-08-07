use core::panic;

use decl::{ConstDecl, ConstExp, ConstInitVal};
use expr::{AddExp, EqExp, LAndExp, LOrExp, MulExp, RelExp, UnaryExp};
use koopa::ir::{
    builder::{
        BasicBlockBuilder, EntityInfoQuerier, GlobalBuilder, GlobalInstBuilder, LocalBuilder,
        LocalInstBuilder, ValueBuilder,
    },
    BasicBlock, BinaryOp, Function, FunctionData, Program, Type, Value,
};
use log::info;

use crate::{
    ast::{self, *},
    ir_enhance::tools,
};

use super::{
    semantic_analysis::{self, const_calculate},
    symbol_table::{self, Symbol, SymbolTable},
};

struct IRFunc {
    function: Function,
    has_return: bool,
    parameters: Vec<(String, Value)>,
}

struct IRGenerator {
    program: Program,
    // only in, not out
    // current one is last one
    functions: Vec<IRFunc>,
    // first in, last out
    // the last one is current block
    blocks: Vec<BasicBlock>,
    return_values: Vec<Value>,
    ast_node_kind_stack: Vec<AstNodeKind>,

    // when get a new symbol table, append it
    // when exit a symbol table, pop it
    // when search a symbol, start from the tail
    symbol_tables: Vec<SymbolTable>,
    // contains global symbol, like func name
    global_symbol_table: SymbolTable,

    // for if elsed
    // contains: (then_block, else_block, end_block)
    if_else_then_count: usize,
    if_else_then_stack: Vec<(BasicBlock, BasicBlock, BasicBlock)>,

    while_count: usize,
    // contains: (while_cond_block, while_body_block, end_block)
    while_stack: Vec<(BasicBlock, BasicBlock, BasicBlock)>,
}

#[derive(PartialEq)]
enum AddFuncType {
    Define,
    Decl,
}

impl IRGenerator {
    fn new() -> Self {
        Self {
            program: Program::new(),
            functions: Vec::new(),
            blocks: Vec::new(),
            return_values: Vec::new(),
            ast_node_kind_stack: Vec::new(),

            symbol_tables: vec![SymbolTable::new()],
            global_symbol_table: SymbolTable::new(),

            if_else_then_count: 0,
            if_else_then_stack: Vec::new(),

            while_count: 0,
            while_stack: Vec::new(),
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

    fn ast_kind_stack_first_n_is(&self, kinds: Vec<AstNodeKind>) -> bool {
        for (k1, k2) in self.ast_node_kind_stack.iter().zip(kinds.iter()) {
            if k1 != k2 {
                return false;
            }
        }
        true
    }

    fn ast_kind_stack_has(&self, kind: AstNodeKind) -> bool {
        self.ast_node_kind_stack.contains(&kind)
    }

    fn ast_kind_stack_check_last(&self, kind: AstNodeKind) -> bool {
        self.ast_node_kind_stack
            .last()
            .map_or(false, |k| *k == kind)
    }

    /// last 1 means last one
    fn ast_kind_stack_check_last_n(&self, n: usize, kind: AstNodeKind) -> bool {
        let index = self.ast_node_kind_stack.len() - n;
        self.ast_node_kind_stack
            .get(index)
            .map_or(false, |k| *k == kind)
    }

    fn add_library_function(&mut self) {
        let get_int = FunctionData::new_decl("@getint".to_owned(), Vec::new(), Type::get_i32());
        let get_ch = FunctionData::new_decl("@getch".to_owned(), Vec::new(), Type::get_i32());
        let get_array = FunctionData::new_decl(
            "@getarray".to_owned(),
            vec![Type::get_pointer(Type::get_i32())],
            Type::get_i32(),
        );

        let put_int = FunctionData::new_decl(
            "@putint".to_owned(),
            vec![Type::get_i32()],
            Type::get_unit(),
        );
        let put_ch =
            FunctionData::new_decl("@putch".to_owned(), vec![Type::get_i32()], Type::get_unit());
        let put_array = FunctionData::new_decl(
            "@putarray".to_owned(),
            vec![Type::get_i32(), Type::get_pointer(Type::get_i32())],
            Type::get_unit(),
        );

        let start_time =
            FunctionData::new_decl("@start_time".to_owned(), Vec::new(), Type::get_unit());
        let stop_time =
            FunctionData::new_decl("@stop_time".to_owned(), Vec::new(), Type::get_unit());

        let func_datas = vec![
            get_int, get_ch, get_array, put_int, put_ch, put_array, start_time, stop_time,
        ];
        for func_data in func_datas {
            self.new_function(func_data, vec![], AddFuncType::Decl);
        }
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
        self.global_symbol_table.get_symbol(name)
    }

    /// not update global symbol table
    fn update_symbol(&mut self, name: String, symbol: Symbol) -> Option<Symbol> {
        for symbol_table in self.symbol_tables.iter_mut().rev() {
            let has_symbol = symbol_table.get_symbol(&name).is_some();
            if has_symbol {
                let old_symbol = symbol_table.insert_symbol(name, symbol).unwrap();
                return Some(old_symbol);
            }
        }
        None
    }

    fn get_const(&self, name: &str) -> Option<i32> {
        for symbol_table in self.symbol_tables.iter().rev() {
            if let Some(symbol) = symbol_table.get_const(name) {
                return Some(symbol);
            }
        }
        self.global_symbol_table.get_const(name)
    }

    fn get_var(&mut self, name: &str) -> Option<Value> {
        for symbol_table in self.symbol_tables.iter().rev() {
            if let Some(symbol) = symbol_table.get_var(name) {
                return Some(symbol);
            }
        }
        self.global_symbol_table.get_var(name)
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

    fn clean_return_value(&mut self) {
        self.return_values.clear();
    }
}

impl IRGenerator {
    fn push_if_else(&mut self) {
        let if_else_count = &format!("{}", self.if_else_then_count);
        let then_block = self.new_block(Some("@then".to_string() + if_else_count));
        let else_block = self.new_block(Some("@else".to_string() + if_else_count));
        let end_block = self.new_block(Some("@end".to_string() + if_else_count));
        self.if_else_then_stack
            .push((then_block, else_block, end_block));
        self.if_else_then_count += 1;
    }

    fn pop_if_else(&mut self) {
        self.if_else_then_stack.pop();
    }

    // return (then, else, end)
    fn current_if_else_then(&mut self) -> (BasicBlock, BasicBlock, BasicBlock) {
        self.if_else_then_stack
            .last()
            .map(|(then_block, else_block, end_block)| {
                (then_block.clone(), else_block.clone(), end_block.clone())
            })
            .expect("should have if-else then")
    }
}

impl IRGenerator {
    fn push_while(&mut self) {
        let while_count = &format!("{}", self.while_count);
        let while_entry = self.new_block(Some("@while_entry".to_string() + while_count));
        let while_body = self.new_block(Some("@while_body".to_string() + while_count));
        let while_end = self.new_block(Some("@while_end".to_string() + while_count));
        self.while_stack.push((while_entry, while_body, while_end));
        self.while_count += 1;
    }

    fn pop_while(&mut self) {
        self.while_stack.pop();
    }

    fn current_while(&mut self) -> (BasicBlock, BasicBlock, BasicBlock) {
        self.while_stack
            .last()
            .map(|(while_entry, while_body, while_end)| {
                (while_entry.clone(), while_body.clone(), while_end.clone())
            })
            .expect("should have while")
    }
}

/// for function
impl IRGenerator {
    fn current_func_data(&mut self) -> &mut FunctionData {
        let function = self.functions.last().expect("function not found");

        self.program.func_mut(function.function)
    }

    fn current_ir_func(&mut self) -> &mut IRFunc {
        self.functions.last_mut().expect("function not found")
    }

    fn new_function(
        &mut self,
        func_data: FunctionData,
        params: Vec<(String, Value)>,
        add_func_type: AddFuncType,
    ) {
        let func_name = func_data.name().to_string();
        let func = self.program.new_func(func_data);

        self.global_symbol_table
            .insert_symbol(func_name, Symbol::Func(func));
        let ir_func = IRFunc {
            function: func,
            has_return: false,
            parameters: params,
        };

        if add_func_type == AddFuncType::Define {
            self.functions.push(ir_func);
        }
    }
}

/// Adapt to the koopa IR
impl IRGenerator {
    fn new_block(&mut self, name: Option<String>) -> BasicBlock {
        let func_data = self.current_func_data();
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
        self.current_func_data()
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
        self.current_func_data()
            .layout_mut()
            .bb_mut(bb)
            .insts_mut()
            .extend(iter);
    }

    fn global_new_value(&mut self) -> GlobalBuilder {
        self.program.new_value()
    }

    fn new_value(&mut self) -> LocalBuilder {
        self.current_func_data().dfg_mut().new_value()
    }
}

fn common_expression(generator: &mut IRGenerator, op: impl Into<BinaryOp>) {
    let rhs = generator.pop_return_value();
    let lhs = generator.pop_return_value();
    let or = generator.new_value().binary(op.into(), lhs, rhs);
    generator.extend([or]);

    generator.append_return_value(or);
}

// execute at leave
fn exp_ir_generate(generator: &mut IRGenerator, ast: &AstNode) {
    if generator.ast_kind_stack_first_n_is(vec![AstNodeKind::CompUnit, AstNodeKind::VarDecl]) {
        // for top level var define, need to skip it
        return;
    }
    if generator.ast_kind_stack_has(AstNodeKind::ConstDecl) {
        // expression in const decl shouldn't generate ir
        return;
    }
    // need to ignore top level expression
    if !ast.get_kind().is_expression() {
        return;
    }
    match ast {
        AstNode::LOrExp(exp) => {
            // delay deal with LOrExp->LOrExpOpLAndExp(LOrExp, LAndExp)'s left one
            if generator.ast_kind_stack_check_last(AstNodeKind::LOrExp) {
                let (then_block, else_block, end_block) = generator.current_if_else_then();
                let lhs = generator.pop_return_value();
                let br = generator.new_value().branch(lhs, then_block, else_block);
                generator.extend([br]);

                // deal with then block
                let jump = generator.new_value().jump(end_block);
                generator.extend_to_specified_block(then_block, [jump]);

                generator.pop_block();
                generator.push_block(end_block);
                generator.push_block(else_block);
            }
            match exp {
                LOrExp::LOrExpOpLAndExp(_, _) => {
                    let rhs = generator.pop_return_value();
                    let alloc = generator.pop_return_value();
                    let (_, _, end_block) = generator.current_if_else_then();

                    // deal with else block
                    let zero = generator.new_value().integer(0);
                    let not_eq_0 = generator.new_value().binary(BinaryOp::NotEq, rhs, zero);
                    let assign2 = generator.new_value().store(not_eq_0, alloc);
                    let jump = generator.new_value().jump(end_block);
                    generator.extend([not_eq_0, assign2, jump]);

                    generator.pop_if_else();
                    // pop else_block
                    generator.pop_block();
                    let load = generator.new_value().load(alloc);
                    generator.extend([load]);

                    generator.append_return_value(load);
                }
                LOrExp::LAndExp(_) => {}
            }
        }

        AstNode::LAndExp(exp) => {
            // delay deal with LAndExp->LAndExpOpEqExp(LAndExp, EqExp)
            if generator.ast_kind_stack_check_last(AstNodeKind::LAndExp) {
                let (then_block, else_block, end_block) = generator.current_if_else_then();
                let lhs = generator.pop_return_value();
                let br = generator.new_value().branch(lhs, then_block, else_block);
                generator.extend([br]);

                generator.pop_block();
                generator.push_block(end_block);
                generator.push_block(then_block);
            }

            match exp {
                LAndExp::EqExp(_) => {}
                LAndExp::LAndExpOpEqExp(_, _) => {
                    let rhs = generator.pop_return_value();
                    let alloc = generator.pop_return_value();
                    let (_, else_block, end_block) = generator.current_if_else_then();

                    // deal with then block
                    let zero = generator.new_value().integer(0);
                    let not_eq_0 = generator.new_value().binary(BinaryOp::NotEq, rhs, zero);
                    let assign2 = generator.new_value().store(not_eq_0, alloc);
                    let jump = generator.new_value().jump(end_block);
                    generator.extend([not_eq_0, assign2, jump]);

                    // pop then block
                    generator.pop_block(); 
                    // deal with else block
                    let jump = generator.new_value().jump(end_block);
                    generator.extend_to_specified_block(else_block, [jump]);

                    generator.pop_if_else();
                    let load = generator.new_value().load(alloc);
                    generator.extend([load]);

                    generator.append_return_value(load);
                }
            }
        }
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
            UnaryExp::FuncCall(func_call) => {
                let want_len = func_call.args.len();
                assert!(generator.return_values.len() >= want_len);
                let want_index = generator.return_values.len() - want_len;

                let args: Vec<_> = generator.return_values.split_off(want_index);
                let func_name = tools::turn_into_ir_name(&func_call.ident);
                let function = match generator
                    .global_symbol_table
                    .get_symbol(&func_name)
                    .expect("function not found")
                    .clone()
                {
                    Symbol::Func(function) => function,
                    _ => unreachable!("function not found"),
                };
                let call = generator.new_value().call(function, args);

                generator.extend([call]);

                generator.append_return_value(call);
            }
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
                    symbol_table::Symbol::Const(value) => generator.new_value().integer(value),
                    symbol_table::Symbol::Var(variable) => {
                        let load = generator.new_value().load(variable);
                        generator.extend([load]);
                        load
                    }
                    symbol_table::Symbol::Func(_) => {
                        unreachable!("cannot use func in expression except function call")
                    }
                    Symbol::FuncParam(func_param) => {
                        // for func param, need to alloc a variable
                        // then load data from func_param to this variable
                        let alloc = generator.new_value().alloc(Type::get_i32());
                        let store = generator.new_value().store(func_param, alloc);
                        // update the symbol table
                        generator.update_symbol(name.ident.clone(), Symbol::Var(alloc));

                        let load = generator.new_value().load(alloc);
                        generator.extend([alloc, store, load]);
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
    generator.add_library_function();

    let sink = &mut |s: &ast::TraversalStep| {
        if let ast::TraversalStep::Enter(enter) = s {
            generator.append_ast_kind(enter.get_kind());
            match enter {
                AstNode::FuncDef(func_def) => {
                    let params: Vec<(Option<String>, Type)> = func_def
                        .func_f_params
                        .iter()
                        .map(|x| x.clone().into())
                        .collect();

                    let func_data = FunctionData::with_param_names(
                        "@".to_owned() + &func_def.ident,
                        params,
                        func_def.func_type.clone().into(),
                    );
                    let mut parameters = Vec::new();
                    for (param, value) in func_def.func_f_params.iter().zip(func_data.params()) {
                        parameters.push((param.ident.to_owned(), value.clone()));
                    }
                    generator.new_function(func_data, parameters, AddFuncType::Define);

                    generator.new_block_and_append(Some("@entry".to_owned()));
                }
                AstNode::Block(_) => {
                    generator.new_current_symbol_table();
                    if generator.ast_kind_stack_check_last_n(2, AstNodeKind::FuncDef) {
                        // for func def, need to add parameter to symbol table
                        let mut param_vector = Vec::new();
                        for (param, value) in generator.current_ir_func().parameters.iter() {
                            param_vector.push((param.clone(), value.clone()));
                        }

                        while let Some((param, value)) = param_vector.pop() {
                            generator
                                .current_symbol_table()
                                .insert_symbol(param, Symbol::FuncParam(value));
                        }
                    }
                }
                AstNode::ConstDecl(const_decl) => {
                    // last 1: ConstDecl
                    // last 2: Block or CompUnit
                    if generator.ast_kind_stack_check_last_n(2, AstNodeKind::Block) {
                        semantic_analysis::const_calculate(
                            *const_decl,
                            generator.current_symbol_table(),
                        );
                    } else {
                        semantic_analysis::const_calculate(
                            *const_decl,
                            &mut generator.global_symbol_table,
                        );
                    }
                }
                AstNode::Stmt(Stmt::IfElseStmt(_, _, _)) => {
                    generator.push_if_else();
                }
                AstNode::Stmt(Stmt::WhileStmt(_, _)) => {
                    generator.push_while();
                    let (entry_block, _, _) = generator.current_while();
                    let jump = generator.new_value().jump(entry_block);
                    generator.extend([jump]);

                    generator.pop_block();
                    generator.push_block(entry_block);
                }
                AstNode::ThenStmt(_) => {
                    let (then_block, _, _) = generator.current_if_else_then();
                    generator.push_block(then_block);
                }
                AstNode::ElseStmt(_) => {
                    let (_, else_block, _) = generator.current_if_else_then();
                    generator.push_block(else_block);
                }
                AstNode::LOrExp(exp) => match exp {
                    LOrExp::LAndExp(_) => {}
                    LOrExp::LOrExpOpLAndExp(_, _) => {
                        generator.push_if_else();
                        let alloc = generator.new_value().alloc(Type::get_i32());
                        let init = generator.new_value().integer(1);
                        let assign = generator.new_value().store(init, alloc);
                        generator.extend([alloc, assign]);

                        generator.append_return_value(alloc);
                    }
                },
                AstNode::LAndExp(exp) => match exp {
                    LAndExp::EqExp(_) => {}
                    LAndExp::LAndExpOpEqExp(_, _) => {
                        generator.push_if_else();
                        let alloc = generator.new_value().alloc(Type::get_i32());
                        let init = generator.new_value().integer(0);
                        let assign = generator.new_value().store(init, alloc);
                        generator.extend([alloc, assign]);

                        generator.append_return_value(alloc);
                    }
                },
                // TODO change exp to evaluate at this time
                // and in this time cal exp.traversal function to
                // calculate it like const calculation
                _ => {}
            }
        }

        if let TraversalStep::Leave(leave) = s {
            generator.pop_ast_kind();
            match leave {
                AstNode::FuncDef(_) => {
                    if !generator.current_ir_func().has_return {
                        let ret = generator.new_value().ret(None);
                        generator.extend([ret]);
                    }
                }
                AstNode::BlockItem(_) => {
                    // TODO, need to make sure
                    // need to clean return value
                    generator.clean_return_value();
                }
                AstNode::Stmt(stmt) => match stmt {
                    Stmt::ReturnExp(_) => {
                        let return_value = generator.pop_return_value_option();
                        let ret = generator.new_value().ret(return_value);
                        generator.extend([ret]);
                        generator.current_ir_func().has_return = true;
                    }
                    Stmt::AssignStmt(l_val, _) => {
                        let rhs = generator.pop_return_value();

                        let lhs_name = &l_val.ident;
                        let lhs_symbol = generator
                            .get_symbol(lhs_name)
                            .expect(format!("should define {}", lhs_name).as_str())
                            .clone();
                        let dest = match lhs_symbol {
                            Symbol::Const(_) => {
                                panic!("const cannot be modified");
                            }
                            Symbol::Var(var) => var.clone(),
                            Symbol::Func(_) => unreachable!("function cannot be modified"),
                            Symbol::FuncParam(func_param) => {
                                // for func param, need to alloc a variable
                                // then load data from func_param to this variable
                                let alloc = generator.new_value().alloc(Type::get_i32());
                                let store = generator.new_value().store(func_param, alloc);
                                generator.extend([alloc, store]);
                                generator.update_symbol(lhs_name.clone(), Symbol::Var(alloc));
                                alloc
                            }
                        };

                        let store = generator.new_value().store(rhs, dest);
                        generator.extend([store]);
                    }
                    Stmt::ExpStmt(_) => {}
                    Stmt::BlockStmt(_) => {}
                    Stmt::ContinueStmt => {
                        if !generator.ast_kind_stack_has(AstNodeKind::WhileBody) {
                            panic!("continue should in while body");
                        }
                        let (entry_block, _, _) = generator.current_while();
                        let jump = generator.new_value().jump(entry_block);
                        generator.extend([jump]);
                    }
                    Stmt::BreakStmt => {
                        if !generator.ast_kind_stack_has(AstNodeKind::WhileBody) {
                            panic!("break should in while body");
                        }
                        let (_, _, end_block) = generator.current_while();
                        let jump = generator.new_value().jump(end_block);
                        generator.extend([jump]);
                    }
                    Stmt::IfElseStmt(_, _, then) => {
                        if then.is_none() {
                            let (_, then_block, end_block) = generator.current_if_else_then();
                            let jump = generator.new_value().jump(end_block);
                            generator.extend_to_specified_block(then_block, [jump]);
                        }
                        generator.pop_if_else();
                    }
                    Stmt::WhileStmt(_, _) => {
                        generator.pop_while();
                    }
                },

                AstNode::IfCond(_) => {
                    let origin_block = generator.pop_block().expect("should have origin block");
                    let cond_exp = generator.pop_return_value();
                    let t = generator.new_value().value_type(cond_exp);
                    info!("type: {:?}", t);

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

                AstNode::WhileCond(_) => {
                    let cond_exp = generator.pop_return_value();
                    let (_, body_block, end_block) = generator.current_while();
                    let br = generator
                        .new_value()
                        .branch(cond_exp, body_block, end_block);
                    generator.extend([br]);

                    _ = generator.pop_block();
                    generator.push_block(body_block);
                }
                AstNode::WhileBody(_) => {
                    let (while_entry, _, end_block) = generator.current_while();
                    let jump = generator.new_value().jump(while_entry);
                    generator.extend([jump]);

                    _ = generator.pop_block();
                    generator.push_block(end_block);
                }

                AstNode::VarDef(var_def) => {
                    let in_global = generator.ast_kind_stack_check_last_n(2, AstNodeKind::CompUnit);

                    if in_global {
                        let (name, init_value) = match var_def {
                            decl::VarDef::IdentDefine(ident) => (ident, 0),
                            decl::VarDef::IdentInitVal(ident, init_val) => {
                                let const_exp = ConstExp {
                                    exp: init_val.exp.clone(),
                                };
                                let mock_const = ConstDecl {
                                    b_type: decl::BType::Int,
                                    const_defs: vec![decl::ConstDef {
                                        ident: ident.clone(),
                                        const_init_val: ConstInitVal { const_exp },
                                    }],
                                };
                                let mut local_symbol_table = SymbolTable::new();
                                const_calculate(&mock_const, &mut local_symbol_table);
                                let init_value = local_symbol_table
                                    .get_const(&ident)
                                    .expect("global var should have init value");
                                (ident, init_value)
                            }
                        };
                        let init_value = generator.global_new_value().integer(init_value);
                        let alloc = generator.global_new_value().global_alloc(init_value);
                        generator
                            .global_symbol_table
                            .insert_var(name.clone(), alloc);
                    } else {
                        match var_def {
                            decl::VarDef::IdentDefine(ident) => {
                                let alloc = generator.new_value().alloc(Type::get_i32());
                                generator.extend([alloc]);
                                generator
                                    .current_symbol_table()
                                    .insert_var(ident.clone(), alloc);
                            }
                            decl::VarDef::IdentInitVal(ident, _) => {
                                let rhs = generator.pop_return_value();
                                let alloc = generator.new_value().alloc(Type::get_i32());
                                let store = generator.new_value().store(rhs, alloc);
                                generator.extend([alloc, store]);
                                generator
                                    .current_symbol_table()
                                    .insert_var(ident.clone(), alloc);
                            }
                        }
                    }
                }
                AstNode::Block(_) => {
                    generator.pop_current_symbol_table();
                }

                _ => {}
            }

            exp_ir_generate(&mut generator, leave);
        }
    };

    ast_node.traversal(sink);

    generator.program
}
