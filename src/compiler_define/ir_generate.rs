use std::{cell::RefCell, rc::Rc};

use koopa::ir::{FunctionData, Type};

use crate::{ast::{self, *}, ir_enhance::ir_builder::{BlockBuilder, FunctionBuilder, ProgramBuilder}};

struct IRGenerator {

    program_builder: ProgramBuilder,

}

impl<'a> IRGenerator {
    
    fn new() -> Self {
        Self {
            program_builder: ProgramBuilder::new(),
        }
    }

    fn new_function(&mut self, func_data: FunctionData) -> FunctionBuilder {
        let func = self.program_builder.new_function(func_data);
        func
    }
}

#[test]
fn test_func() {
    let mut a = "a".to_string();

    let b = &mut || {
        a = "b".to_string();
    };

    b();
    println!("{}", a);
}

pub fn ir_generate(ast_node: &ast::CompUnit) -> koopa::ir::Program {

    let mut generator = IRGenerator::new();

    let sink = &mut |s: &ast::TraversalStep| {
        if let ast::TraversalStep::Enter(enter) = s {
            match enter {
                AstNode::FuncDef(func_def) => {
                    let func_data = FunctionData::with_param_names("@".to_owned() +
                         &func_def.ident, vec![], Type::get_i32());

                    let func_builder = generator.new_function(func_data);
                    funcs.push(func_builder);
                }
                _ => {}
            }
        }
        if let TraversalStep::Leave(leave) = s {
            match leave {
                AstNode::CompUnit(_) => {},
                AstNode::FuncDef(_) => {},  
                AstNode::Block(_) => todo!(),
                AstNode::BlockItem(_) => todo!(),
                AstNode::Stmt(_) => todo!(),
                AstNode::Decl(_) => todo!(),
                AstNode::ConstDecl(_) => todo!(),
                AstNode::BType(_) => todo!(),
                AstNode::ConstDef(_) => todo!(),
                AstNode::ConstInitVal(_) => todo!(),
                AstNode::ConstExp(_) => todo!(),
                AstNode::Exp(_) => todo!(),
                AstNode::LOrExp(_) => todo!(),
                AstNode::LAndExp(_) => todo!(),
                AstNode::EqExp(_) => todo!(),
                AstNode::RelExp(_) => todo!(),
                AstNode::AddExp(_) => todo!(),
                AstNode::MulExp(_) => todo!(),
                AstNode::UnaryExp(_) => todo!(),
                AstNode::PrimaryExp(_) => todo!(),
                AstNode::LVal(_) => todo!(),
                AstNode::Number(_) => todo!(),
            }
        }

    };

    ast_node.traversal(sink);

    // program_builder.build()

    unimplemented!()
}