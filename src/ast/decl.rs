use env_logger::init;

use super::{AstNode, Exp, TraversalStep};

#[derive(Debug)]
pub struct ConstDecl {
    pub b_type: BType,
    pub const_defs: Vec<ConstDef>,
}

impl ConstDecl {
    pub fn traversal(&self, sink: &mut dyn FnMut(&TraversalStep)) {
        sink(&TraversalStep::Enter(AstNode::ConstDecl(self)));
        for const_def in self.const_defs.iter() {
            const_def.traversal(sink);
        }

        sink(&TraversalStep::Leave(AstNode::ConstDecl(self)));
    }
}

#[derive(Debug, Clone)]
pub enum BType {
    Int,
}

#[derive(Debug)]
pub struct ConstDef {
    pub ident: String,
    pub const_init_val: ConstInitVal,
}

impl ConstDef {
    pub fn traversal(&self, sink: &mut dyn FnMut(&TraversalStep)) {
        sink(&TraversalStep::Enter(AstNode::ConstDef(self)));
        self.const_init_val.traversal(sink);
        sink(&TraversalStep::Leave(AstNode::ConstDef(self)));
    }
}

#[derive(Debug)]
pub struct ConstInitVal {
    pub const_exp: ConstExp,
}

impl ConstInitVal {
    pub fn traversal(&self, sink: &mut dyn FnMut(&TraversalStep)) {
        sink(&TraversalStep::Enter(AstNode::ConstInitVal(self)));
        self.const_exp.traversal(sink);
        sink(&TraversalStep::Leave(AstNode::ConstInitVal(self)));
    }
}

#[derive(Debug)]
pub struct ConstExp {
    pub exp: Box<Exp>,
}

impl ConstExp {
    pub fn traversal(&self, sink: &mut dyn FnMut(&TraversalStep)) {
        sink(&TraversalStep::Enter(AstNode::ConstExp(self)));
        self.exp.traversal(sink);
        sink(&TraversalStep::Leave(AstNode::ConstExp(self)));
    }
}


#[derive(Debug)]
pub struct VarDecl {
    pub b_type: BType,
    pub var_defs: Vec<VarDef>,
}

impl VarDecl {
    pub fn traversal(&self, sink: &mut dyn FnMut(&TraversalStep)) {
        sink(&TraversalStep::Enter(AstNode::VarDecl(self)));
        for var_def in self.var_defs.iter() {
            var_def.traversal(sink);
        }
    }
}

#[derive(Debug)]
pub enum VarDef {
    IdentDefine(String),
    IdentInitVal(String, InitVal),
}

impl VarDef {
    pub fn traversal(&self, sink: &mut dyn FnMut(&TraversalStep)) {
        sink(&TraversalStep::Enter(AstNode::VarDef(self)));
        match self {
            VarDef::IdentDefine(_) => {},
            VarDef::IdentInitVal(_, init_val) => {
                init_val.traversal(sink);
            },
        }
        sink(&TraversalStep::Leave(AstNode::VarDef(self)));
    }
}

#[derive(Debug)]
pub struct InitVal {
    pub exp: Box<Exp>,
}

impl InitVal {
    pub fn traversal(&self, sink: &mut dyn FnMut(&TraversalStep)) {
        sink(&TraversalStep::Enter(AstNode::InitVal(self)));
        self.exp.traversal(sink);
        sink(&TraversalStep::Leave(AstNode::InitVal(self)));
    }
}