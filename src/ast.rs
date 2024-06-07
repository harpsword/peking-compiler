use crate::ir_enhance::ir_builder::*;
use koopa::ir::{builder_traits::*, *};
use log::info;

use self::const_decl::*;
use self::expr::*;

pub mod const_decl;
pub mod expr;

pub enum AstNode<'a> {
    CompUnit(&'a CompUnit),
    FuncDef(&'a FuncDef),

    Block(&'a Block),
    BlockItem(&'a BlockItem),
    Stmt(&'a Stmt),
    Decl(&'a Decl),

    ConstDecl(&'a ConstDecl),
    BType(&'a BType),
    ConstDef(&'a ConstDef),
    ConstInitVal(&'a ConstInitVal),
    ConstExp(&'a ConstExp),

    Exp(&'a Exp),
    LOrExp(&'a LOrExp),
    LAndExp(&'a LAndExp),
    EqExp(&'a EqExp),
    RelExp(&'a RelExp),
    AddExp(&'a AddExp),
    MulExp(&'a MulExp),
    UnaryExp(&'a UnaryExp),
    PrimaryExp(&'a PrimaryExp),
    LVal(&'a LVal),
    Number(&'a i32),
}

impl AstNode<'_> {
    pub fn get_kind(&self) -> AstNodeKind {
        match self {
            AstNode::CompUnit(_) => AstNodeKind::CompUnit,
            AstNode::FuncDef(_) => AstNodeKind::FuncDef,

            AstNode::Block(_) => AstNodeKind::Block,
            AstNode::BlockItem(_) => AstNodeKind::BlockItem,
            AstNode::Stmt(_) => AstNodeKind::Stmt,
            AstNode::Decl(_) => AstNodeKind::Decl,

            AstNode::ConstDecl(_) => AstNodeKind::ConstDecl,
            AstNode::BType(_) => AstNodeKind::BType,
            AstNode::ConstDef(_) => AstNodeKind::ConstDef,
            AstNode::ConstInitVal(_) => AstNodeKind::ConstInitVal,
            AstNode::ConstExp(_) => AstNodeKind::ConstExp,

            AstNode::Exp(_) => AstNodeKind::Exp,
            AstNode::LOrExp(_) => AstNodeKind::LOrExp,
            AstNode::LAndExp(_) => AstNodeKind::LAndExp,
            AstNode::EqExp(_) => AstNodeKind::EqExp,
            AstNode::RelExp(_) => AstNodeKind::RelExp,
            AstNode::AddExp(_) => AstNodeKind::AddExp,
            AstNode::MulExp(_) => AstNodeKind::MulExp,
            AstNode::UnaryExp(_) => AstNodeKind::UnaryExp,
            AstNode::PrimaryExp(_) => AstNodeKind::PrimaryExp,
            AstNode::LVal(_) => AstNodeKind::LVal,
            AstNode::Number(_) => AstNodeKind::Number,
        }
    }
}

#[derive(PartialEq)]
pub enum AstNodeKind {
    CompUnit,
    FuncDef,

    Block,
    BlockItem,
    Stmt,
    Decl,

    ConstDecl,
    BType,
    ConstDef,
    ConstInitVal,
    ConstExp,

    Exp,
    LOrExp,
    LAndExp,
    EqExp,
    RelExp,
    AddExp,
    MulExp,
    UnaryExp,
    PrimaryExp,
    LVal,
    Number,
}

impl AstNodeKind {
    pub fn is_expression(&self) -> bool {
        match self {
            Self::Exp
            | Self::LOrExp
            | Self::LAndExp
            | Self::EqExp
            | Self::RelExp
            | Self::AddExp
            | Self::MulExp
            | Self::UnaryExp
            | Self::PrimaryExp
            | Self::LVal
            | Self::Number => true,
            _ => false,
        }
    }
}

pub enum TraversalStep<'a> {
    Enter(AstNode<'a>),
    Leave(AstNode<'a>),
}

#[derive(Debug)]
pub struct CompUnit {
    pub func_def: FuncDef,
}

impl CompUnit {
    pub fn traversal(&self, sink: &mut dyn FnMut(&TraversalStep)) {
        sink(&TraversalStep::Enter(AstNode::CompUnit(self)));
        self.func_def.traversal(sink);
        sink(&TraversalStep::Leave(AstNode::CompUnit(self)));
    }
}

#[derive(Debug)]
pub struct FuncDef {
    pub func_type: FuncType,
    pub ident: String,
    pub block: Block,
}

impl FuncDef {
    pub fn traversal(&self, sink: &mut dyn FnMut(&TraversalStep)) {
        sink(&TraversalStep::Enter(AstNode::FuncDef(self)));
        self.block.traversal(sink);
        sink(&TraversalStep::Leave(AstNode::FuncDef(self)));
    }
}

#[derive(Debug)]
pub enum FuncType {
    Int,
}

#[derive(Debug)]
pub struct Block {
    pub block_items: Vec<BlockItem>,
}

impl Block {
    pub fn traversal(&self, sink: &mut dyn FnMut(&TraversalStep)) {
        sink(&TraversalStep::Enter(AstNode::Block(self)));
        for block_item in self.block_items.iter() {
            block_item.traversal(sink);
        }
        sink(&TraversalStep::Leave(AstNode::Block(self)));
    }
}

#[derive(Debug)]
pub enum BlockItem {
    Stmt(Stmt),
    Decl(Decl),
}

impl BlockItem {
    pub fn traversal(&self, sink: &mut dyn FnMut(&TraversalStep)) {
        match self {
            BlockItem::Stmt(stmt) => stmt.traversal(sink),
            BlockItem::Decl(decl) => decl.traversal(sink),
        }
    }
}

#[derive(Debug)]
pub enum Decl {
    ConstDecl(ConstDecl),
}

impl Decl {
    pub fn traversal(&self, sink: &mut dyn FnMut(&TraversalStep)) {
        match self {
            Decl::ConstDecl(const_decl) => const_decl.traversal(sink),
        }
    }
}

/// only support return exp;
#[derive(Debug)]
pub struct Stmt {
    pub exp: Box<Exp>,
}

impl Stmt {
    pub fn traversal(&self, sink: &mut dyn FnMut(&TraversalStep)) {
        sink(&TraversalStep::Enter(AstNode::Stmt(self)));
        self.exp.traversal(sink);
        sink(&TraversalStep::Leave(AstNode::Stmt(self)));
    }
}
