use crate::ir_enhance::ir_builder::*;
use koopa::front::ast::Ast;
use koopa::ir::{builder_traits::*, *};
use log::info;

use self::decl::*;
use self::expr::*;

pub mod decl;
pub mod expr;

pub enum AstNode<'a> {
    CompUnit(&'a CompUnit),
    FuncDef(&'a FuncDef),
    FuncFParam(&'a FuncFParam),

    Block(&'a Block),
    BlockItem(&'a BlockItem),
    Decl(&'a Decl),

    Stmt(&'a Stmt),
    IfCond(&'a IfCond),
    ThenStmt(&'a ThenStmt),
    ElseStmt(&'a ElseStmt),

    WhileCond(&'a WhileCond),
    WhileBody(&'a WhileBody),

    ConstDecl(&'a ConstDecl),
    BType(&'a BType),
    ConstDef(&'a ConstDef),
    ConstInitVal(&'a ConstInitVal),
    ConstExp(&'a ConstExp),

    VarDecl(&'a VarDecl),
    VarDef(&'a VarDef),
    InitVal(&'a InitVal),

    Exp(&'a Exp),
    LOrExp(&'a LOrExp),
    LAndExp(&'a LAndExp),
    EqExp(&'a EqExp),
    RelExp(&'a RelExp),
    AddExp(&'a AddExp),
    MulExp(&'a MulExp),
    UnaryExp(&'a UnaryExp),
    FuncCall(&'a FuncCall),
    PrimaryExp(&'a PrimaryExp),
    LVal(&'a LVal),
    Number(&'a i32),
}

impl AstNode<'_> {
    pub fn get_kind(&self) -> AstNodeKind {
        match self {
            AstNode::CompUnit(_) => AstNodeKind::CompUnit,
            AstNode::FuncDef(_) => AstNodeKind::FuncDef,

            AstNode::FuncFParam(_) => AstNodeKind::FuncFParam,

            AstNode::Block(_) => AstNodeKind::Block,
            AstNode::BlockItem(_) => AstNodeKind::BlockItem,
            AstNode::Decl(_) => AstNodeKind::Decl,

            AstNode::Stmt(_) => AstNodeKind::Stmt,
            AstNode::IfCond(_) => AstNodeKind::IfCond,
            AstNode::ThenStmt(_) => AstNodeKind::ThenStmt,
            AstNode::ElseStmt(_) => AstNodeKind::ElseStmt,

            AstNode::WhileCond(_) => AstNodeKind::WhileCond,
            AstNode::WhileBody(_) => AstNodeKind::WhileBody,

            AstNode::ConstDecl(_) => AstNodeKind::ConstDecl,
            AstNode::BType(_) => AstNodeKind::BType,
            AstNode::ConstDef(_) => AstNodeKind::ConstDef,
            AstNode::ConstInitVal(_) => AstNodeKind::ConstInitVal,
            AstNode::ConstExp(_) => AstNodeKind::ConstExp,

            AstNode::VarDecl(_) => AstNodeKind::VarDecl,
            AstNode::VarDef(_) => AstNodeKind::VarDef,
            AstNode::InitVal(_) => AstNodeKind::InitVal,

            AstNode::Exp(_) => AstNodeKind::Exp,
            AstNode::LOrExp(_) => AstNodeKind::LOrExp,
            AstNode::LAndExp(_) => AstNodeKind::LAndExp,
            AstNode::EqExp(_) => AstNodeKind::EqExp,
            AstNode::RelExp(_) => AstNodeKind::RelExp,
            AstNode::AddExp(_) => AstNodeKind::AddExp,
            AstNode::MulExp(_) => AstNodeKind::MulExp,
            AstNode::UnaryExp(_) => AstNodeKind::UnaryExp,
            AstNode::FuncCall(_) => AstNodeKind::FuncCall,
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
    FuncFParam,

    Block,
    BlockItem,
    Decl,

    Stmt,
    IfCond,
    ThenStmt,
    ElseStmt,
    WhileCond,
    WhileBody,

    ConstDecl,
    BType,
    ConstDef,
    ConstInitVal,
    ConstExp,

    VarDecl,
    VarDef,
    InitVal,

    Exp,
    LOrExp,
    LAndExp,
    EqExp,
    RelExp,
    AddExp,
    MulExp,
    UnaryExp,
    FuncCall,
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

pub trait Traversal {
    fn traversal(&self, sink: &mut dyn FnMut(&TraversalStep));
}

#[derive(Debug)]
pub struct CompUnit {
    pub items: Vec<CompUnitItem>,
}

impl Traversal for CompUnit {
    fn traversal(&self, sink: &mut dyn FnMut(&TraversalStep)) {
        sink(&TraversalStep::Enter(AstNode::CompUnit(self)));
        for item in self.items.iter() {
            item.traversal(sink);
        }
        sink(&TraversalStep::Leave(AstNode::CompUnit(self)));
    }
}

#[derive(Debug)]
pub enum CompUnitItem {
    FuncDef(FuncDef),
    Decl(Decl),
}

impl Traversal for CompUnitItem {
    fn traversal(&self, sink: &mut dyn FnMut(&TraversalStep)) {
        // sink(&TraversalStep::Enter(AstNode::CompUnitItem(self)));
        match self {
            CompUnitItem::FuncDef(func_def) => func_def.traversal(sink),
            CompUnitItem::Decl(decl) => decl.traversal(sink),
        }
        // sink(&TraversalStep::Leave(AstNode::CompUnitItem(self)));
    }
}

#[derive(Debug)]
pub struct FuncDef {
    pub func_type: FuncType,
    pub ident: String,
    pub func_f_params: Vec<FuncFParam>,
    pub block: Block,
}

impl Traversal for FuncDef {
    fn traversal(&self, sink: &mut dyn FnMut(&TraversalStep)) {
        sink(&TraversalStep::Enter(AstNode::FuncDef(self)));
        for func_f_param in self.func_f_params.iter() {
            func_f_param.traversal(sink);
        }
        self.block.traversal(sink);
        sink(&TraversalStep::Leave(AstNode::FuncDef(self)));
    }
}

#[derive(Debug, Clone)]
pub struct FuncFParam {
    pub b_type: BType,
    pub ident: String,
}

impl Into<(Option<String>, Type)> for FuncFParam {
    fn into(self) -> (Option<String>, Type) {
        (Some("@".to_owned() + &self.ident), self.b_type.into())
    }
}

impl Traversal for FuncFParam {
    fn traversal(&self, sink: &mut dyn FnMut(&TraversalStep)) {
        sink(&TraversalStep::Enter(AstNode::FuncFParam(self)));
        sink(&TraversalStep::Leave(AstNode::FuncFParam(self)));
    }
}

#[derive(Debug, Clone)]
pub enum FuncType {
    Int,
    Void,
}

impl Into<Type> for FuncType {
    fn into(self) -> Type {
        match self {
            FuncType::Int => Type::get_i32(),
            FuncType::Void => Type::get_unit(),
        }
    }
}

#[derive(Debug)]
pub struct Block {
    pub block_items: Vec<BlockItem>,
}

impl Traversal for Block {
    fn traversal(&self, sink: &mut dyn FnMut(&TraversalStep)) {
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

impl Traversal for BlockItem {
    fn traversal(&self, sink: &mut dyn FnMut(&TraversalStep)) {
        sink(&TraversalStep::Enter(AstNode::BlockItem(self)));
        match self {
            BlockItem::Stmt(stmt) => stmt.traversal(sink),
            BlockItem::Decl(decl) => decl.traversal(sink),
        }
        sink(&TraversalStep::Leave(AstNode::BlockItem(self)));
    }
}

#[derive(Debug)]
pub enum Decl {
    ConstDecl(ConstDecl),
    VarDecl(VarDecl),
}

impl Traversal for Decl {
    fn traversal(&self, sink: &mut dyn FnMut(&TraversalStep)) {
        match self {
            Decl::ConstDecl(const_decl) => const_decl.traversal(sink),
            Decl::VarDecl(var_decl) => var_decl.traversal(sink),
        }
    }
}

/// only support return exp;
#[derive(Debug)]
pub enum Stmt {
    AssignStmt(LVal, Box<Exp>),
    ExpStmt(Option<Box<Exp>>),
    BlockStmt(Block),
    ReturnExp(Box<Exp>),
    BreakStmt,
    ContinueStmt,

    IfElseStmt(IfCond, ThenStmt, Option<ElseStmt>),
    WhileStmt(WhileCond, WhileBody),
}

impl Traversal for Stmt {
    fn traversal(&self, sink: &mut dyn FnMut(&TraversalStep)) {
        sink(&TraversalStep::Enter(AstNode::Stmt(self)));
        match self {
            Stmt::ReturnExp(exp) => {
                exp.traversal(sink);
            }
            Stmt::AssignStmt(l_val, exp) => {
                l_val.traversal(sink);
                exp.traversal(sink);
            }
            Stmt::ExpStmt(exp) => {
                if let Some(exp) = exp {
                    exp.traversal(sink);
                }
            }
            Stmt::BlockStmt(block) => {
                block.traversal(sink);
            }
            Stmt::BreakStmt | Stmt::ContinueStmt => {}
            Stmt::IfElseStmt(exp, then_stmt, else_stmt) => {
                exp.traversal(sink);
                then_stmt.traversal(sink);
                if let Some(else_stmt) = else_stmt {
                    else_stmt.traversal(sink);
                }
            }
            Stmt::WhileStmt(cond, body) => {
                cond.traversal(sink);
                body.traversal(sink);
            }
        }
        sink(&TraversalStep::Leave(AstNode::Stmt(self)));
    }
}

#[derive(Debug)]
pub struct IfCond {
    pub exp: Box<Exp>,
}

impl Traversal for IfCond {
    fn traversal(&self, sink: &mut dyn FnMut(&TraversalStep)) {
        sink(&TraversalStep::Enter(AstNode::IfCond(self)));
        self.exp.traversal(sink);
        sink(&TraversalStep::Leave(AstNode::IfCond(self)));
    }
}

#[derive(Debug)]
pub struct ThenStmt {
    pub stmt: Box<Stmt>,
}

impl Traversal for ThenStmt {
    fn traversal(&self, sink: &mut dyn FnMut(&TraversalStep)) {
        sink(&TraversalStep::Enter(AstNode::ThenStmt(self)));
        self.stmt.traversal(sink);
        sink(&TraversalStep::Leave(AstNode::ThenStmt(self)));
    }
}

#[derive(Debug)]
pub struct ElseStmt {
    pub stmt: Box<Stmt>,
}

impl Traversal for ElseStmt {
    fn traversal(&self, sink: &mut dyn FnMut(&TraversalStep)) {
        sink(&TraversalStep::Enter(AstNode::ElseStmt(self)));
        self.stmt.traversal(sink);
        sink(&TraversalStep::Leave(AstNode::ElseStmt(self)));
    }
}

#[derive(Debug)]
pub struct WhileCond(pub Box<Exp>);

impl Traversal for WhileCond {
    fn traversal(&self, sink: &mut dyn FnMut(&TraversalStep)) {
        sink(&TraversalStep::Enter(AstNode::WhileCond(self)));
        self.0.traversal(sink);
        sink(&TraversalStep::Leave(AstNode::WhileCond(self)));
    }
}

#[derive(Debug)]
pub struct WhileBody(pub Box<Stmt>);

impl Traversal for WhileBody {
    fn traversal(&self, sink: &mut dyn FnMut(&TraversalStep)) {
        sink(&TraversalStep::Enter(AstNode::WhileBody(self)));
        self.0.traversal(sink);
        sink(&TraversalStep::Leave(AstNode::WhileBody(self)));
    }
}
