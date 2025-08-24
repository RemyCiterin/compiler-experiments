use super::*;

use crate::ast::{Binop, Unop};

use crate::decl_ast;

#[derive(Eq, PartialEq, Clone, Debug)]
pub enum IntSize {
    I32,
    U32
}

decl_ast! {
    enum TypeCore Type {
        Ident ident(name: String),
        Pointer pointer(inner: Type),
        Int int(size: IntSize),
        Array array(inner: Type, size: usize),
        Void void()
    }
}

decl_ast! {
    enum LValueCore LValue {
        Variable variable(name: String),
        LField lfield(lvalue: LValue, name: String),
        RField rfield(rvalue: RValue, name: String),
        Deref defer(rvalue: RValue)
    }
}

decl_ast! {
    enum RValueCore RValue {
        Constant constant(value: i32),
        Binop binop(binop: Binop, lhs: RValue, rhs: RValue),
        And and(lhs: RValue, rhs: RValue),
        Or or(lhs: RValue, rhs: RValue),
        Call call(name: String, args: Vec<RValue>),
        Unop unop(unop: Unop, arg: RValue),
        LValue lvalue(lvalue: LValue),
        Ref reference(lvalue: LValue),
        Cast cast(rvalue: RValue, ty: Type)
    }
}

decl_ast!{
    enum StmtCore Stmt {
        // Declaration of variables
        Decl decl(name: String, ty: Type),
        DeclArray decl_array(name: String, ty: Type, size: usize),

        // Composition rules
        Nop nop(),
        Seq seq(lhs: Stmt, rhs: Stmt),

        // Control flow
        While _while_(cond: RValue, body: Stmt),
        Ite ite(cond: RValue, lhs: Stmt, rhs: Stmt),
        It it(cond: RValue, body: Stmt),
        Return _return_(expr: RValue),
        Break _break_(),
        Continue _continue_(),

        // sub-expressions and assignations
        Scope scope(body: Stmt),
        Expr expr(rvalue: RValue),
        Assign assign(lvalue: LValue, rvalue: RValue)
    }
}

decl_ast! {
    enum DeclCore Decl{
        Import import(path: String),
        // Type declaration
        Record record(name: String, fields: Vec<(String, Type)>),
        Alias alias(name: String, inner: Type),

        // Variable and array declaration
        Variable variable(name: String, ty: Type),
        Array array(name: String, values: Vec<i32>),

        // Function declaration
        Function function(name: String, args: Vec<(String, Type)>, body: Stmt, ret: Type, public: bool),

        // Composition rules
        Seq seq(lhs: Decl, rhs: Decl),
        Empty empty()
    }
}
