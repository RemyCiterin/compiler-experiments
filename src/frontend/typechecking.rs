
use super::*;
use super::ast::*;

use std::collections::{HashMap, HashSet};

pub enum TypeInfo {
    Ident{name: String},
    Pointer{inner: Box<TypeInfo>},
    Int{size: IntSize},
    Array{inner: Box<TypeInfo>, size: usize},
    Record{fields: Vec<(String, TypeInfo)>},
    Void
}

pub struct TypeError {
    pub message: String,
    pub span: Span
}

impl TypeInfo {
    pub fn from_type(ty: &Type) -> TypeInfo {
        match *ty.core.clone() {
            TypeCore::Int{size} => TypeInfo::Int{size},
            TypeCore::Ident{name} => TypeInfo::Ident{name},
            TypeCore::Pointer{inner} =>
                TypeInfo::Pointer{inner: Box::new(Self::from_type(&inner))},
            TypeCore::Array{inner, size} =>
                TypeInfo::Array{inner: Box::new(Self::from_type(&inner)), size},
            TypeCore::Void{} => TypeInfo::Void
        }
    }

    pub fn from_record
        (rec_name: &str, record: &Vec<(String, Type)>, span: Span) -> Result<Self, TypeError> {

        // Check that each fild is defined once
        let mut set: HashSet<String> = HashSet::new();
        for (name, _) in record.iter() {
            if set.contains(name) {
                return Err(TypeError{
                    message: format!("field `{name}` is defined multiple times in `{rec_name}`"),
                    span
                });
            }

            set.insert(name.clone());
        }

        Ok(Self::Record{
            fields:
                record
                .iter()
                .map(|(name, ty)|
                    (name.clone(), Self::from_type(ty)))
                .collect()
        })
    }
}

pub struct TypeTable<'a> {
    pub types: HashMap<&'a str, TypeInfo>,
}

impl<'a> TypeTable<'a> {
    pub fn build(decl: &'a Decl) -> Result<Self, TypeError> {
        let mut types: HashMap<&'a str, TypeInfo> = HashMap::new();

        let mut dirty: Vec<&'a Decl> = vec![decl];

        while let Some(decl) = dirty.pop() {
            match *decl.core {
                DeclCore::Seq{ref lhs, ref rhs} => {
                    dirty.push(lhs);
                    dirty.push(rhs);
                }
                DeclCore::Alias{ref name, ref inner} => {
                    types.insert(name, TypeInfo::from_type(inner));
                }
                DeclCore::Record{ref name, ref fields} => {
                    let record = TypeInfo::from_record(name, fields, decl.span)?;
                    types.insert(name, record);
                }
                DeclCore::Import{..} => {}
                DeclCore::Variable{..} => {}
                DeclCore::Array{..} => {}
                DeclCore::Function{..} => {}
                DeclCore::Empty{..} => {}
            }
        }

        Ok(Self{types})
    }

    pub fn contains(&self, name: &str) -> bool {
        self.types.contains_key(name)
    }

    pub fn get<'b>(&'b self, name: &str, span: Span) -> Result<&'b TypeInfo, TypeError> {
        if let Some(x) = self.types.get(name) {
            Ok(x)
        } else {
            Err(TypeError{
                message: format!("`{name}` doesn't refer to a valid type declaration"),
                span
            })
        }
    }
}

// decl_ast! {
//     enum LValueCore LValue {
//         Variable variable(name: String),
//         LField lfield(lvalue: LValue, name: String),
//         RField rfield(rvalue: RValue, name: String),
//         Deref defer(rvalue: RValue)
//     }
// }
//
// decl_ast! {
//     enum RValueCore RValue {
//         Constant constant(value: i32),
//         Binop binop(binop: Binop, lhs: RValue, rhs: RValue),
//         And and(lhs: RValue, rhs: RValue),
//         Or or(lhs: RValue, rhs: RValue),
//         Call call(name: String, args: Vec<RValue>),
//         Unop unop(unop: Unop, arg: RValue),
//         LValue lvalue(lvalue: LValue),
//         Ref reference(lvalue: LValue),
//         Cast cast(rvalue: RValue, ty: Type)
//     }
// }
//
// decl_ast!{
//     enum StmtCore Stmt {
//         // Declaration of variables
//         Decl decl(name: String, ty: Type),
//         DeclArray decl_array(name: String, ty: Type, size: usize),
//
//         // Composition rules
//         Nop nop(),
//         Seq seq(lhs: Stmt, rhs: Stmt),
//
//         // Control flow
//         While _while_(cond: RValue, body: Stmt),
//         Ite ite(cond: RValue, lhs: Stmt, rhs: Stmt),
//         It it(cond: RValue, body: Stmt),
//         Return _return_(expr: RValue),
//         Break _break_(),
//         Continue _continue_(),
//
//         // sub-expressions and assignations
//         Scope scope(body: Stmt),
//         Expr expr(rvalue: RValue),
//         Assign assign(lvalue: LValue, rvalue: RValue)
//     }
// }
//
// decl_ast! {
//     enum DeclCore Decl{
//         Import import(path: String),
//         // Type declaration
//         Record record(name: String, fields: Vec<(String, Type)>),
//         Alias alias(name: String, inner: Type),
//
//         // Variable and array declaration
//         Variable variable(name: String, ty: Type),
//         Array array(name: String, values: Vec<i32>),
//
//         // Function declaration
//         Function function(name: String, args: Vec<(String, Type)>, body: Stmt, ret: Type, public: bool),
//
//         // Composition rules
//         Seq seq(lhs: Decl, rhs: Decl),
//         Empty empty()
//     }
// }
