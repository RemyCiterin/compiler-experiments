use super::ast::*;
use crate::ast::{Binop, Unop};

use peg::*;

pub enum Binding{
    Array(String, usize),
    Var(String),
}

peg::parser!(pub grammar customlang() for str {
    rule location() -> peg::str::LineCol =
        #{|input, pos| peg::RuleResult::Matched(pos, peg::Parse::position_repr(input, pos))}

    pub rule rvalue() -> RValue = precedence!{
        x:(@) _ "||" _ y:@ {
            let begin = x.span.begin;
            let end = y.span.end;
            RValue::or(x, y, begin, end)
        }
        --
        x:(@) _ "&&" _ y:@ {
            let begin = x.span.begin;
            let end = y.span.end;
            RValue::and(x, y, begin, end)
        }
        --
        x:(@) _ "|" _ y:@ {
            let begin = x.span.begin;
            let end = y.span.end;
            RValue::binop(Binop::Or, x, y, begin, end)
        }
        --
        x:(@) _ "^" _ y:@ {
            let begin = x.span.begin;
            let end = y.span.end;
            RValue::binop(Binop::Xor, x, y, begin, end)
        }
        --
        x:(@) _ "&" _ y:@ {
            let begin = x.span.begin;
            let end = y.span.end;
            RValue::binop(Binop::And, x, y, begin, end)
        }
        --
        x:(@) _ "==" _ y:@ {
            let begin = x.span.begin;
            let end = y.span.end;
            RValue::binop(Binop::Equal, x, y, begin, end)
        }
        x:(@) _ "!=" _ y:@ {
            let begin = x.span.begin;
            let end = y.span.end;
            RValue::binop(Binop::NotEqual, x, y, begin, end)
        }
        --
        x:(@) _ "<" _ y:@ {
            let begin = x.span.begin;
            let end = y.span.end;
            RValue::binop(Binop::LessThan, x, y, begin, end)
        }
        x:(@) _ ">" _ y:@ {
            let begin = x.span.begin;
            let end = y.span.end;
            RValue::binop(Binop::LessThan, y, x, begin, end)
        }
        x:(@) _ "<=" _ y:@ {
            let begin = x.span.begin;
            let end = y.span.end;
            RValue::binop(Binop::LessEqual, x, y, begin, end)
        }
        x:(@) _ ">=" _ y:@ {
            let begin = x.span.begin;
            let end = y.span.end;
            RValue::binop(Binop::LessEqual, y, x, begin, end)
        }
        --
        x:(@) _ "<<" _ y:@ {
            let begin = x.span.begin;
            let end = y.span.end;
            RValue::binop(Binop::Sll, x, y, begin, end)
        }
        x:(@) _ ">>" _ y:@ {
            let begin = x.span.begin;
            let end = y.span.end;
            RValue::binop(Binop::Sra, x, y, begin, end)
        }
        --
        x:(@) _ "+" _ y:@ {
            let begin = x.span.begin;
            let end = y.span.end;
            RValue::binop(Binop::Add, x, y, begin, end)
        }
        --
        x:(@) _ "-" _ y:@ {
            let begin = x.span.begin;
            let end = y.span.end;
            RValue::binop(Binop::Sub, x, y, begin, end)
        }
        --
        x:(@) _ "*" _ y:@ {
            let begin = x.span.begin;
            let end = y.span.end;
            RValue::binop(Binop::Mul, x, y, begin, end)
        }
        x:(@) _ "/" _ y:@ {
            let begin = x.span.begin;
            let end = y.span.end;
            RValue::binop(Binop::SDiv, x, y, begin, end)
        }
        x:(@) _ "%" _ y:@ {
            let begin = x.span.begin;
            let end = y.span.end;
            RValue::binop(Binop::SRem, x, y, begin, end)
        }
        --
        begin:location() "-" _ x:(@)
            { let end = x.span.end; RValue::unop(Unop::Neg, x, begin, end) }
        begin:location() "~" _ x:@
            { let end= x.span.end; RValue::unop(Unop::Not, x, begin, end) }
        begin:location() "!" _ x:@ {
            let end= x.span.end;
            let zero = RValue::constant(0, begin, end);
            RValue::binop(Binop::Equal, x, zero, begin, end)
        }
        --
        begin:location() "@cast" _ "(" _ v:rvalue() _ ":" _ t:type_() _ ")" end:location() {
            RValue::cast(v, t, begin, end)
        }
        begin:location() s:ident() _ "(" args:(_rvalue_() ** ",") ")" end:location() {
            RValue::call(s, args, begin, end)
        }
        begin:location() "&" _ lvalue:lvalue() end:location() {
            RValue::reference(lvalue, begin, end)
        }
        --
        begin:location() lvalue:lvalue() end:location() {
            RValue::lvalue(lvalue, begin, end)
        }
        --
        begin:location() n:number() end:location()
            { RValue::constant(n, begin, end) }
        --
        begin:location() n:character() end:location()
            { RValue::constant(n as i32, begin, end) }
        --
        "(" _ e:rvalue() _ ")" { e }
    }

    rule _rvalue_() -> RValue =
        _ r:rvalue() _ {r}

    #[cache_left_rec]
    pub rule lvalue() -> LValue = precedence!{
        lvalue:@ _ "." _ name:ident() end:location() {
            let begin = lvalue.span.begin;
            LValue::lfield(lvalue, name, begin, end)
        }
        rvalue:rvalue() _ "->" _ name:ident() end:location() {
            let begin = rvalue.span.begin;
            LValue::rfield(rvalue, name, begin, end)
        }
        begin:location() "*" _ rvalue:rvalue() end:location()
            { LValue::defer(rvalue, begin, end) }
        lvalue:@ _ "[" _ rvalue:rvalue() _ "]" end:location() {
            let begin = lvalue.span.begin;
            let two = RValue::constant(2, begin, end);
            let reference = RValue::lvalue(lvalue, begin, end);
            let offset = RValue::binop(Binop::Sll, rvalue, two, begin ,end);
            let addr = RValue::binop(Binop::PtrAdd, reference, offset, begin, end);
            LValue::defer(addr, begin, end)
        }
        "(" _ lvalue:lvalue() _ ")"
            { lvalue }
        begin:location() var:ident() end:location()
            { LValue::variable(var, begin, end) }
    }

    rule _ = ( quiet!{[' ' | '\n' | '\t']} / quiet!{ "//"[^ '\n']*['\n'] } )*

    pub rule stmt_core() -> Stmt = precedence!{
        begin:location() "let" _ v:ident() _ ":" _ ty:type_() _ ";" end:location() {
            Stmt::decl(v.clone(), ty, begin, end)
        }
        s1:(@) _ s2:@ {
            let begin = s1.span.begin;
            let end = s2.span.end;
            Stmt::seq(s1, s2, begin, end)
        }
        begin:location() "nop" _ ";" end:location()
            { Stmt::nop(begin, end) }
        begin:location() "while" _ e:rvalue() _ "{" _ s:stmt_core() _ "}" end:location()
            { Stmt::_while_(e, s, begin, end) }
        begin:location()
            "if" _ e:rvalue() _ _ "{" _ s1:stmt_core() _ "}" _
            "else" _ "{" _ s2:stmt_core() _ "}" end:location()
            { Stmt::ite(e, s1, s2, begin, end) }
        begin:location()
            "if" _ e:rvalue() _ _ "{" _ s1:stmt_core() _ "}" _ end:location()
            { Stmt::it(e, s1, begin, end) }
        begin:location() "return" _ e:rvalue() _ ";" end:location()
            { Stmt::_return_(e, begin, end) }
        begin:location() lvalue:lvalue() _ "=" _ e:rvalue() _ ";" end:location()
            { Stmt::assign(lvalue, e, begin, end) }
        begin:location() "break" _ ";" end:location()
            { Stmt::_break_(begin, end) }
        begin:location() "continue" _ ";" end:location()
            { Stmt::_continue_(begin, end) }
        begin:location() rvalue:rvalue() _ ";" end:location()
            { Stmt::expr(rvalue, begin, end) }
        begin:location() "{" _ body:stmt_core() _ "}" end:location()
            { Stmt::scope(body, begin, end) }
    }

    pub rule stmt() -> Stmt =
        _ s:stmt_core() _ { s }

    pub rule decl_core() -> Decl = precedence!{
        begin:location() "import" _ s:ident() _ ";" end:location()
            { Decl::import(s, begin, end) }
        begin:location() "type" _ name:ident() _ "=" _ t:type_() _ ";" end:location()
            { Decl::alias(name, t, begin, end) }
        begin:location() "struct" _ name:ident() _ "{" fields:bindings() "}" end:location()
            { Decl::record(name, fields, begin, end) }
        d1:(@) _ d2:@ {
            let begin = d1.span.begin;
            let end = d2.span.end;
            Decl::seq(d1, d2, begin, end)
        }
        begin:location() ret:type_() _ s:ident() _ "(" args:bindings() ")" _
        "{" body:stmt() "}" end:location() {
                Decl::function(s, args, body, ret, false, begin, end)
        }
        begin:location() "pub" _ ret:type_() _ s:ident() _ "(" args:bindings() ")" _
        "{" body:stmt() "}" end:location() {
                Decl::function(s, args, body, ret, false, begin, end)
        }
        begin:location() "var" _ s:ident() _ ":" _ t:type_() _ ";" end:location()
            { Decl::variable(s, t, begin, end) }
        begin:location() "var" _ s:ident() _ "=" _ mem:string() _ ";" end:location() {
                let vec = mem.as_bytes()
                    .iter().map(|x| *x as i32)
                    .chain(std::iter::once(0))
                    .collect();
                Decl::array(s, vec, begin, end)
            }
    }

    pub rule decl() -> Decl = precedence!{
        _ d:decl_core() _ { d }
        l:location() _ { Decl::empty(l, l) }
    }

    rule number() -> i32
        = n:$(['0'..='9']+) {? n.parse().or(Err("i32")) }

    rule ident() -> String
        = s:$(['a'..='z' | 'A'..='Z' | '_']['a'..='z' | 'A'..='Z' | '0'..='9' | '_']*) { s.into() }

    rule builtin() -> String
        = s:$(['@']['a'..='z' | 'A'..='Z' | '_']['a'..='z' | 'A'..='Z' | '0'..='9' | '_']*)
            { s.into() }

    rule _variable_() -> String
        = _ s:ident() _ { s }

    rule binding() -> (String, Type) = precedence!{
        _ v:ident() _ ":" _ t:type_() _ { (v, t) }
    }

    rule bindings() -> Vec<(String, Type)> =
        args:(binding() ** ",") (_ "," _)*
        { args }

    rule type_() -> Type = precedence!{
        begin:location() "i32" end:location() { Type::int(IntSize::I32, begin, end) }
        begin:location() "u32" end:location() { Type::int(IntSize::U32, begin, end) }
        begin:location() "void" end:location() { Type::void(begin, end) }
        --
        begin:location() s:ident() end:location() { Type::ident(s, begin, end) }
        begin:location() "*" _ x:(@) {
            let end = x.span.end;
            Type::pointer(x, begin, end)
        }
        --
        x:(@) _ "[" _ n:number() _ "]" end:location() {
            let begin = x.span.begin;
            Type::array(x, n as usize, begin, end)
        }
    }

    rule string() -> String =
        "\"" s:$([^'"']*) "\"" { parse_string(s) }

    rule character() -> u8 =
        "'" s:([^'\'']) "'" { s as u8 }
});



pub fn parse_string(s: &str) -> String {
    let mut result = Vec::<u8>::new();
    let bytes = s.as_bytes();
    let mut i: usize = 0;


    while i < s.len() {
        if i+1 < bytes.len() && bytes[i] == b'\\' {
            match bytes[i+1] {
                b'n' => result.push(b'\n'),
                b'r' => result.push(b'\r'),
                b'\\' => result.push(b'\\'),
                b't' => result.push(b'\t'),
                b'\'' => result.push(b'\''),
                b'"' => result.push(b'"'),
                _ => panic!(),
            }

            i += 2;

            continue;
        }

        result.push(bytes[i]);
        i += 1;
    }

    String::from_utf8(result).unwrap()
}
