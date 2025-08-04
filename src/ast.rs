use std::fmt;
use peg;

pub type Variable = String;


#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub enum Binop {
    And, Or, Xor, Add, Sub, Sll, Sra, Srl,
    Equal, NotEqual, LessThan, ULessThan, LessEqual, ULessEqual
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub enum Unop {
    Not, Neg
}

impl fmt::Display for Binop {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Binop::And => write!(f, "&"),
            Binop::Or => write!(f, "|"),
            Binop::Xor => write!(f, "^"),
            Binop::Add => write!(f, "+"),
            Binop::Sub => write!(f, "-"),
            Binop::Sll => write!(f, "<<"),
            Binop::Sra => write!(f, ">>a"),
            Binop::Srl => write!(f, ">>l"),
            Binop::Equal => write!(f, "=="),
            Binop::NotEqual => write!(f, "!="),
            Binop::LessThan => write!(f, "<s"),
            Binop::LessEqual => write!(f, "<=s"),
            Binop::ULessThan => write!(f, "<u"),
            Binop::ULessEqual => write!(f, "<=u"),
        }
    }
}

impl fmt::Display for Unop {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Unop::Not => write!(f, "~"),
            Unop::Neg => write!(f, "-"),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum ExprCore {
    Constant(isize),
    Variable(Variable),
    Binop(Binop, Expr, Expr),
    Unop(Unop, Expr),
    Call(String, Vec<Expr>),
    Deref(Expr),
}

type LineCol = peg::str::LineCol;

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Expr {
    pub expr: Box<ExprCore>,
    pub begin: LineCol,
    pub end: LineCol,
}

impl Expr {
    fn binop(op: Binop, e1: Expr, e2: Expr, begin: LineCol, end: LineCol) -> Self {
        Self {
            expr: Box::new(ExprCore::Binop(op, e1, e2)),
            begin,
            end
        }
    }

    fn unop(op: Unop, e: Expr, begin: LineCol, end: LineCol) -> Self {
        Self {
            expr: Box::new(ExprCore::Unop(op, e)),
            begin,
            end
        }
    }

    fn deref(e: Expr, begin: LineCol, end: LineCol) -> Self {
        Self {
            expr: Box::new(ExprCore::Deref(e)),
            begin,
            end
        }
    }

    fn call(s: String, args: Vec<Expr>, begin: LineCol, end: LineCol) -> Self {
        Self {
            expr: Box::new(ExprCore::Call(s, args)),
            begin,
            end
        }
    }

    fn variable(s: String, begin: LineCol, end: LineCol) -> Self {
        Self {
            expr: Box::new(ExprCore::Variable(s)),
            begin,
            end
        }
    }

    fn number(x: isize, begin: LineCol, end: LineCol) -> Self {
        Self {
            expr: Box::new(ExprCore::Constant(x)),
            begin,
            end
        }
    }
}


#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Stmt {
    Decl(String),
    Nop,
    Seq(Box<Stmt>, Box<Stmt>),
    Assign(Variable, Expr),
    While(Expr, Box<Stmt>),
    Ite(Expr, Box<Stmt>, Box<Stmt>),
    Return(Expr),
    Break,
    Continue,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Decl{
    Variable(String, isize),
    Function(String, Vec<String>, Stmt),
    Seq(Box<Decl>, Box<Decl>),
}

peg::parser!(pub grammar customlang() for str {
    rule location() -> peg::str::LineCol =
        #{|input, pos| peg::RuleResult::Matched(pos, peg::Parse::position_repr(input, pos))}

    pub rule expr() -> Expr = precedence!{
        x:(@) _ "==" _ y:@ {
            let begin = x.begin;
            let end = y.end;
            Expr::binop(Binop::Equal, x, y, begin, end)
        }
        x:(@) _ "!=" _ y:@ {
            let begin = x.begin;
            let end = y.end;
            Expr::binop(Binop::NotEqual, x, y, begin, end)
        }
        x:(@) _ "<s" _ y:@ {
            let begin = x.begin;
            let end = y.end;
            Expr::binop(Binop::LessThan, x, y, begin, end)
        }
        x:(@) _ "<u" _ y:@ {
            let begin = x.begin;
            let end = y.end;
            Expr::binop(Binop::ULessThan, x, y, begin, end)
        }
        x:(@) _ "<=s" _ y:@ {
            let begin = x.begin;
            let end = y.end;
            Expr::binop(Binop::LessEqual, x, y, begin, end)
        }
        x:(@) _ "<=u" _ y:@ {
            let begin = x.begin;
            let end = y.end;
            Expr::binop(Binop::ULessEqual, x, y, begin, end)
        }
        x:(@) _ "+" _ y:@ {
            let begin = x.begin;
            let end = y.end;
            Expr::binop(Binop::Add, x, y, begin, end)
        }
        x:(@) _ "-" _ y:@ {
            let begin = x.begin;
            let end = y.end;
            Expr::binop(Binop::Sub, x, y, begin, end)
        }
        x:(@) _ "&" _ y:@ {
            let begin = x.begin;
            let end = y.end;
            Expr::binop(Binop::And, x, y, begin, end)
        }
        x:(@) _ "|" _ y:@ {
            let begin = x.begin;
            let end = y.end;
            Expr::binop(Binop::Or, x, y, begin, end)
        }
        x:(@) _ "^" _ y:@ {
            let begin = x.begin;
            let end = y.end;
            Expr::binop(Binop::Xor, x, y, begin, end)
        }
        x:(@) _ "<<" _ y:@ {
            let begin = x.begin;
            let end = y.end;
            Expr::binop(Binop::Sll, x, y, begin, end)
        }
        x:(@) _ ">>a" _ y:@ {
            let begin = x.begin;
            let end = y.end;
            Expr::binop(Binop::Sra, x, y, begin, end)
        }
        x:(@) _ ">>l" _ y:@ {
            let begin = x.begin;
            let end = y.end;
            Expr::binop(Binop::Srl, x, y, begin, end)
        }
        begin:location() "-" _ x:@
            { let end = x.end; Expr::unop(Unop::Neg, x, begin, end) }
        begin:location() "~" _ x:@
            { let end= x.end; Expr::unop(Unop::Not, x, begin, end) }
        begin:location() "*" _ x:@
            { let end = x.end; Expr::deref(x, begin, end) }
        x:@ _ "[" _ e:expr() _ "]" l:location() {
            let begin = x.begin;
            Expr::deref(Expr::binop(Binop::Add, x, e, begin, l), begin, l)
        }
        begin:location() s:variable() _ "(" args:(expr() ** ",") ")" end:location()
            { Expr::call(s, args, begin, end) }
        begin:location() s:variable() end:location()
            { Expr::variable(s, begin, end) }
        begin:location() n:number() end:location()
            { Expr::number(n, begin, end) }
    }

    rule _ = quiet!{[' ' | '\n' | '\t']*}

    pub rule stmt_core() -> Stmt = precedence!{
        "let" _ v:variable() _ "=" _ e:expr() _ ";"
            { Stmt::Seq(Box::new(Stmt::Decl(v.clone())), Box::new(Stmt::Assign(v, e))) }
        s1:(@) _ s2:@
            { Stmt::Seq(Box::new(s1), Box::new(s2)) }
        "nop" _ ";"
            { Stmt::Nop }
        "while" _ e:expr() _ "{" _ s:stmt_core() _ "}"
            { Stmt::While(e, Box::new(s)) }
        "if" _ e:expr() _ "then" _ "{" _ s1:stmt_core() _ "}" _ "else" _ "{" _ s2:stmt_core() _ "}"
            { Stmt::Ite(e, Box::new(s1), Box::new(s2)) }
        "return" _ e:expr() _ ";"
            { Stmt::Return(e) }
        v:variable() _ "=" _ e:expr() _ ";"
            { Stmt::Assign(v, e) }
        "break" _ ";"
            { Stmt::Break }
        "continue" _ ";"
            { Stmt::Continue }
    }

    pub rule stmt() -> Stmt =
        _ s:stmt_core() _ { s }

    pub rule decl_core() -> Decl = precedence!{
        d1:(@) _ d2:@
            { Decl::Seq(Box::new(d1), Box::new(d2)) }
        "def" _ s:variable() _ "(" args:(_variable_() ** ",") ")" _ "{" body:stmt() "}"
            { Decl::Function(s, args, body) }
        "var" _ s:variable() _ ":=" n:number() ";"
            { Decl::Variable(s, n) }
    }

    pub rule decl() -> Decl = precedence!{
        _ d:decl_core() _ { d }
    }

    rule number() -> isize
        = n:$(['0'..='9']+) {? n.parse().or(Err("isize")) }

    rule variable() -> String
        = s:$(['a'..='z' | 'A'..='Z' | '_']['a'..='z' | 'A'..='Z' | '0'..='9' | '_']*) { s.into() }

    rule _variable_() -> String
        = _ s:variable() _ { s }
});
