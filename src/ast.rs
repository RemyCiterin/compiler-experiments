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

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Expr {
    Constant(isize),
    Variable(Variable),
    Binop(Binop, Box<Expr>, Box<Expr>),
    Unop(Unop, Box<Expr>),
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Stmt {
    Nop,
    Seq(Box<Stmt>, Box<Stmt>),
    Assign(Variable, Expr),
    While(Expr, Box<Stmt>),
    Ite(Expr, Box<Stmt>, Box<Stmt>),
    Return(Expr),
    Break,
    Continue,
}

peg::parser!(pub grammar customlang() for str {
    pub rule expr() -> Expr = precedence!{
        x:(@) _ "==" _ y:@ { Expr::Binop(Binop::Equal, Box::new(x), Box::new(y)) }
        x:(@) _ "!=" _ y:@ { Expr::Binop(Binop::NotEqual, Box::new(x), Box::new(y)) }
        x:(@) _ "<s" _ y:@ { Expr::Binop(Binop::LessThan, Box::new(x), Box::new(y)) }
        x:(@) _ "<u" _ y:@ { Expr::Binop(Binop::ULessThan, Box::new(x), Box::new(y)) }
        x:(@) _ "<=s" _ y:@ { Expr::Binop(Binop::LessEqual, Box::new(x), Box::new(y)) }
        x:(@) _ "<=u" _ y:@ { Expr::Binop(Binop::ULessEqual, Box::new(x), Box::new(y)) }
        x:(@) _ "+" _ y:@ { Expr::Binop(Binop::Add, Box::new(x), Box::new(y)) }
        x:(@) _ "-" _ y:@ { Expr::Binop(Binop::Sub, Box::new(x), Box::new(y)) }
        x:(@) _ "&" _ y:@ { Expr::Binop(Binop::And, Box::new(x), Box::new(y)) }
        x:(@) _ "|" _ y:@ { Expr::Binop(Binop::Or, Box::new(x), Box::new(y)) }
        x:(@) _ "^" _ y:@ { Expr::Binop(Binop::Xor, Box::new(x), Box::new(y)) }
        x:(@) _ "<<" _ y:@ { Expr::Binop(Binop::Sll, Box::new(x), Box::new(y)) }
        x:(@) _ ">>a" _ y:@ { Expr::Binop(Binop::Sra, Box::new(x), Box::new(y)) }
        x:(@) _ ">>l" _ y:@ { Expr::Binop(Binop::Srl, Box::new(x), Box::new(y)) }
        "-" x:@ { Expr::Unop(Unop::Neg, Box::new(x)) }
        "~" x:@ { Expr::Unop(Unop::Not, Box::new(x)) }
        s:variable() { Expr::Variable(s) }
        n:number() { Expr::Constant(n) }
    }

    rule _ = quiet!{[' ' | '\n' | '\t']*}

    pub rule stmt_core() -> Stmt = precedence!{
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
        v:variable() _ ":=" _ e:expr() _ ";"
            { Stmt::Assign(v, e) }
    }

    pub rule stmt() -> Stmt =
        _ s:stmt_core() _ { s }

    rule number() -> isize
        = n:$(['0'..='9']+) {? n.parse().or(Err("isize")) }

    rule variable() -> String
        = s:$(['a'..='z' | 'A'..='Z' | '_']['a'..='z' | 'A'..='Z' | '0'..='9' | '_']*) { s.into() }

});
