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
    Constant(i32),
    Variable(Variable),
    Binop(Binop, Expr, Expr),
    Unop(Unop, Expr),
    Call(String, Vec<Expr>),
    Deref(Expr),
}

pub type LineCol = peg::str::LineCol;

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

    fn number(x: i32, begin: LineCol, end: LineCol) -> Self {
        Self {
            expr: Box::new(ExprCore::Constant(x)),
            begin,
            end
        }
    }
}


#[derive(Clone, PartialEq, Eq, Debug)]
pub enum StmtCore {
    Decl(String),
    Nop,
    Seq(Stmt, Stmt),
    Assign(Variable, Expr),
    While(Expr, Stmt),
    Ite(Expr, Stmt, Stmt),
    Return(Expr),
    Break,
    Continue,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Stmt {
    pub stmt: Box<StmtCore>,
    pub begin: LineCol,
    pub end: LineCol,
}

pub fn show_error(msg: &str, program: &str, begin: LineCol, end: LineCol) {
    let lines: Vec<&str> = program.lines().collect();

    if begin.line == end.line {
        println!("{msg} at line {} column {}-{}:", begin.line, begin.column, end.column);
    } else {
        println!("{msg} at line {}-{}:", begin.line, end.line);
    }

    let fst = begin.line as isize - 3;
    let lst = end.line as isize + 1;

    let red: String = "\x1b[31m".to_string();
    let white: String = "\x1b[0m".to_string();
    let mut current_color: &str = &white;

    for n in fst..=lst {
        if n >= 0 && n < lines.len() as isize {
            let n = n as usize;
            let line = lines[n];

            let digit_color =
                if begin.line - 1 <= n && n <= end.line - 1 {&red} else {&white};
            print!("{digit_color}{}:\t{current_color}", n+1);

            if n == begin.line - 1 && n == end.line - 1 {
                let (x, y) = line.split_at(begin.column-1);
                let (y, z) = y.split_at(end.column-begin.column);
                println!("{}{red}{}{white}{}", x.to_string(), y.to_string(), z.to_string());
            } else if n == begin.line - 1 {
                let (x, y) = line.split_at(begin.column-1);
                println!("{}{red}{}", x.to_string(), y.to_string());
                current_color = &red;
            } else if n == end.line - 1 {
                let (x, y) = line.split_at(end.column-1);
                println!("{}{white}{}", x.to_string(), y.to_string());
                current_color = &white;
            } else {
                println!("{}", lines[n as usize]);
            }
        }
    }
}

impl Stmt {
    fn decl(s: String, begin: LineCol, end: LineCol) -> Self {
        Self {
            stmt: Box::new(StmtCore::Decl(s)),
            begin,
            end,
        }
    }

    fn nop(begin: LineCol, end: LineCol) -> Self {
        Self {
            stmt: Box::new(StmtCore::Nop),
            begin,
            end,
        }
    }

    fn _break_(begin: LineCol, end: LineCol) -> Self {
        Self {
            stmt: Box::new(StmtCore::Break),
            begin,
            end,
        }
    }

    fn _continue_(begin: LineCol, end: LineCol) -> Self {
        Self {
            stmt: Box::new(StmtCore::Continue),
            begin,
            end,
        }
    }

    fn seq(s1: Stmt, s2: Stmt, begin: LineCol, end: LineCol) -> Self {
        Self {
            stmt: Box::new(StmtCore::Seq(s1, s2)),
            begin,
            end,
        }
    }

    fn assign(s: Variable, e: Expr, begin: LineCol, end: LineCol) -> Self {
        Self {
            stmt: Box::new(StmtCore::Assign(s, e)),
            begin,
            end,
        }
    }

    fn _while_(cond: Expr, body: Stmt, begin: LineCol, end: LineCol) -> Self {
        Self {
            stmt: Box::new(StmtCore::While(cond, body)),
            begin,
            end,
        }
    }

    fn ite(cond: Expr, s1: Stmt, s2: Stmt, begin: LineCol, end: LineCol) -> Self {
        Self {
            stmt: Box::new(StmtCore::Ite(cond, s1, s2)),
            begin,
            end,
        }
    }

    fn _return_(e: Expr, begin: LineCol, end: LineCol) -> Self {
        Self {
            stmt: Box::new(StmtCore::Return(e)),
            begin,
            end,
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum DeclCore{
    Variable(String, i32),
    Function(String, Vec<String>, Stmt),
    Seq(Decl, Decl),
    Empty,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Decl {
    pub decl: Box<DeclCore>,
    pub begin: LineCol,
    pub end: LineCol,
}

impl Decl {
    fn variable(s: String, x: i32, begin: LineCol, end: LineCol) -> Self {
        Self {
            decl: Box::new(DeclCore::Variable(s, x)),
            begin,
            end
        }
    }

    fn function(s: String, args: Vec<String>, body: Stmt, begin: LineCol, end: LineCol) -> Self {
        Self {
            decl: Box::new(DeclCore::Function(s, args, body)),
            begin,
            end
        }
    }

    fn seq(d1: Decl, d2: Decl, begin: LineCol, end: LineCol) -> Self {
        Self {
            decl: Box::new(DeclCore::Seq(d1, d2)),
            begin,
            end
        }
    }

    fn empty(pos: LineCol) -> Self {
        Self {
            decl: Box::new(DeclCore::Empty),
            begin: pos,
            end: pos
        }
    }
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
        begin:location() "let" _ v:variable() _ ":=" _ e:expr() _ ";" end:location() {
            Stmt::seq(
                Stmt::decl(v.clone(), begin, end),
                Stmt::assign(v, e, begin, end),
                begin,
                end
            )
        }
        s1:(@) _ s2:@ {
            let begin = s1.begin;
            let end = s2.end;
            Stmt::seq(s1, s2, begin, end)
        }
        begin:location() "nop" _ ";" end:location()
            { Stmt::nop(begin, end) }
        begin:location() "while" _ e:expr() _ "{" _ s:stmt_core() _ "}" end:location()
            { Stmt::_while_(e, s, begin, end) }
        begin:location()
            "if" _ e:expr() _ _ "{" _ s1:stmt_core() _ "}" _
            "else" _ "{" _ s2:stmt_core() _ "}" end:location()
            { Stmt::ite(e, s1, s2, begin, end) }
        begin:location() "return" _ e:expr() _ ";" end:location()
            { Stmt::_return_(e, begin, end) }
        begin:location() v:variable() _ "=" _ e:expr() _ ";" end:location()
            { Stmt::assign(v, e, begin, end) }
        begin:location() "break" _ ";" end:location()
            { Stmt::_break_(begin, end) }
        begin:location() "continue" _ ";" end:location()
            { Stmt::_continue_(begin, end) }
    }

    pub rule stmt() -> Stmt =
        _ s:stmt_core() _ { s }

    pub rule decl_core() -> Decl = precedence!{
        d1:(@) _ d2:@ {
            let begin = d1.begin;
            let end = d2.end;
            Decl::seq(d1, d2, begin, end)
        }
        begin:location() "def" _ s:variable() _ "(" args:(_variable_() ** ",") ")" _
            "{" body:stmt() "}" end:location()
            { Decl::function(s, args, body, begin, end) }
        begin:location() "var" _ s:variable() _ ":=" _ n:number() _ ";" end:location()
            { Decl::variable(s, n, begin, end) }
    }

    pub rule decl() -> Decl = precedence!{
        _ d:decl_core() _ { d }
        l:location() _ { Decl::empty(l) }
    }

    rule number() -> i32
        = n:$(['0'..='9']+) {? n.parse().or(Err("i32")) }

    rule variable() -> String
        = s:$(['a'..='z' | 'A'..='Z' | '_']['a'..='z' | 'A'..='Z' | '0'..='9' | '_']*) { s.into() }

    rule _variable_() -> String
        = _ s:variable() _ { s }
});
