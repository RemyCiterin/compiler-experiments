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

macro_rules! ast {
    ( enum $core:ident $name:ident { $($const:ident $fn:ident ( $($arg:ident : $field:ty),* ) ),* } ) => {
        #[derive(Clone, PartialEq, Eq, Debug)]
        pub enum $core { $($const { $( $arg : $field ),* } ),* }

        #[derive(Clone, PartialEq, Eq, Debug)]
        pub struct $name {
            pub core: Box<$core>,
            pub begin: LineCol,
            pub end: LineCol,
        }

        impl $name {
            $(
                pub fn $fn ( $($arg : $field,)*  begin: LineCol, end: LineCol ) -> Self {
                    Self {
                        core: Box::new( $core::$const  {  $($arg),* } ),
                        begin,
                        end,
                    }
                }
            )*
        }
    };
}

ast!{
    enum LValueCore LValue {
        Variable variable(name: Variable),
        Deref defer(rvalue: RValue)
    }
}

ast!{
    enum RValueCore RValue {
        Constant constant(value: i32),
        Binop binop(binop: Binop, lhs: RValue, rhs: RValue),
        Call call(name: String, args: Vec<RValue>),
        Unop unop(unop: Unop, arg: RValue),
        LValue lvalue(lvalue: LValue),
        Ref reference(lvalue: LValue)
    }
}


ast!{
    enum StmtCore Stmt {
        Decl decl(name: String),
        Nop nop(),
        Seq seq(lhs: Stmt, rhs: Stmt),
        Assign assign(lvalue: LValue, rvalue: RValue),
        While _while_(cond: RValue, body: Stmt),
        Ite ite(cond: RValue, lhs: Stmt, rhs: Stmt),
        Return _return_(expr: RValue),
        Break _break_(),
        Continue _continue_()
    }
}

pub type LineCol = peg::str::LineCol;


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


ast! {
    enum DeclCore Decl{
        Variable variable(name: String, value: i32),
        Array array(name: String, values: Vec<i32>),
        Function function(name: String, args: Vec<String>, body: Stmt),
        Seq seq(lhs: Decl, rhs: Decl),
        Empty empty()
    }
}


peg::parser!(pub grammar customlang() for str {
    rule location() -> peg::str::LineCol =
        #{|input, pos| peg::RuleResult::Matched(pos, peg::Parse::position_repr(input, pos))}

    pub rule rvalue() -> RValue = precedence!{
        x:(@) _ "==" _ y:@ {
            let begin = x.begin;
            let end = y.end;
            RValue::binop(Binop::Equal, x, y, begin, end)
        }
        x:(@) _ "!=" _ y:@ {
            let begin = x.begin;
            let end = y.end;
            RValue::binop(Binop::NotEqual, x, y, begin, end)
        }
        x:(@) _ "<s" _ y:@ {
            let begin = x.begin;
            let end = y.end;
            RValue::binop(Binop::LessThan, x, y, begin, end)
        }
        x:(@) _ "<u" _ y:@ {
            let begin = x.begin;
            let end = y.end;
            RValue::binop(Binop::ULessThan, x, y, begin, end)
        }
        x:(@) _ "<=s" _ y:@ {
            let begin = x.begin;
            let end = y.end;
            RValue::binop(Binop::LessEqual, x, y, begin, end)
        }
        x:(@) _ "<=u" _ y:@ {
            let begin = x.begin;
            let end = y.end;
            RValue::binop(Binop::ULessEqual, x, y, begin, end)
        }
        x:(@) _ "+" _ y:@ {
            let begin = x.begin;
            let end = y.end;
            RValue::binop(Binop::Add, x, y, begin, end)
        }
        x:(@) _ "-" _ y:@ {
            let begin = x.begin;
            let end = y.end;
            RValue::binop(Binop::Sub, x, y, begin, end)
        }
        x:(@) _ "&" _ y:@ {
            let begin = x.begin;
            let end = y.end;
            RValue::binop(Binop::And, x, y, begin, end)
        }
        x:(@) _ "|" _ y:@ {
            let begin = x.begin;
            let end = y.end;
            RValue::binop(Binop::Or, x, y, begin, end)
        }
        x:(@) _ "^" _ y:@ {
            let begin = x.begin;
            let end = y.end;
            RValue::binop(Binop::Xor, x, y, begin, end)
        }
        x:(@) _ "<<" _ y:@ {
            let begin = x.begin;
            let end = y.end;
            RValue::binop(Binop::Sll, x, y, begin, end)
        }
        x:(@) _ ">>a" _ y:@ {
            let begin = x.begin;
            let end = y.end;
            RValue::binop(Binop::Sra, x, y, begin, end)
        }
        x:(@) _ ">>l" _ y:@ {
            let begin = x.begin;
            let end = y.end;
            RValue::binop(Binop::Srl, x, y, begin, end)
        }
        begin:location() "-" _ x:@
            { let end = x.end; RValue::unop(Unop::Neg, x, begin, end) }
        begin:location() "~" _ x:@
            { let end= x.end; RValue::unop(Unop::Not, x, begin, end) }
        begin:location() s:variable() _ "(" args:(rvalue() ** ",") ")" end:location()
            { RValue::call(s, args, begin, end) }
        begin:location() "&" _ lvalue:lvalue() end:location() {
            RValue::reference(lvalue, begin, end)
        }
        begin:location() lvalue:lvalue() end:location() {
            RValue::lvalue(lvalue, begin, end)
        }
        begin:location() n:number() end:location()
            { RValue::constant(n, begin, end) }
        "(" _ e:rvalue() _ ")" { e }
    }

    pub rule lvalue() -> LValue = precedence!{
        begin:location() "*" _ rvalue:rvalue() end:location()
            { LValue::defer(rvalue, begin, end) }
        lvalue:@ _ "[" _ rvalue:rvalue() _ "]" end:location() {
            let begin = lvalue.begin;
            let two = RValue::constant(2, begin, end);
            let reference = RValue::reference(lvalue, begin, end);
            let offset = RValue::binop(Binop::Sll, rvalue, two, begin ,end);
            let addr = RValue::binop(Binop::Add, reference, offset, begin, end);
            LValue::defer(addr, begin, end)
        }
        begin:location() var:variable() end:location()
            { LValue::variable(var, begin, end) }
    }

    rule _ = ( quiet!{[' ' | '\n' | '\t']} / quiet!{ "//"[^ '\n']*['\n'] } )*

    pub rule stmt_core() -> Stmt = precedence!{
        begin:location() "let" _ v:variable() _ "=" _ e:rvalue() _ ";" end:location() {
            Stmt::seq(
                Stmt::decl(v.clone(), begin, end),
                Stmt::assign(LValue::variable(v, begin, end), e, begin, end),
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
        begin:location() "while" _ e:rvalue() _ "{" _ s:stmt_core() _ "}" end:location()
            { Stmt::_while_(e, s, begin, end) }
        begin:location()
            "if" _ e:rvalue() _ _ "{" _ s1:stmt_core() _ "}" _
            "else" _ "{" _ s2:stmt_core() _ "}" end:location()
            { Stmt::ite(e, s1, s2, begin, end) }
        begin:location() "return" _ e:rvalue() _ ";" end:location()
            { Stmt::_return_(e, begin, end) }
        begin:location() lvalue:lvalue() _ "=" _ e:rvalue() _ ";" end:location()
            { Stmt::assign(lvalue, e, begin, end) }
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
        begin:location() "var" _ s:variable() _ "=" _ n:number() _ ";" end:location()
            { Decl::variable(s, n, begin, end) }
        begin:location() "var" _ s:variable() _ "[" _ n1:number() _ "]" _
            "=" _ n2:number() _ ";" end:location()
            { Decl::array(s, std::iter::repeat(n2).take(n1 as usize).collect(), begin, end) }
    }

    pub rule decl() -> Decl = precedence!{
        _ d:decl_core() _ { d }
        l:location() _ { Decl::empty(l, l) }
    }

    rule number() -> i32
        = n:$(['0'..='9']+) {? n.parse().or(Err("i32")) }

    rule variable() -> String
        = s:$(['a'..='z' | 'A'..='Z' | '_']['a'..='z' | 'A'..='Z' | '0'..='9' | '_']*) { s.into() }

    rule _variable_() -> String
        = _ s:variable() _ { s }
});
