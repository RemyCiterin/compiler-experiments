use std::fmt;
use peg;

pub type Variable = String;


#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub enum Binop {
    /// Bitwise and
    And,

    /// Bitwise or
    Or,

    /// Bitwise xor
    Xor,

    /// Addition
    Add,

    /// Substraction
    Sub,

    /// Left shift
    Sll,

    /// Right arithmetic (signed) shift
    Sra,

    /// Right linear (unsigned) shift
    Srl,

    /// Return 1 if equals, 0 otherwise
    Equal,

    /// Return 0 if equals, 1 otherwise
    NotEqual,

    /// Signed less than, return 1 if true, 0 otherwise
    LessThan,

    /// Unsigned less than, return 1 if true, 0 otherwise
    ULessThan,

    /// Signed less than or equal, return 1 if true, 0 otherwise
    LessEqual,

    /// Unsigned less than or equal, return 1 if true, 0 otherwise
    ULessEqual
}

#[macro_export]
macro_rules! binop {
    ($i:ident) => { crate::ast::Binop::$i };
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub enum Unop {
    /// Bitwise not
    Not,

    /// Negation operator
    Neg
}

#[macro_export]
macro_rules! unop {
    ($i:ident) => { crate::ast::Unop::$i };
}

impl Unop{
    pub fn eval(&self, e: i32) -> i32 {
        match self {
            Unop::Neg => e.wrapping_neg(),
            Unop::Not => !e,
        }
    }
}

pub fn sll(lhs: i32, rhs: i32) -> i32 {
    let x = lhs.cast_unsigned();
    let y = rhs.cast_unsigned();

    let ret =
        if y >= 32 { 0 }
        else { x.wrapping_shl(y) };

    ret.cast_signed()
}

pub fn srl(lhs: i32, rhs: i32) -> i32 {
    let x = lhs.cast_unsigned();
    let y = rhs.cast_unsigned();

    let ret =
        if y >= 32 { 0 }
        else { x.wrapping_shr(y) };

    ret.cast_signed()
}

impl Binop {
    pub fn eval(&self, lhs: i32, rhs: i32) -> i32 {
        match self {
            Binop::And => lhs & rhs,
            Binop::Or => lhs | rhs,
            Binop::Xor => lhs ^ rhs,
            Binop::Add => lhs.wrapping_add(rhs),
            Binop::Sub => lhs.wrapping_sub(rhs),
            Binop::Sll => sll(lhs, rhs),
            Binop::Sra => lhs.wrapping_shr(rhs.cast_unsigned()),
            Binop::Srl => srl(lhs, rhs),
            Binop::Equal => (lhs == rhs) as i32,
            Binop::NotEqual => (lhs != rhs) as i32,
            Binop::LessThan => (lhs < rhs) as i32,
            Binop::LessEqual => (lhs <= rhs) as i32,
            Binop::ULessThan => (lhs.cast_unsigned() < rhs.cast_unsigned()) as i32,
            Binop::ULessEqual => (lhs.cast_unsigned() <= rhs.cast_unsigned()) as i32,
        }
    }


    pub fn commutative(&self) -> bool {
        match self {
            Binop::And
                | Binop::Or
                | Binop::Xor
                | Binop::Add
                | Binop::Equal
                | Binop::NotEqual
                => true,
            _ => false
        }
    }
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
        Expr expr(rvalue: RValue),
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
                let (y, z) = y.split_at(end.column-begin.column+1);
                println!("{}{red}{}{white}{}", x.to_string(), y.to_string(), z.to_string());
            } else if n == begin.line - 1 {
                let (x, y) = line.split_at(begin.column-1);
                println!("{}{red}{}", x.to_string(), y.to_string());
                current_color = &red;
            } else if n == end.line - 1 {
                let (x, y) = line.split_at(end.column);
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

