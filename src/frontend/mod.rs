pub mod ast;
pub mod typechecking;
pub mod parser;

#[macro_export]
macro_rules! decl_ast {
    ( enum $core:ident $name:ident { $($const:ident $fn:ident ( $($arg:ident : $field:ty),* ) ),* } ) => {
        #[derive(Clone, PartialEq, Eq, Debug)]
        pub enum $core { $($const { $( $arg : $field ),* } ),* }

        #[derive(Clone, PartialEq, Eq, Debug)]
        pub struct $name {
            pub core: Box<$core>,
            pub span: Span,
        }

        impl $name {
            $(
                pub fn $fn ( $($arg : $field,)*  begin: LineCol, end: LineCol ) -> Self {
                    Self {
                        core: Box::new( $core::$const  {  $($arg),* } ),
                        span: Span{begin, end}
                    }
                }
            )*
        }
    };
}

pub type LineCol = peg::str::LineCol;

#[derive(Clone, PartialEq, Eq, Debug, Copy)]
pub struct Span{
    pub begin: LineCol,
    pub end: LineCol
}

pub fn show_error(msg: &str, program: &str, span: Span) {
    let lines: Vec<&str> = program.lines().collect();

    if span.begin.line == span.end.line {
        println!("{msg} at line {} column {}-{}:", span.begin.line, span.begin.column, span.end.column);
    } else {
        println!("{msg} at line {}-{}:", span.begin.line, span.end.line);
    }

    let fst = span.begin.line as isize - 3;
    let lst = span.end.line as isize + 1;

    let red: String = "\x1b[31m".to_string();
    let white: String = "\x1b[0m".to_string();
    let mut current_color: &str = &white;

    for n in fst..=lst {
        if n >= 0 && n < lines.len() as isize {
            let n = n as usize;
            let line = lines[n];

            let digit_color =
                if span.begin.line - 1 <= n && n <= span.end.line - 1 {&red} else {&white};
            print!("{digit_color}{}:\t{current_color}", n+1);

            if n == span.begin.line - 1 && n == span.end.line - 1 {
                let (x, y) = line.split_at(span.begin.column-1);
                let (y, z) = y.split_at(span.end.column-span.begin.column+1);
                println!("{}{red}{}{white}{}", x.to_string(), y.to_string(), z.to_string());
            } else if n == span.begin.line - 1 {
                let (x, y) = line.split_at(span.begin.column-1);
                println!("{}{red}{}", x.to_string(), y.to_string());
                current_color = &red;
            } else if n == span.end.line - 1 {
                let (x, y) = line.split_at(span.end.column);
                println!("{}{white}{}", x.to_string(), y.to_string());
                current_color = &white;
            } else {
                println!("{}", lines[n as usize]);
            }
        }
    }
}

const PROGRAM: &'static str = "
struct FOO {
    x: i32,
    y: *FOO,
}

int foo(x: i32, y: *FOO) {
    return x + y->x;
}
";

pub fn test() {
    let parsed = parser::customlang::decl(PROGRAM);

    match parsed {
        Ok(ok) => println!("{:?}", ok),
        Err(peg::error::ParseError{location, expected}) => {
            let msg = format!("unexpected token, expect: {expected}");
            show_error(&msg, PROGRAM, Span{begin:location, end: location});
            return;
        }
    }
}
