
pub enum SExpr {
    Atom(String),
    Int(i32),
    Str(String),
    App(Box<SExpr>, Vec<SExpr>),
}

impl std::fmt::Display for SExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Atom(s) => write!(f, "{s}"),
            Self::Int(x) => write!(f, "{x}"),
            Self::Str(x) => write!(f, "\"{x}\""),
            Self::App(fun, list) => {
                write!(f, "({fun}")?;
                for x in list.iter() {
                    write!(f, " {x}")?;
                }

                write!(f, ")")
            }
        }
    }
}

peg::parser!(pub grammar parser() for str {
    rule sexpr_core() -> SExpr = precedence!{
        s:variable() { SExpr::Atom(s) }
        i:number() { SExpr::Int(i) }
        s:string() { SExpr::Str(s) }
        "(" e:sexpr() args:(sexpr()*) ")" {
            if args.len() == 0 { e } else {
                SExpr::App(Box::new(e), args)
            }
        }
    }

    pub rule sexpr() -> SExpr =
        _ e:sexpr_core() _ { e }

    pub rule decl() -> Vec<SExpr> =
        exprs:(sexpr()*) { exprs }

    rule _ = ( quiet!{[' ' | '\n' | '\t']} / quiet!{ ";"[^ '\n']*['\n'] } )*

    rule location() -> peg::str::LineCol =
        #{|input, pos| peg::RuleResult::Matched(pos, peg::Parse::position_repr(input, pos))}

    rule number() -> i32
        = n:$(['0'..='9']+) {? n.parse().or(Err("i32")) }

    rule variable() -> String
        = s:$(['a'..='z' | 'A'..='Z' | '_']['a'..='z' | 'A'..='Z' | '0'..='9' | '_']*) { s.into() }

    rule _variable_() -> String
        = _ s:variable() _ { s }

    rule string() -> String =
        "\"" s:$([^'"']*) "\"" { s.to_string() }

    rule character() -> u8 =
        "'" s:([^'\'']) "'" { s as u8 }
});


pub fn test_sexpr_parser() {
    let prog = "
( declare_fun Foo (Int Int Int) )
( define_fun Bar ((x Int) (y Int) (z Int)) (
    (and x y (z) \"foo\")
))
        ";

    let result = parser::decl(prog);

    match result {
        Ok(decls) => {
            for decl in decls.iter() {
                println!("\n{decl}\n");
            }
        }
        Err(x) => println!("{:?}", x),
    }
}
