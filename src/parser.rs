use crate::ast::*;

peg::parser!(pub grammar customlang() for str {
    rule location() -> peg::str::LineCol =
        #{|input, pos| peg::RuleResult::Matched(pos, peg::Parse::position_repr(input, pos))}

    pub rule rvalue() -> RValue = precedence!{
        x:(@) _ "&&" _ y:@ {
            let begin = x.begin;
            let end = y.end;
            let zero = RValue::constant(0, begin, end);
            let lhs = RValue::binop(Binop::NotEqual, x, zero.clone(), begin, end);
            let rhs = RValue::binop(Binop::NotEqual, y, zero, begin, end);
            RValue::binop(Binop::And, lhs, rhs, begin, end)
        }
        x:(@) _ "||" _ y:@ {
            let begin = x.begin;
            let end = y.end;
            let zero = RValue::constant(0, begin, end);
            let lhs = RValue::binop(Binop::NotEqual, x, zero.clone(), begin, end);
            let rhs = RValue::binop(Binop::NotEqual, y, zero, begin, end);
            RValue::binop(Binop::Or, lhs, rhs, begin, end)
        }
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
        x:(@) _ "<" _ y:@ {
            let begin = x.begin;
            let end = y.end;
            RValue::binop(Binop::LessThan, x, y, begin, end)
        }
        x:(@) _ ">" _ y:@ {
            let begin = x.begin;
            let end = y.end;
            RValue::binop(Binop::LessThan, y, x, begin, end)
        }
        x:(@) _ "<=" _ y:@ {
            let begin = x.begin;
            let end = y.end;
            RValue::binop(Binop::LessEqual, x, y, begin, end)
        }
        x:(@) _ ">=" _ y:@ {
            let begin = x.begin;
            let end = y.end;
            RValue::binop(Binop::LessEqual, y, x, begin, end)
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
        x:(@) _ ">>" _ y:@ {
            let begin = x.begin;
            let end = y.end;
            RValue::binop(Binop::Sra, x, y, begin, end)
        }
        begin:location() "-" _ x:@
            { let end = x.end; RValue::unop(Unop::Neg, x, begin, end) }
        begin:location() "~" _ x:@
            { let end= x.end; RValue::unop(Unop::Not, x, begin, end) }
        begin:location() "!" _ x:@ {
            let end= x.end;
            let zero = RValue::constant(0, begin, end);
            RValue::binop(Binop::Equal, x, zero, begin, end)
        }
        begin:location() "@srl" _ "(" _ x:rvalue() _ "," _ y:rvalue() _ ")" end:location()
            { RValue::binop(Binop::Srl, x, y, begin, end) }
        begin:location() "@ult" _ "(" _ x:rvalue() _ "," _ y:rvalue() _ ")" end:location()
            { RValue::binop(Binop::ULessThan, x, y, begin, end) }
        begin:location() "@ule" _ "(" _ x:rvalue() _ "," _ y:rvalue() _ ")" end:location()
            { RValue::binop(Binop::ULessEqual, x, y, begin, end) }
        begin:location() s:variable() _ "(" args:(_rvalue_() ** ",") ")" end:location()
            { RValue::call(s, args, begin, end) }
        begin:location() "&" _ lvalue:lvalue() end:location() {
            RValue::reference(lvalue, begin, end)
        }
        begin:location() lvalue:lvalue() end:location() {
            RValue::lvalue(lvalue, begin, end)
        }
        begin:location() n:number() end:location()
            { RValue::constant(n, begin, end) }
        begin:location() n:character() end:location()
            { RValue::constant(n as i32, begin, end) }
        "(" _ e:rvalue() _ ")" { e }
    }

    rule _rvalue_() -> RValue =
        _ r:rvalue() _ {r}

    pub rule lvalue() -> LValue = precedence!{
        begin:location() "*" _ rvalue:rvalue() end:location()
            { LValue::defer(rvalue, begin, end) }
        lvalue:@ _ "[" _ rvalue:rvalue() _ "]" end:location() {
            let begin = lvalue.begin;
            let two = RValue::constant(2, begin, end);
            let reference = RValue::lvalue(lvalue, begin, end);
            let offset = RValue::binop(Binop::Sll, rvalue, two, begin ,end);
            let addr = RValue::binop(Binop::Add, reference, offset, begin, end);
            LValue::defer(addr, begin, end)
        }
        "(" _ lvalue:lvalue() _ ")"
            { lvalue }
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
        begin:location() rvalue:rvalue() _ ";" end:location()
            { Stmt::expr(rvalue, begin, end) }
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
        begin:location() "var" _ s:variable() _ "=" _ n:character() _ ";" end:location()
            { Decl::variable(s, n as i32, begin, end) }
        begin:location() "var" _ s:variable() _ "[" _ n1:number() _ "]" _
            "=" _ n2:number() _ ";" end:location()
            { Decl::array(s, std::iter::repeat(n2).take(n1 as usize).collect(), begin, end) }
        begin:location() "var" _ s:variable() _ "=" _ mem:string() _ ";" end:location() {
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

    rule variable() -> String
        = s:$(['a'..='z' | 'A'..='Z' | '_']['a'..='z' | 'A'..='Z' | '0'..='9' | '_']*) { s.into() }

    rule builtin() -> String
        = s:$(['@']['a'..='z' | 'A'..='Z' | '_']['a'..='z' | 'A'..='Z' | '0'..='9' | '_']*)
            { s.into() }

    rule _variable_() -> String
        = _ s:variable() _ { s }

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
