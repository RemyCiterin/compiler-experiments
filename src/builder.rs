use crate::ssa::*;
use crate::ast::*;
use std::collections::{HashMap, HashSet};

pub struct Builder {
    /// Output control flow graph
    cfg: Cfg<Instr>,

    /// Instructions of the block we are currently building
    stmt: Vec<Instr>,

    /// Label of the block we are currently building
    label: Label,

    /// Environment of local variables
    env: HashMap<String, Lit>,

    /// Global variables as a set of strings that we can point to
    globals: HashSet<String>,

    /// Exit label of the current loop (if it exists)
    current_exit: Option<Label>,

    /// Entry label of the current loop (if it exists)
    current_header: Option<Label>,
}

pub struct BuilderError {
    message: String,
    begin: LineCol,
    end: LineCol,
}

pub fn show_builder_error(program: &str, err: BuilderError) {
    show_error(&err.message, program, err.begin, err.end)
}

impl Builder {
    pub fn new(args: Vec<String>, globals: HashSet<String>) -> Self {
        let mut cfg = Cfg::new(false);
        let mut stmt: Vec<Instr> = vec![];
        let label = cfg.entry();

        let mut env = HashMap::new();

        // We start by pushing arguments to the stack in case we dereference them
        for arg in args {
            let slot = cfg.fresh_stack_var(4);
            let slot = Lit::Stack( slot );
            let id: Var = cfg.fresh_arg();
            env.insert(arg, slot.clone());

            stmt.push(
                Instr::Store{volatile: false, val: Lit::Var(id), addr: slot.clone()});
        }

        Builder {
            cfg,
            env,
            stmt,
            label,
            globals,
            current_exit: None,
            current_header: None,
        }
    }

    /// Return the current control flow graph
    pub fn cfg(mut self) -> Cfg<Instr> {
        let id = self.cfg.fresh_var();
        self.stmt.push(Instr::Move(id, Lit::Int(0)));
        self.gen_return(id);
        self.cfg
    }

    /// Return the address of a variable (local or global) as a literal
    pub fn lookup(&mut self, s: String, begin: LineCol, end: LineCol)
        -> Result<Lit, BuilderError> {
        if let Some(id) = self.env.get(&s) { return Ok(id.clone()); }
        if self.globals.contains(&s) {
            return Ok(Lit::Addr(s));
        }

        return Err(BuilderError{
            message: format!("undefined variable `{s}`").to_string(),
            begin,
            end,
        });
    }

    pub fn gen_lvalue(&mut self, lvalue: LValue) -> Result<Lit, BuilderError> {
        match *lvalue.core {
            LValueCore::Variable{name} => {
                Ok(self.lookup(name, lvalue.begin, lvalue.end)?)
            }
            LValueCore::Deref{rvalue} => {
                Ok(Lit::Var(self.gen_rvalue(rvalue)?))
            }
        }
    }

    /// Build an expression and return a variable representing the output value
    pub fn gen_rvalue(&mut self, expr: RValue) -> Result<Var, BuilderError> {
        match *expr.core {
            RValueCore::Call{name, args} => {
                let mut ids: Vec<Lit> = vec![];
                for arg in args { ids.push(Lit::Var(self.gen_rvalue(arg)?)); }
                let id: Var = self.cfg.fresh_var();
                self.stmt.push(Instr::Call(id, name, ids));
                Ok(id)
            }
            RValueCore::Constant{value} => {
                let id: Var = self.cfg.fresh_var();
                self.stmt.push(Instr::Move(id, Lit::Int(value)));
                Ok(id)
            }
            RValueCore::LValue{lvalue} => {
                let x = self.cfg.fresh_var();
                let addr = self.gen_lvalue(lvalue)?;
                self.stmt.push(
                    Instr::Load{dest: x, addr, volatile: false});
                Ok(x)
            }
            RValueCore::Binop{binop, lhs, rhs} => {
                let l: Var = self.gen_rvalue(lhs)?;
                let r: Var  = self.gen_rvalue(rhs)?;
                let id: Var = self.cfg.fresh_var();
                self.stmt.push(Instr::Binop(id, binop, Lit::Var(l), Lit::Var(r)));
                Ok(id)
            }
            RValueCore::Unop{unop,arg} => {
                let y: Var = self.gen_rvalue(arg)?;
                let id: Var = self.cfg.fresh_var();
                self.stmt.push(Instr::Unop(id, unop, Lit::Var(y)));
                Ok(id)
            }
            RValueCore::Ref{lvalue} => {
                let id = self.cfg.fresh_var();
                let lit = self.gen_lvalue(lvalue)?;
                self.stmt.push(Instr::Move(id, lit));
                Ok(id)
            }
            RValueCore::And{lhs, rhs} => {
                let l1: Label = self.cfg.fresh_label();
                let l2: Label = self.cfg.fresh_label();
                let id = self.cfg.fresh_var();

                let id1 = self.gen_rvalue(lhs)?;
                self.stmt.push(Instr::Move(id, Lit::Var(id1)));
                self.gen_branch(id1, l1, l2);

                self.label = l1;
                let id2 = self.gen_rvalue(rhs)?;
                self.stmt.push(Instr::Move(id, Lit::Var(id2)));
                self.gen_jump(l2);

                self.label = l2;
                Ok(id)
            }
            RValueCore::Or{lhs, rhs} => {
                let l1: Label = self.cfg.fresh_label();
                let l2: Label = self.cfg.fresh_label();
                let id = self.cfg.fresh_var();

                let id1 = self.gen_rvalue(lhs)?;
                self.stmt.push(Instr::Move(id, Lit::Var(id1)));
                self.gen_branch(id1, l1, l2);

                self.label = l2;
                let id2 = self.gen_rvalue(rhs)?;
                self.stmt.push(Instr::Move(id, Lit::Var(id2)));
                self.gen_jump(l1);

                self.label = l1;
                Ok(id)
            }
        }
    }

    /// End a block by generating a branch instruction
    pub fn gen_branch(&mut self, cond: Var, l1: Label, l2: Label) {
        self.stmt.push(Instr::Branch(Lit::Var(cond), l1, l2));
        self.cfg.set_block_stmt(self.label, std::mem::take(&mut self.stmt));
    }

    /// End a block by generating a jump instruction
    pub fn gen_jump(&mut self, label: Label) {
        self.stmt.push(Instr::Jump(label));
        self.cfg.set_block_stmt(self.label, std::mem::take(&mut self.stmt));
    }

    /// End a block by generating a return instruction
    pub fn gen_return(&mut self, id: Var) {
        self.stmt.push(Instr::Return(Lit::Var(id)));
        self.cfg.set_block_stmt(self.label, std::mem::take(&mut self.stmt));
    }

    /// Generate the instructions/block for a given statement
    pub fn gen_stmt(&mut self, stmt: Stmt) -> Result<(), BuilderError> {
        match *stmt.core {
            StmtCore::Decl{name: s} => {
                let slot = self.cfg.fresh_stack_var(4);

                self.env.insert(s.clone(), Lit::Stack(slot));

                Ok(())
            }
            StmtCore::Expr{rvalue} => {
                self.gen_rvalue(rvalue)?;
                Ok(())
            }
            StmtCore::Nop{} => Ok(()),
            StmtCore::It{cond, body} => {
                let id = self.gen_rvalue(cond)?;
                let l1 = self.cfg.fresh_label();
                let exit = self.cfg.fresh_label();
                self.gen_branch(id, l1, exit);


                let env = self.env.clone();
                self.label = l1;
                self.gen_stmt(body)?;
                self.gen_jump(exit);
                self.label = exit;
                self.env = env;

                Ok(())
            }
            StmtCore::Ite{cond, lhs, rhs} => {
                let id = self.gen_rvalue(cond)?;
                let l1 = self.cfg.fresh_label();
                let l2 = self.cfg.fresh_label();
                let exit = self.cfg.fresh_label();
                self.gen_branch(id, l1, l2);


                let env = self.env.clone();
                self.label = l1;
                self.gen_stmt(lhs)?;
                self.gen_jump(exit);

                self.env = env.clone();
                self.label = l2;
                self.gen_stmt(rhs)?;
                self.gen_jump(exit);
                self.env = env;

                self.label = exit;

                Ok(())
            }
            StmtCore::While{cond, body: stmt} => {
                let header = self.cfg.fresh_label();
                let body = self.cfg.fresh_label();
                let exit = self.cfg.fresh_label();

                self.gen_jump(header);

                self.label = header;
                let id = self.gen_rvalue(cond)?;
                self.gen_branch(id, body, exit);

                let header_save =
                    std::mem::replace(&mut self.current_header, Some(header));
                let exit_save =
                    std::mem::replace(&mut self.current_exit, Some(exit));
                let env = self.env.clone();

                self.label = body;
                self.gen_stmt(stmt)?;
                self.gen_jump(header);
                self.current_header = header_save;
                self.current_exit = exit_save;
                self.env = env;

                self.label = exit;
                Ok(())
            }
            StmtCore::Seq{lhs, rhs} => {
                self.gen_stmt(lhs)?;
                self.gen_stmt(rhs)
            }
            StmtCore::Return{expr} => {
                let id = self.gen_rvalue(expr)?;
                self.gen_return(id);
                self.label = self.cfg.fresh_label();
                Ok(())
            }
            StmtCore::Assign{lvalue, rvalue} => {
                let id = self.gen_rvalue(rvalue)?;
                let addr = self.gen_lvalue(lvalue)?;

                self.stmt.push(
                    Instr::Store{addr, val: Lit::Var(id), volatile: false});
                Ok(())
            }
            StmtCore::Break{} => {
                if self.current_exit.is_none() {
                    return Err(BuilderError{
                        message: "`break` is illegal outside of a loop".to_string(),
                        begin: stmt.begin,
                        end: stmt.end,
                    });
                }

                self.gen_jump(self.current_exit.unwrap());

                // Generate a fresh label to ensure everything after a `break` is unreachable
                self.label = self.cfg.fresh_label();
                Ok(())
            }
            StmtCore::Continue{} => {
                if self.current_header.is_none() {
                    return Err(BuilderError{
                        message: "`continue` is illegal outside of a loop".to_string(),
                        begin: stmt.begin,
                        end: stmt.end,
                    });
                }

                self.gen_jump(self.current_header.unwrap());

                // Generate a fresh label to ensure everything after a `continue` is unreachable
                self.label = self.cfg.fresh_label();
                Ok(())
            }
        }
    }
}


fn get_symbols(program: Decl, symbols: &mut HashSet<String>) -> Result<(), BuilderError> {

    let mut add = |s: &str| {
        if symbols.contains(s) {
            Err(BuilderError{
                message: format!("symbol `{s}` is already defined"),
                begin: program.begin,
                end: program.end,
            })
        } else {
            symbols.insert(s.to_string());
            Ok(())
        }
    };

    match *program.core {
        DeclCore::Array{name, ..} => add(&name)?,
        DeclCore::Variable{name, ..} => add(&name)?,
        DeclCore::Seq{lhs, rhs} => {
            get_symbols(lhs, symbols)?;
            get_symbols(rhs, symbols)?;
        },
        DeclCore::Function{name, ..} => add(&name)?,
        DeclCore::Empty{} => {}
    }

    Ok(())
}

fn fill_table(program: Decl, symbols: &HashSet<String>, table: &mut SymbolTable<Instr>)
    -> Result<(), BuilderError> {

    match *program.core {
        DeclCore::Variable{name, value} =>
            _ = table.symbols.insert(name, Section::Data(vec![Word::Int(value)])),
        DeclCore::Array{name, values} => {
            let mut data: Vec<Word> = values.iter().map(|x| Word::Int(*x)).collect();
            data.insert(0, Word::Addr(name.clone(), 4));
            _ = table.symbols.insert(name, Section::Data(data))
        },
        DeclCore::Seq{lhs, rhs} => {
            fill_table(lhs, symbols, table)?;
            fill_table(rhs, symbols, table)?;
        },
        DeclCore::Function{name, args, body} => {
            let mut builder = Builder::new(args, symbols.clone());
            builder.gen_stmt(body)?;

            table.symbols.insert(name, Section::Text(builder.cfg()));
        },
        DeclCore::Empty{} => {}
    }

    Ok(())
}

pub fn build(program: Decl) -> Result<SymbolTable<Instr>, BuilderError> {
    let mut table = SymbolTable{ symbols: HashMap::new() };

    let mut symbols = HashSet::new();

    get_symbols(program.clone(), &mut symbols)?;

    fill_table(program, &symbols, &mut table)?;

    Ok(table)
}
