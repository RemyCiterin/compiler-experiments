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
    env: HashMap<String, Var>,

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
        let mut cfg = Cfg::new(false, vec![]);
        let label = cfg.entry();

        let mut env = HashMap::new();
        for arg in args {
            env.insert(arg, cfg.fresh_arg());
        }

        Builder {
            cfg,
            label,
            globals,
            stmt: vec![],
            env: HashMap::new(),
            current_exit: None,
            current_header: None,
        }
    }

    /// Return the current control flow graph
    pub fn cfg(self) -> Cfg<Instr> {
        self.cfg
    }

    /// Return the address of a variable (local or global) as a literal
    pub fn lookup(&mut self, s: String, begin: LineCol, end: LineCol)
        -> Result<Lit, BuilderError> {
        if let Some(id) = self.env.get(&s) { return Ok(Lit::Var(*id)); }
        if self.globals.contains(&s) {
            return Ok(Lit::Addr(s));
        }

        return Err(BuilderError{
            message: format!("undefined variable `{s}`").to_string(),
            begin,
            end,
        });
    }

    /// Build an expression and return a variable representing the output value
    pub fn gen_expr(&mut self, expr: Expr) -> Result<Var, BuilderError> {
        match *expr.expr {
            ExprCore::Call(func, args) => {
                let mut ids: Vec<Lit> = vec![];
                for arg in args { ids.push(Lit::Var(self.gen_expr(arg)?)); }
                let id: Var = self.cfg.fresh_var();
                self.stmt.push(Instr::Call(id, func, ids));
                Ok(id)
            }
            ExprCore::Constant(i) => {
                let id: Var = self.cfg.fresh_var();
                self.stmt.push(Instr::Move(id, Lit::Int(i)));
                Ok(id)
            }
            ExprCore::Variable(s) => {
                let x = self.cfg.fresh_var();
                let addr = self.lookup(s, expr.begin, expr.end)?;
                self.stmt.push(
                    Instr::Load{dest: x, addr, volatile: false});
                Ok(x)
            }
            ExprCore::Binop(binop, lhs, rhs) => {
                let l: Var = self.gen_expr(lhs)?;
                let r: Var  = self.gen_expr(rhs)?;
                let id: Var = self.cfg.fresh_var();
                self.stmt.push(Instr::Binop(id, binop, Lit::Var(l), Lit::Var(r)));
                Ok(id)
            }
            ExprCore::Unop(unop,x) => {
                let y: Var = self.gen_expr(x)?;
                let id: Var = self.cfg.fresh_var();
                self.stmt.push(Instr::Unop(id, unop, Lit::Var(y)));
                Ok(id)
            }
            ExprCore::Deref(x) => {
                let y: Var = self.gen_expr(x)?;
                let id: Var = self.cfg.fresh_var();
                self.stmt.push(Instr::Load{dest: id, addr: Lit::Var(y), volatile: false});
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
        match *stmt.stmt {
            StmtCore::Decl(s) => {
                let id = self.cfg.fresh_stack_var();
                self.env.insert(s.clone(), id);

                Ok(())
            }
            StmtCore::Nop => Ok(()),
            StmtCore::Ite(cond, lhs, rhs) => {
                let id = self.gen_expr(cond)?;
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
            StmtCore::While(cond, stmt) => {
                let header = self.cfg.fresh_label();
                let body = self.cfg.fresh_label();
                let exit = self.cfg.fresh_label();

                self.gen_jump(header);

                self.label = header;
                let id = self.gen_expr(cond)?;
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
            StmtCore::Seq(lhs, rhs) => {
                self.gen_stmt(lhs)?;
                self.gen_stmt(rhs)
            }
            StmtCore::Return(e) => {
                let id = self.gen_expr(e)?;
                self.gen_return(id);
                Ok(())
            }
            StmtCore::Assign(s, e) => {
                let id = self.gen_expr(e)?;
                let addr = self.lookup(s, stmt.begin, stmt.end)?;

                self.stmt.push(
                    Instr::Store{addr, val: Lit::Var(id), volatile: false});
                Ok(())
            }
            StmtCore::Break => {
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
            StmtCore::Continue => {
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

    match *program.decl {
        DeclCore::Variable(v, ..) => add(&v)?,
        DeclCore::Seq(d1, d2) => {
            get_symbols(d1, symbols)?;
            get_symbols(d2, symbols)?;
        },
        DeclCore::Function(s, ..) => add(&s)?,
        DeclCore::Empty => {}
    }

    Ok(())
}

fn fill_table(program: Decl, symbols: &HashSet<String>, table: &mut SymbolTable<Instr>)
    -> Result<(), BuilderError> {

    match *program.decl {
        DeclCore::Variable(v, i) =>
            _ = table.symbols.insert(v, Section::Data(vec![Word::Int(i)])),
        DeclCore::Seq(d1, d2) => {
            fill_table(d1, symbols, table)?;
            fill_table(d2, symbols, table)?;
        },
        DeclCore::Function(s, args, body) => {
            let mut builder = Builder::new(args, symbols.clone());
            builder.gen_stmt(body)?;

            table.symbols.insert(s, Section::Text(builder.cfg()));
        },
        DeclCore::Empty => {}
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
