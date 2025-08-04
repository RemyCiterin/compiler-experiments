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

impl Builder {
    pub fn new(args: Vec<String>) -> Self {
        let mut cfg = Cfg::new(false, vec![]);
        let label = cfg.entry();

        let mut env = HashMap::new();
        for arg in args {
            env.insert(arg, cfg.fresh_arg());
        }

        Builder {
            cfg,
            label,
            stmt: vec![],
            env: HashMap::new(),
            current_exit: None,
            current_header: None,
            globals: HashSet::new(),
        }
    }

    /// Return the current control flow graph
    pub fn cfg(self) -> Cfg<Instr> {
        self.cfg
    }

    /// Return the address of a variable (local or global) as a literal
    pub fn lookup(&mut self, s: String) -> Lit {
        if let Some(id) = self.env.get(&s) { return Lit::Var(*id); }
        if self.globals.contains(&s) {
            return Lit::Addr(s);
        }

        panic!("undefined variable {s}");
    }

    /// Build an expression and return a variable representing the output value
    pub fn gen_expr(&mut self, expr: Expr) -> Var {
        match *expr.expr {
            ExprCore::Call(func, args) => {
                let ids = args
                    .iter()
                    .map(|e| Lit::Var(self.gen_expr(e.clone()))).collect();
                let id: Var = self.cfg.fresh_var();
                self.stmt.push(Instr::Call(id, func, ids));
                id
            }
            ExprCore::Constant(i) => {
                let id: Var = self.cfg.fresh_var();
                self.stmt.push(Instr::Move(id, Lit::Int(i)));
                id
            }
            ExprCore::Variable(s) => {
                let x = self.cfg.fresh_var();
                let addr = self.lookup(s);
                self.stmt.push(
                    Instr::Load{dest: x, addr, volatile: false});
                x
            }
            ExprCore::Binop(binop, lhs, rhs) => {
                let l: Var = self.gen_expr(lhs);
                let r: Var  = self.gen_expr(rhs);
                let id: Var = self.cfg.fresh_var();
                self.stmt.push(Instr::Binop(id, binop, Lit::Var(l), Lit::Var(r)));
                id
            }
            ExprCore::Unop(unop,x) => {
                let y: Var = self.gen_expr(x);
                let id: Var = self.cfg.fresh_var();
                self.stmt.push(Instr::Unop(id, unop, Lit::Var(y)));
                id
            }
            ExprCore::Deref(x) => {
                let y: Var = self.gen_expr(x);
                let id: Var = self.cfg.fresh_var();
                self.stmt.push(Instr::Load{dest: id, addr: Lit::Var(y), volatile: false});
                id
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
    pub fn gen_stmt(&mut self, stmt: Stmt) {
        match stmt {
            Stmt::Decl(s) => {
                if !self.env.contains_key(&s) {
                    let id = self.cfg.fresh_stack_var();
                    self.env.insert(s.clone(), id);
                }
            }
            Stmt::Nop => {}
            Stmt::Ite(cond, lhs, rhs) => {
                let id = self.gen_expr(cond);
                let l1 = self.cfg.fresh_label();
                let l2 = self.cfg.fresh_label();
                let exit = self.cfg.fresh_label();
                self.gen_branch(id, l1, l2);


                let env = self.env.clone();
                self.label = l1;
                self.gen_stmt(*lhs);
                self.gen_jump(exit);

                self.env = env.clone();
                self.label = l2;
                self.gen_stmt(*rhs);
                self.gen_jump(exit);
                self.env = env;

                self.label = exit;
            }
            Stmt::While(cond, stmt) => {
                let header = self.cfg.fresh_label();
                let body = self.cfg.fresh_label();
                let exit = self.cfg.fresh_label();

                self.gen_jump(header);

                self.label = header;
                let id = self.gen_expr(cond);
                self.gen_branch(id, body, exit);

                let header_save =
                    std::mem::replace(&mut self.current_header, Some(header));
                let exit_save =
                    std::mem::replace(&mut self.current_exit, Some(exit));
                let env = self.env.clone();

                self.label = body;
                self.gen_stmt(*stmt);
                self.gen_jump(header);
                self.current_header = header_save;
                self.current_exit = exit_save;
                self.env = env;

                self.label = exit;
            }
            Stmt::Seq(lhs, rhs) => {
                self.gen_stmt(*lhs);
                self.gen_stmt(*rhs);
            }
            Stmt::Return(e) => {
                let id = self.gen_expr(e);
                self.gen_return(id);
            }
            Stmt::Assign(s, e) => {
                let id = self.gen_expr(e);
                let addr = self.lookup(s);

                self.stmt.push(
                    Instr::Store{addr, val: Lit::Var(id), volatile: false});
            }
            Stmt::Break => {
                if self.current_exit.is_none() {
                    panic!("`break` is illegal outside of a loop");
                }

                self.gen_jump(self.current_exit.unwrap());

                // Generate a fresh label to ensure everything after a `break` is unreachable
                self.label = self.cfg.fresh_label();
            }
            Stmt::Continue => {
                if self.current_header.is_none() {
                    panic!("`break` is illegal outside of a loop");
                }

                self.gen_jump(self.current_header.unwrap());

                // Generate a fresh label to ensure everything after a `continue` is unreachable
                self.label = self.cfg.fresh_label();
            }
        }
    }
}
