use crate::ssa::*;
use crate::ast::*;
use std::collections::HashMap;

pub struct Builder {
    cfg: Cfg<Instr>,
    stmt: Vec<Instr>,
    label: Label,
    map: HashMap<String, Var>,

    /// Exit label of the current loop (if it exists)
    current_exit: Option<Label>,

    /// Entry label of the current loop (if it exists)
    current_header: Option<Label>,
}

impl Builder {
    pub fn new() -> Self {
        let cfg = Cfg::new(false, vec![]);
        let label = cfg.entry();

        Builder {
            cfg,
            label,
            stmt: vec![],
            map: HashMap::new(),
            current_exit: None,
            current_header: None,

        }
    }

    pub fn cfg(self) -> Cfg<Instr> {
        self.cfg
    }

    pub fn gen_expr(&mut self, expr: Expr) -> Var {
        match expr {
            Expr::Constant(i) => {
                let id: Var = self.cfg.fresh_var();
                self.stmt.push(Instr::Move(id, Lit::Int(i)));
                id
            }
            Expr::Variable(s) => {
                if !self.map.contains_key(&s) {
                    let id: Var = self.cfg.fresh_var();
                    self.map.insert(s.clone(), id);
                }

                self.map[&s]
            }
            Expr::Binop(binop, lhs, rhs) => {
                let l: Var = self.gen_expr(*lhs);
                let r: Var  = self.gen_expr(*rhs);
                let id: Var = self.cfg.fresh_var();
                self.stmt.push(Instr::Binop(id, binop, Lit::Var(l), Lit::Var(r)));
                id
            }
            Expr::Unop(unop,x) => {
                let y: Var = self.gen_expr(*x);
                let id: Var = self.cfg.fresh_var();
                self.stmt.push(Instr::Unop(id, unop, Lit::Var(y)));
                id
            }
        }
    }

    pub fn gen_branch(&mut self, cond: Var, l1: Label, l2: Label) {
        self.stmt.push(Instr::Branch(Lit::Var(cond), l1, l2));
        self.cfg.set_block_stmt(self.label, std::mem::take(&mut self.stmt));
    }

    pub fn gen_jump(&mut self, label: Label) {
        self.stmt.push(Instr::Jump(label));
        self.cfg.set_block_stmt(self.label, std::mem::take(&mut self.stmt));
    }

    pub fn gen_return(&mut self, id: Var) {
        self.stmt.push(Instr::Return(Lit::Var(id)));
        self.cfg.set_block_stmt(self.label, std::mem::take(&mut self.stmt));
    }

    pub fn gen_stmt(&mut self, stmt: Stmt) {
        match stmt {
            Stmt::Nop => {}
            Stmt::Ite(cond, lhs, rhs) => {
                let id = self.gen_expr(cond);
                let l1 = self.cfg.fresh_label();
                let l2 = self.cfg.fresh_label();
                let exit = self.cfg.fresh_label();
                self.gen_branch(id, l1, l2);


                self.label = l1;
                self.gen_stmt(*lhs);
                self.gen_jump(exit);

                self.label = l2;
                self.gen_stmt(*rhs);
                self.gen_jump(exit);

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
                self.label = body;
                self.gen_stmt(*stmt);
                self.gen_jump(header);
                self.current_header = header_save;
                self.current_exit = exit_save;

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

                if !self.map.contains_key(&s) {
                    let id = self.cfg.fresh_var();
                    self.map.insert(s.clone(), id);
                }

                self.stmt.push(Instr::Move(self.map[&s], Lit::Var(id)));
            }
            Stmt::Break => {
                if self.current_exit.is_none() {
                    panic!("`break` is illegal outside of a loop");
                }

                self.gen_jump(self.current_exit.unwrap());
                self.label = self.current_exit.unwrap();
            }
            Stmt::Continue => {
                if self.current_header.is_none() {
                    panic!("`break` is illegal outside of a loop");
                }

                self.gen_jump(self.current_header.unwrap());
                self.label = self.current_header.unwrap();
            }
        }
    }
}

pub struct Builder2 {
    cfg: Cfg<Instr>,
    stmt: Vec<Instr>,
    label: Label,
    map: HashMap<String, Var>,

    /// Exit label of the current loop (if it exists)
    current_exit: Option<Label>,

    /// Entry label of the current loop (if it exists)
    current_header: Option<Label>,
}

impl Builder2 {
    pub fn new() -> Self {
        let cfg = Cfg::new(false, vec![]);
        let label = cfg.entry();

        Builder2 {
            cfg,
            label,
            stmt: vec![],
            map: HashMap::new(),
            current_exit: None,
            current_header: None,
        }
    }

    pub fn cfg(self) -> Cfg<Instr> {
        self.cfg
    }

    pub fn gen_expr(&mut self, expr: Expr) -> Var {
        match expr {
            Expr::Constant(i) => {
                let id: Var = self.cfg.fresh_var();
                self.stmt.push(Instr::Move(id, Lit::Int(i)));
                id
            }
            Expr::Variable(s) => {
                if !self.map.contains_key(&s) {
                    let id: Var = self.cfg.fresh_stack_var();
                    self.map.insert(s.clone(), id);
                }

                let x = self.cfg.fresh_var();
                self.stmt.push(
                    Instr::Load{dest: x, addr: Lit::Var(self.map[&s]), volatile: false});
                x
            }
            Expr::Binop(binop, lhs, rhs) => {
                let l: Var = self.gen_expr(*lhs);
                let r: Var  = self.gen_expr(*rhs);
                let id: Var = self.cfg.fresh_var();
                self.stmt.push(Instr::Binop(id, binop, Lit::Var(l), Lit::Var(r)));
                id
            }
            Expr::Unop(unop,x) => {
                let y: Var = self.gen_expr(*x);
                let id: Var = self.cfg.fresh_var();
                self.stmt.push(Instr::Unop(id, unop, Lit::Var(y)));
                id
            }
        }
    }

    pub fn gen_branch(&mut self, cond: Var, l1: Label, l2: Label) {
        self.stmt.push(Instr::Branch(Lit::Var(cond), l1, l2));
        self.cfg.set_block_stmt(self.label, std::mem::take(&mut self.stmt));
    }

    pub fn gen_jump(&mut self, label: Label) {
        self.stmt.push(Instr::Jump(label));
        self.cfg.set_block_stmt(self.label, std::mem::take(&mut self.stmt));
    }

    pub fn gen_return(&mut self, id: Var) {
        self.stmt.push(Instr::Return(Lit::Var(id)));
        self.cfg.set_block_stmt(self.label, std::mem::take(&mut self.stmt));
    }

    pub fn gen_stmt(&mut self, stmt: Stmt) {
        match stmt {
            Stmt::Nop => {}
            Stmt::Ite(cond, lhs, rhs) => {
                let id = self.gen_expr(cond);
                let l1 = self.cfg.fresh_label();
                let l2 = self.cfg.fresh_label();
                let exit = self.cfg.fresh_label();
                self.gen_branch(id, l1, l2);


                self.label = l1;
                self.gen_stmt(*lhs);
                self.gen_jump(exit);

                self.label = l2;
                self.gen_stmt(*rhs);
                self.gen_jump(exit);

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
                self.label = body;
                self.gen_stmt(*stmt);
                self.gen_jump(header);
                self.current_header = header_save;
                self.current_exit = exit_save;

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

                if !self.map.contains_key(&s) {
                    let id = self.cfg.fresh_stack_var();
                    self.map.insert(s.clone(), id);
                }

                self.stmt.push(
                    Instr::Store{addr: Lit::Var(self.map[&s]), val: Lit::Var(id), volatile: false});
            }
            Stmt::Break => {
                if self.current_exit.is_none() {
                    panic!("`break` is illegal outside of a loop");
                }

                self.gen_jump(self.current_exit.unwrap());
                self.label = self.current_exit.unwrap();
            }
            Stmt::Continue => {
                if self.current_header.is_none() {
                    panic!("`break` is illegal outside of a loop");
                }

                self.gen_jump(self.current_header.unwrap());
                self.label = self.current_header.unwrap();
            }
        }
    }
}
