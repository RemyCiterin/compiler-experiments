use crate::ssa::*;
use crate::ast::*;
use std::collections::HashMap;

pub struct Builder {
    cfg: Cfg,
    stmt: Vec<Instr>,
    label: Label,
    map: HashMap<String, Var>,
}

impl Builder {
    pub fn new() -> Self {
        let cfg = Cfg::new(false);
        let label = cfg.entry();

        Builder {
            cfg,
            label,
            stmt: vec![],
            map: HashMap::new(),

        }
    }

    pub fn cfg(self) -> Cfg {
        self.cfg
    }

    pub fn gen_expr(&mut self, expr: Expr) -> Var {
        match expr {
            Expr::Constant(i) => {
                let id: Var = self.cfg.fresh_var();
                self.stmt.push(Instr::Li(id, i));
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
                self.stmt.push(Instr::Binop(id, binop, l, r));
                id
            }
            Expr::Unop(unop,x) => {
                let y: Var = self.gen_expr(*x);
                let id: Var = self.cfg.fresh_var();
                self.stmt.push(Instr::Unop(id, unop, y));
                id
            }
        }
    }

    pub fn gen_branch(&mut self, cond: Var, l1: Label, l2: Label) {
        self.stmt.push(Instr::Branch(cond, l1, l2));
        self.cfg.set_block_stmt(self.label, std::mem::take(&mut self.stmt));
    }

    pub fn gen_jump(&mut self, label: Label) {
        self.stmt.push(Instr::Jump(label));
        self.cfg.set_block_stmt(self.label, std::mem::take(&mut self.stmt));
    }

    pub fn gen_return(&mut self, id: Var) {
        self.stmt.push(Instr::Return(id));
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

                self.label = body;
                self.gen_stmt(*stmt);
                self.gen_jump(header);

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

                self.stmt.push(Instr::Move(self.map[&s], id));
            }
        }
    }
}
