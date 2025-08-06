use crate::interference::InterferenceGraph;
// use crate::liveness::Liveness;
use crate::ast::*;
use crate::ssa::*;
use slotmap::*;

new_key_type!{
    pub struct Color;
}

pub struct Coloring {
    pub var2color: SparseSecondaryMap<Var, Color>,
}

pub struct RegisterAllocator {
    pub matrix: InterferenceGraph,
}

pub trait Arch {
    type Opcode: Instruction;

    /// List all the callee saved registers
    fn callee_save() -> Vec<usize>;

    /// List all the callee saved registers
    fn caller_save() -> Vec<usize>;

    /// List of the function parameters (must be caller saved)
    fn params() -> Vec<usize>;

    /// Result register (must be callee saved)
    fn result() -> usize;

    /// Declare a function using it's name, arguments, stack variables
    fn declare_function(cfg: &Cfg<Instr>, ) -> Vec<Self::Opcode>;

    //fn entry(cfg: &Cfg<Instr>, coloring: )
    //fn encode_block(stmt: Box<Instr>, coloring: ) -> Vec<Self::Opcode>;
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Reg {
    /// A physical register represented as an `usize`
    Phys(usize),

    /// A virutal register represented using a `Var`
    Virt(Var),
}

impl std::fmt::Display for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Virt(v) => write!(f, "{v}"),
            Self::Phys(v) => write!(f, "p{v}"),
        }
    }
}

impl Reg {
    pub fn to_virt(&self) -> Option<Var> {
        match self {
            Self::Virt(v) => Some(*v),
            Self::Phys(_) => None,
        }
    }

    pub fn to_phys(&self) -> Option<usize> {
        match self {
            Self::Virt(_) => None,
            Self::Phys(p) => Some(*p),
        }
    }

    pub fn to_virt_mut(&mut self) -> Option<&mut Var> {
        match self {
            Self::Virt(v) => Some(v),
            Self::Phys(_) => None,
        }
    }

    pub fn to_phys_mut(&mut self) -> Option<&mut usize> {
        match self {
            Self::Virt(_) => None,
            Self::Phys(p) => Some(p),
        }
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum RvInstr {
    Add(Reg, Reg, Reg),
    Addi(Reg, Reg, i16),
    Xori(Reg, Reg, i16),
    Sltiu(Reg, Reg, i16),
    Sub(Reg, Reg, Reg),
    Slt(Reg, Reg, Reg),
    Sltu(Reg, Reg, Reg),
    Sll(Reg, Reg, Reg),
    Srl(Reg, Reg, Reg),
    Sra(Reg, Reg, Reg),
    And(Reg, Reg, Reg),
    Or(Reg, Reg, Reg),
    Xor(Reg, Reg, Reg),
    Beq(Reg, Reg, Label, Label),
    Bne(Reg, Reg, Label, Label),
    Blt(Reg, Reg, Label, Label),
    Bltu(Reg, Reg, Label, Label),
    Bge(Reg, Reg, Label, Label),
    Bgeu(Reg, Reg, Label, Label),
    Jump(Label),
    La(Reg, String),
    Li(Reg, i32),
    RvCall(String),
    GenericCall(Reg, String, Vec<Reg>),
    Return(Reg),
    Load(Reg, Reg, i16),
    Store(Reg, Reg, i16),
}

impl RvInstr {
    pub fn neg(dst: Reg, src: Reg) -> Self {
        Self::Sub(dst, Reg::Phys(0), src)
    }

    pub fn not(dst: Reg, src: Reg) -> Self {
        Self::Xori(dst, src, -1)
    }

    pub fn from_binop(dst: Var, binop: Binop, v1: Var, v2: Var) -> Vec<Self> {
        match binop {
            Binop::Add => vec![Self::Add(Reg::Virt(dst), Reg::Virt(v1), Reg::Virt(v2))],
            Binop::Sub => vec![Self::Sub(Reg::Virt(dst), Reg::Virt(v1), Reg::Virt(v2))],
            Binop::And => vec![Self::And(Reg::Virt(dst), Reg::Virt(v1), Reg::Virt(v2))],
            Binop::Or => vec![Self::Or(Reg::Virt(dst), Reg::Virt(v1), Reg::Virt(v2))],
            Binop::Xor => vec![Self::Xor(Reg::Virt(dst), Reg::Virt(v1), Reg::Virt(v2))],
            Binop::Sll => vec![Self::Sll(Reg::Virt(dst), Reg::Virt(v1), Reg::Virt(v2))],
            Binop::Sra => vec![Self::Sra(Reg::Virt(dst), Reg::Virt(v1), Reg::Virt(v2))],
            Binop::Srl => vec![Self::Srl(Reg::Virt(dst), Reg::Virt(v1), Reg::Virt(v2))],
            Binop::LessThan => vec![Self::Slt(Reg::Virt(dst), Reg::Virt(v1), Reg::Virt(v2))],
            Binop::ULessThan => vec![Self::Sltu(Reg::Virt(dst), Reg::Virt(v1), Reg::Virt(v2))],

            Binop::Equal =>
                vec![
                    Self::Sub(Reg::Virt(dst), Reg::Virt(v1), Reg::Virt(v2)),
                    Self::Sltiu(Reg::Virt(dst), Reg::Virt(dst), 1)
                ],
            Binop::NotEqual =>
                vec![
                    Self::Sub(Reg::Virt(dst), Reg::Virt(v1), Reg::Virt(v2)),
                    Self::Sltu(Reg::Virt(dst), Reg::Phys(0), Reg::Virt(dst))
                ],
            Binop::LessEqual =>
                vec![
                    Self::Slt(Reg::Virt(dst), Reg::Virt(v2), Reg::Virt(v1)),
                    Self::Xori(Reg::Virt(dst), Reg::Virt(dst), -1)
                ],
            Binop::ULessEqual =>
                vec![
                    Self::Sltu(Reg::Virt(dst), Reg::Virt(v2), Reg::Virt(v1)),
                    Self::Xori(Reg::Virt(dst), Reg::Virt(dst), -1)
                ],
        }
    }
}

impl std::fmt::Display for RvInstr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            _ => Ok(())
        }
    }
}

impl RvInstr {
    pub fn dependencies(&self) -> Vec<Reg> {
        match self {
            Self::Add(_, x, y)
                | Self::Sub(_, x, y)
                | Self::Slt(_, x, y)
                | Self::Sltu(_, x, y)
                | Self::Srl(_, x, y)
                | Self::Sll(_, x, y)
                | Self::Sra(_, x, y)
                | Self::And(_, x, y)
                | Self::Or(_, x, y)
                | Self::Xor(_, x, y)
                => vec![*x, *y],
            Self::Beq(x, y, _, _)
                | Self::Bne(x, y, _, _)
                | Self::Blt(x, y, _, _)
                | Self::Bltu(x, y, _, _)
                | Self::Bge(x, y, _, _)
                | Self::Bgeu(x, y, _, _)
                => vec![*x, *y],
            Self::RvCall(_) => vec![],
            Self::GenericCall(_, _, args) =>
                args.iter().cloned().collect(),
            Self::Addi(_, x, _)
                | Self::Xori(_, x, _)
                | Self::Sltiu(_, x, _)
                => vec![*x],
            Self::Jump(_) => vec![],
            Self::Li(_, _) => vec![],
            Self::La(_, _) => vec![],
            Self::Return(x) => vec![*x],
            Self::Load(_, x, _) => vec![*x],
            Self::Store(x, y, _) => vec![*x, *y],
        }
    }

    pub fn dependencies_mut(&mut self) -> Vec<&mut Reg> {
        match self {
            Self::Add(_, x, y)
                | Self::Sub(_, x, y)
                | Self::Slt(_, x, y)
                | Self::Sltu(_, x, y)
                | Self::Srl(_, x, y)
                | Self::Sll(_, x, y)
                | Self::Sra(_, x, y)
                | Self::And(_, x, y)
                | Self::Or(_, x, y)
                | Self::Xor(_, x, y)
                => vec![x, y],
            Self::Beq(x, y, _, _)
                | Self::Bne(x, y, _, _)
                | Self::Blt(x, y, _, _)
                | Self::Bltu(x, y, _, _)
                | Self::Bge(x, y, _, _)
                | Self::Bgeu(x, y, _, _)
                => vec![x, y],
            Self::RvCall(_) => vec![],
            Self::GenericCall(_, _, args) =>
                args.iter_mut().collect(),
            Self::Addi(_, x, _)
                | Self::Xori(_, x, _)
                | Self::Sltiu(_, x, _)
                => vec![x],
            Self::Jump(_) => vec![],
            Self::Li(_, _) => vec![],
            Self::La(_, _) => vec![],
            Self::Return(x) => vec![x],
            Self::Load(_, x, _) => vec![x],
            Self::Store(x, y, _) => vec![x, y],
        }
    }

    pub fn target(&self) -> Option<Reg> {
        match self {
            Self::Add(x, _, _)
                | Self::Sub(x, _, _)
                | Self::Slt(x, _, _)
                | Self::Sltu(x, _, _)
                | Self::Srl(x, _, _)
                | Self::Sll(x, _, _)
                | Self::Sra(x, _, _)
                | Self::And(x, _, _)
                | Self::Or(x, _, _)
                | Self::Xor(x, _, _)
                => Some(*x),
            Self::Beq(_, _, _, _)
                | Self::Bne(_, _, _, _)
                | Self::Blt(_, _, _, _)
                | Self::Bltu(_, _, _, _)
                | Self::Bge(_, _, _, _)
                | Self::Bgeu(_, _, _, _)
                => None,
            Self::RvCall(_) => None,
            Self::GenericCall(x, _, _) => Some(*x),
            Self::Addi(x, _, _)
                | Self::Xori(x, _, _)
                | Self::Sltiu(x, _, _)
                => Some(*x),
            Self::Jump(_) => None,
            Self::Li(x, _) => Some(*x),
            Self::La(x, _) => Some(*x),
            Self::Return(_) => None,
            Self::Load(x, _, _) => Some(*x),
            Self::Store(_, _, _) => None,
        }
    }

    pub fn target_mut(&mut self) -> Option<&mut Reg> {
        match self {
            Self::Add(x, _, _)
                | Self::Sub(x, _, _)
                | Self::Slt(x, _, _)
                | Self::Sltu(x, _, _)
                | Self::Srl(x, _, _)
                | Self::Sll(x, _, _)
                | Self::Sra(x, _, _)
                | Self::And(x, _, _)
                | Self::Or(x, _, _)
                | Self::Xor(x, _, _)
                => Some(x),
            Self::Beq(_, _, _, _)
                | Self::Bne(_, _, _, _)
                | Self::Blt(_, _, _, _)
                | Self::Bltu(_, _, _, _)
                | Self::Bge(_, _, _, _)
                | Self::Bgeu(_, _, _, _)
                => None,
            Self::RvCall(_) => None,
            Self::GenericCall(x, _, _) => Some(x),
            Self::Addi(x, _, _)
                | Self::Xori(x, _, _)
                | Self::Sltiu(x, _, _)
                => Some(x),
            Self::Jump(_) => None,
            Self::Li(x, _) => Some(x),
            Self::La(x, _) => Some(x),
            Self::Return(_) => None,
            Self::Load(x, _, _) => Some(x),
            Self::Store(_, _, _) => None,
        }
    }
}

impl Instruction for RvInstr {
    fn operands(&self) -> Vec<Var> {
        self.dependencies().into_iter().filter_map(|x| x.to_virt()).collect()
    }

    fn operands_mut(&mut self) -> Vec<&mut Var> {
        self.dependencies_mut().into_iter().filter_map(|x| x.to_virt_mut()).collect()
    }

    fn destination(&self) -> Option<Var> {
        match self.target() {
            Some(Reg::Virt(v)) => Some(v),
            _ => None
        }
    }

    fn destination_mut(&mut self) -> Option<&mut Var> {
        match self.target_mut() {
            Some(Reg::Virt(v)) => Some(v),
            _ => None
        }
    }

    fn labels_mut(&mut self) -> Vec<&mut Label> {
        match self {
            Self::Add(_, _, _)
                | Self::Sub(_, _, _)
                | Self::Slt(_, _, _)
                | Self::Sltu(_, _, _)
                | Self::Srl(_, _, _)
                | Self::Sll(_, _, _)
                | Self::Sra(_, _, _)
                | Self::And(_, _, _)
                | Self::Or(_, _, _)
                | Self::Xor(_, _, _)
                => vec![],
            Self::Beq(_, _, l1, l2)
                | Self::Bne(_, _, l1, l2)
                | Self::Blt(_, _, l1, l2)
                | Self::Bltu(_, _, l1, l2)
                | Self::Bge(_, _, l1, l2)
                | Self::Bgeu(_, _, l1, l2)
                => vec![l1, l2],
            Self::RvCall(_) => vec![],
            Self::GenericCall(_, _, _) => vec![],
            Self::Addi(_, _, _)
                | Self::Xori(_, _, _)
                | Self::Sltiu(_, _, _)
                => vec![],
            Self::Jump(l) => vec![l],
            Self::Li(_, _) => vec![],
            Self::La(_, _) => vec![],
            Self::Return(_) => vec![],
            Self::Load(_, _, _) => vec![],
            Self::Store(_, _, _) => vec![],
        }
    }

    fn labels(&self) -> Vec<Label> {
        match self {
            Self::Add(_, _, _)
                | Self::Sub(_, _, _)
                | Self::Slt(_, _, _)
                | Self::Sltu(_, _, _)
                | Self::Srl(_, _, _)
                | Self::Sll(_, _, _)
                | Self::Sra(_, _, _)
                | Self::And(_, _, _)
                | Self::Or(_, _, _)
                | Self::Xor(_, _, _)
                => vec![],
            Self::Beq(_, _, l1, l2)
                | Self::Bne(_, _, l1, l2)
                | Self::Blt(_, _, l1, l2)
                | Self::Bltu(_, _, l1, l2)
                | Self::Bge(_, _, l1, l2)
                | Self::Bgeu(_, _, l1, l2)
                => vec![*l1, *l2],
            Self::RvCall(_) => vec![],
            Self::GenericCall(_, _, _) => vec![],
            Self::Addi(_, _, _)
                | Self::Xori(_, _, _)
                | Self::Sltiu(_, _, _)
                => vec![],
            Self::Jump(l) => vec![*l],
            Self::Li(_, _) => vec![],
            Self::La(_, _) => vec![],
            Self::Return(_) => vec![],
            Self::Load(_, _, _) => vec![],
            Self::Store(_, _, _) => vec![],
        }
    }

    fn exit_block(&self) -> bool {
        match self {
            Self::Add(_, _, _)
                | Self::Sub(_, _, _)
                | Self::Slt(_, _, _)
                | Self::Sltu(_, _, _)
                | Self::Srl(_, _, _)
                | Self::Sll(_, _, _)
                | Self::Sra(_, _, _)
                | Self::And(_, _, _)
                | Self::Or(_, _, _)
                | Self::Xor(_, _, _)
                => false,
            Self::Beq(_, _, _, _)
                | Self::Bne(_, _, _, _)
                | Self::Blt(_, _, _, _)
                | Self::Bltu(_, _, _, _)
                | Self::Bge(_, _, _, _)
                | Self::Bgeu(_, _, _, _)
                => true,
            Self::RvCall(_) => false,
            Self::GenericCall(_, _, _) => false,
            Self::Addi(_, _, _)
                | Self::Xori(_, _, _)
                | Self::Sltiu(_, _, _)
                => false,
            Self::Jump(_) => true,
            Self::Li(_, _) => false,
            Self::La(_, _) => false,
            Self::Return(_) => true,
            Self::Load(_, _, _) => false,
            Self::Store(_, _, _) => false,
        }
    }

    fn may_have_side_effect(&self) -> bool {
        match self {
            Self::Add(_, _, _)
                | Self::Sub(_, _, _)
                | Self::Slt(_, _, _)
                | Self::Sltu(_, _, _)
                | Self::Srl(_, _, _)
                | Self::Sll(_, _, _)
                | Self::Sra(_, _, _)
                | Self::And(_, _, _)
                | Self::Or(_, _, _)
                | Self::Xor(_, _, _)
                => false,
            Self::Beq(_, _, _, _)
                | Self::Bne(_, _, _, _)
                | Self::Blt(_, _, _, _)
                | Self::Bltu(_, _, _, _)
                | Self::Bge(_, _, _, _)
                | Self::Bgeu(_, _, _, _)
                => true,
            Self::RvCall(_) => true,
            Self::GenericCall(_, _, _) => true,
            Self::Addi(_, _, _)
                | Self::Xori(_, _, _)
                | Self::Sltiu(_, _, _)
                => false,
            Self::Jump(_) => true,
            Self::Li(_, _) => false,
            Self::La(_, _) => false,
            Self::Return(_) => true,
            Self::Load(_, _, _) => true,
            Self::Store(_, _, _) => true,
        }
    }
}


pub struct Translator {
    old: Cfg<Instr>,
    new: Cfg<RvInstr>,
    labels: SecondaryMap<Label, Label>,
    vars: SparseSecondaryMap<Var, Var>,
}

impl Translator {
    pub fn new(old: Cfg<Instr>) -> Self {
        let mut labels = SecondaryMap::new();
        let mut vars = SparseSecondaryMap::new();
        let mut new = Cfg::new(false, vec![]);

        for (b, _) in old.iter_blocks() {
            labels.insert(b, new.fresh_label());
        }

        for v in old.args.iter() {
            vars.insert(*v, new.fresh_arg());
        }

        for (v, kind) in old.iter_vars() {
            match kind {
                VarKind::Arg => {}
                VarKind::Stack => {
                    vars.insert(v, new.fresh_stack_var());
                }
                _ => {
                    vars.insert(v, new.fresh_var());
                }
            }
        }

        Self {
            labels,
            vars,
            old,
            new,
        }
    }

    pub fn load_lit(&mut self, stmt: &mut Vec<RvInstr>, lit: Lit) -> Var {
        match lit {
            Lit::Addr(s) => {
                let id = self.new.fresh_var();
                stmt.push(RvInstr::La(Reg::Virt(id), s));
                id
            }
            Lit::Int(i) => {
                let id = self.new.fresh_var();
                stmt.push(RvInstr::Li(Reg::Virt(id), i));
                id
            }
            Lit::Var(v) => self.vars[v]
        }
    }

    pub fn translate_block(&mut self, block: Label) {
        let mut stmt: Vec<RvInstr> = vec![];

        for instr in self.old[block].stmt.clone() {
            match instr {
                Instr::Binop(dst, binop, l1, l2) => {
                    let v1 = self.load_lit(&mut stmt, l1);
                    let v2 = self.load_lit(&mut stmt, l2);
                    stmt.extend(RvInstr::from_binop(dst, binop, v1, v2));
                }
                _ => {}
            }
        }

        self.new.set_block_stmt(self.labels[block], stmt);
    }
}
