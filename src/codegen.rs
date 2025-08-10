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
pub enum RvBinop {
    Add, Sub, Slt, Sltu, Sll, Srl, Sra, And, Or, Xor
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum RvUnop {
    Add, Slt, Sltu, Sll, Srl, Sra, And, Or, Xor
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum RvBranchKind {
    Eq, Ne, Lt, Ltu, Ge, Geu
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum RvInstr {
    Unop(Reg, RvUnop, Reg, i16),
    Binop(Reg, RvBinop, Reg, Reg),
    Branch(RvBranchKind, Reg, Reg, Label, Label),
    Jump(Label),
    La(Reg, String),
    Li(Reg, i32),
    Ls(Reg, Slot),
    RvCall(String),
    GenericCall(Reg, String, Vec<Reg>),
    Phi(Reg, Vec<(Reg, Label)>),
    Return(Reg),
    Load(Reg, Reg, i16),
    Store(Reg, Reg, i16),
    Ret,
}


pub fn check_riscv_immediate(imm: i16) -> bool {
    imm >= -2048 && imm <= 2047
}

impl RvInstr {
    /// Bitwise negation
    pub fn neg(dst: Reg, src: Reg) -> Self {
        Self::sub(dst, Reg::Phys(0), src)
    }

    pub fn ne_zero(dst: Reg, src: Reg) -> Self {
        Self::sltu(dst, Reg::Phys(0), src)
    }

    pub fn eq_zero(dst: Reg, src: Reg) -> Self {
        Self::sltiu(dst, src, 1)
    }

    pub fn not(dst: Reg, src: Reg) -> Self {
        Self::xori(dst, src, -1)
    }

    pub fn add(dst: Reg, src1: Reg, src2: Reg) -> Self {
        Self::Binop(dst, RvBinop::Add, src1, src2)
    }

    pub fn and(dst: Reg, src1: Reg, src2: Reg) -> Self {
        Self::Binop(dst, RvBinop::And, src1, src2)
    }

    pub fn sub(dst: Reg, src1: Reg, src2: Reg) -> Self {
        Self::Binop(dst, RvBinop::Sub, src1, src2)
    }

    pub fn or(dst: Reg, src1: Reg, src2: Reg) -> Self {
        Self::Binop(dst, RvBinop::Or, src1, src2)
    }

    pub fn xor(dst: Reg, src1: Reg, src2: Reg) -> Self {
        Self::Binop(dst, RvBinop::Xor, src1, src2)
    }

    pub fn sll(dst: Reg, src1: Reg, src2: Reg) -> Self {
        Self::Binop(dst, RvBinop::Sll, src1, src2)
    }

    pub fn sra(dst: Reg, src1: Reg, src2: Reg) -> Self {
        Self::Binop(dst, RvBinop::Sra, src1, src2)
    }

    pub fn srl(dst: Reg, src1: Reg, src2: Reg) -> Self {
        Self::Binop(dst, RvBinop::Srl, src1, src2)
    }

    pub fn slt(dst: Reg, src1: Reg, src2: Reg) -> Self {
        Self::Binop(dst, RvBinop::Slt, src1, src2)
    }

    pub fn sltu(dst: Reg, src1: Reg, src2: Reg) -> Self {
        Self::Binop(dst, RvBinop::Sltu, src1, src2)
    }

    pub fn addi(dst: Reg, src: Reg, imm: i16) -> Self {
        assert!(check_riscv_immediate(imm));
        Self::Unop(dst, RvUnop::Add, src, imm)
    }

    pub fn andi(dst: Reg, src: Reg, imm: i16) -> Self {
        assert!(check_riscv_immediate(imm));
        Self::Unop(dst, RvUnop::And, src, imm)
    }

    pub fn ori(dst: Reg, src: Reg, imm: i16) -> Self {
        assert!(check_riscv_immediate(imm));
        Self::Unop(dst, RvUnop::Or, src, imm)
    }

    pub fn xori(dst: Reg, src: Reg, imm: i16) -> Self {
        assert!(check_riscv_immediate(imm));
        Self::Unop(dst, RvUnop::Xor, src, imm)
    }

    pub fn slli(dst: Reg, src: Reg, imm: i16) -> Self {
        assert!(check_riscv_immediate(imm));
        Self::Unop(dst, RvUnop::Sll, src, imm)
    }

    pub fn srai(dst: Reg, src: Reg, imm: i16) -> Self {
        assert!(check_riscv_immediate(imm));
        Self::Unop(dst, RvUnop::Sra, src, imm)
    }

    pub fn srli(dst: Reg, src: Reg, imm: i16) -> Self {
        assert!(check_riscv_immediate(imm));
        Self::Unop(dst, RvUnop::Srl, src, imm)
    }

    pub fn slti(dst: Reg, src: Reg, imm: i16) -> Self {
        assert!(check_riscv_immediate(imm));
        Self::Unop(dst, RvUnop::Slt, src, imm)
    }

    pub fn sltiu(dst: Reg, src: Reg, imm: i16) -> Self {
        assert!(check_riscv_immediate(imm));
        Self::Unop(dst, RvUnop::Sltu, src, imm)
    }

    pub fn from_binop(dst: Var, binop: Binop, v1: Var, v2: Var) -> Vec<Self> {
        match binop {
            Binop::Add => vec![Self::add(Reg::Virt(dst), Reg::Virt(v1), Reg::Virt(v2))],
            Binop::Sub => vec![Self::sub(Reg::Virt(dst), Reg::Virt(v1), Reg::Virt(v2))],
            Binop::And => vec![Self::and(Reg::Virt(dst), Reg::Virt(v1), Reg::Virt(v2))],
            Binop::Or => vec![Self::or(Reg::Virt(dst), Reg::Virt(v1), Reg::Virt(v2))],
            Binop::Xor => vec![Self::xor(Reg::Virt(dst), Reg::Virt(v1), Reg::Virt(v2))],
            Binop::Sll => vec![Self::sll(Reg::Virt(dst), Reg::Virt(v1), Reg::Virt(v2))],
            Binop::Sra => vec![Self::sra(Reg::Virt(dst), Reg::Virt(v1), Reg::Virt(v2))],
            Binop::Srl => vec![Self::srl(Reg::Virt(dst), Reg::Virt(v1), Reg::Virt(v2))],
            Binop::LessThan => vec![Self::slt(Reg::Virt(dst), Reg::Virt(v1), Reg::Virt(v2))],
            Binop::ULessThan => vec![Self::sltu(Reg::Virt(dst), Reg::Virt(v1), Reg::Virt(v2))],

            Binop::Equal =>
                vec![
                    Self::sub(Reg::Virt(dst), Reg::Virt(v1), Reg::Virt(v2)),
                    Self::eq_zero(Reg::Virt(dst), Reg::Virt(dst))
                ],
            Binop::NotEqual =>
                vec![
                    Self::sub(Reg::Virt(dst), Reg::Virt(v1), Reg::Virt(v2)),
                    Self::ne_zero(Reg::Virt(dst), Reg::Virt(dst))
                ],
            Binop::LessEqual =>
                vec![
                    Self::slt(Reg::Virt(dst), Reg::Virt(v2), Reg::Virt(v1)),
                    Self::not(Reg::Virt(dst), Reg::Virt(dst))
                ],
            Binop::ULessEqual =>
                vec![
                    Self::sltu(Reg::Virt(dst), Reg::Virt(v2), Reg::Virt(v1)),
                    Self::not(Reg::Virt(dst), Reg::Virt(dst))
                ],
        }
    }

    pub fn from_unop(dst: Var, unop: Unop, v: Var) -> Vec<Self> {
        match unop {
            Unop::Not => vec![Self::not(Reg::Virt(dst), Reg::Virt(v))],
            Unop::Neg => vec![Self::neg(Reg::Virt(dst), Reg::Virt(v))],
        }
    }
}

impl std::fmt::Display for RvBinop {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Add => write!(f, "add"),
            Self::And => write!(f, "and"),
            Self::Sub => write!(f, "sub"),
            Self::Or => write!(f, "or"),
            Self::Xor => write!(f, "xor"),
            Self::Sll => write!(f, "sll"),
            Self::Sra => write!(f, "sra"),
            Self::Srl => write!(f, "srl"),
            Self::Slt => write!(f, "stl"),
            Self::Sltu => write!(f, "sltu"),
        }
    }
}

impl std::fmt::Display for RvUnop {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Add => write!(f, "addi"),
            Self::And => write!(f, "andi"),
            Self::Or => write!(f, "ori"),
            Self::Xor => write!(f, "xori"),
            Self::Sll => write!(f, "slli"),
            Self::Sra => write!(f, "srai"),
            Self::Srl => write!(f, "srli"),
            Self::Slt => write!(f, "stli"),
            Self::Sltu => write!(f, "sltiu"),
        }
    }
}

impl std::fmt::Display for RvBranchKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Eq => write!(f, "beq"),
            Self::Ne => write!(f, "bne"),
            Self::Lt => write!(f, "blt"),
            Self::Ltu => write!(f, "bltu"),
            Self::Ge => write!(f, "bge"),
            Self::Geu => write!(f, "bgeu"),
        }
    }
}

impl std::fmt::Display for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Virt(v) => write!(f, "{v}"),
            Self::Phys(v) => write!(f, "x{v}"),
        }
    }
}

impl std::fmt::Display for RvInstr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        //write!(f, "{:?}", self)
        match self {
            Self::Binop(dest, binop, src1, src2) =>
                write!(f, "{binop} {dest}, {src1}, {src2}"),
            Self::Unop(dest, unop, src, imm) =>
                write!(f, "{unop} {dest}, {src}, {imm}"),
            Self::Branch(kind, x, y, l1, l2) =>
                write!(f, "{kind} {x}, {y}, {l1}; jump {l2}"),
            Self::Jump(label) =>
                write!(f, "jump {label}"),
            Self::Li(r, i) =>
                write!(f, "li {r}, {i}"),
            Self::La(r, s) =>
                write!(f, "la {r}, {s}"),
            Self::Ls(r, s) =>
                write!(f, "la {r}, {s}"),
            Self::RvCall(s) =>
                write!(f, "call {s}"),
            Self::GenericCall(r, s, args) => {
                write!(f, "call {r} {s} (")?;
                for (i, a) in args.iter().enumerate() {
                    write!(f, "{a}")?;
                    if i != args.len() - 1 { write!(f, ", ")?; }
                }
                write!(f, ")")
            }
            Self::Return(r) =>
                write!(f, "ret {r}"),
            Self::Phi(r ,args) => {
                write!(f, "phi {r}")?;
                for (r, l) in args.iter() {
                    write!(f, " ({r}, {l})")?;
                }
                Ok(())
            }
            Self::Ret =>
                write!(f, "ret"),
            Self::Load(dest, addr, imm) =>
                write!(f, "lw {dest}, {imm}({addr})"),
            Self::Store(addr, val, imm) =>
                write!(f, "sw {val}, {imm}({addr})"),
        }
    }
}

impl RvInstr {
    pub fn dependencies(&self) -> Vec<Reg> {
        match self {
            Self::Phi(_, args) =>
                args.iter().map(|(r, _)| *r).collect(),
            Self::Binop(_, _, x, y)
                => vec![*x, *y],
            Self::Branch(_, x, y, _, _)
                => vec![*x, *y],
            Self::RvCall(_) => vec![],
            Self::GenericCall(_, _, args) =>
                args.iter().cloned().collect(),
            Self::Unop(_, _, x, _)
                => vec![*x],
            Self::Jump(_) => vec![],
            Self::Li(_, _) => vec![],
            Self::La(_, _) => vec![],
            Self::Ls(_, _) => vec![],
            Self::Return(x) => vec![*x],
            Self::Load(_, x, _) => vec![*x],
            Self::Store(x, y, _) => vec![*x, *y],
            Self::Ret => vec![],
        }
    }

    pub fn dependencies_mut(&mut self) -> Vec<&mut Reg> {
        match self {
            Self::Phi(_, args) =>
                args.iter_mut().map(|(r, _)| r).collect(),
            Self::Binop(_, _, x, y)
                => vec![x, y],
            Self::Branch(_, x, y, _, _)
                => vec![x, y],
            Self::RvCall(_) => vec![],
            Self::GenericCall(_, _, args) =>
                args.iter_mut().collect(),
            Self::Unop(_, _, x, _)
                => vec![x],
            Self::Jump(_) => vec![],
            Self::Li(_, _) => vec![],
            Self::La(_, _) => vec![],
            Self::Ls(_, _) => vec![],
            Self::Return(x) => vec![x],
            Self::Load(_, x, _) => vec![x],
            Self::Store(x, y, _) => vec![x, y],
            Self::Ret => vec![],
        }
    }

    pub fn target(&self) -> Option<Reg> {
        match self {
            Self::Phi(x, ..) => Some(*x),
            Self::Binop(x, _, _, _)
                => Some(*x),
            Self::Branch(_, _, _, _, _)
                => None,
            Self::RvCall(_) => None,
            Self::GenericCall(x, _, _) => Some(*x),
            Self::Unop(x, _, _, _)
                => Some(*x),
            Self::Jump(_) => None,
            Self::Li(x, _) => Some(*x),
            Self::La(x, _) => Some(*x),
            Self::Ls(x, _) => Some(*x),
            Self::Return(_) => None,
            Self::Load(x, _, _) => Some(*x),
            Self::Store(_, _, _) => None,
            Self::Ret => None,
        }
    }

    pub fn target_mut(&mut self) -> Option<&mut Reg> {
        match self {
            Self::Phi(x, ..) => Some(x),
            Self::Binop(x, _, _, _)
                => Some(x),
            Self::Branch(_, _, _, _, _)
                => None,
            Self::RvCall(_) => None,
            Self::GenericCall(x, _, _) => Some(x),
            Self::Unop(x, _, _, _)
                => Some(x),
            Self::Jump(_) => None,
            Self::Li(x, _) => Some(x),
            Self::La(x, _) => Some(x),
            Self::Ls(x, _) => Some(x),
            Self::Return(_) => None,
            Self::Load(x, _, _) => Some(x),
            Self::Store(_, _, _) => None,
            Self::Ret => None,
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
            Self::Phi(..) => vec![],
            Self::Binop(_, _, _, _)
                => vec![],
            Self::Branch(_, _, _, l1, l2)
                => vec![l1, l2],
            Self::RvCall(_) => vec![],
            Self::GenericCall(_, _, _) => vec![],
            Self::Unop(_, _, _, _)
                => vec![],
            Self::Jump(l) => vec![l],
            Self::Li(_, _) => vec![],
            Self::La(_, _) => vec![],
            Self::Ls(_, _) => vec![],
            Self::Return(_) => vec![],
            Self::Load(_, _, _) => vec![],
            Self::Store(_, _, _) => vec![],
            Self::Ret => vec![],
        }
    }

    fn labels(&self) -> Vec<Label> {
        match self {
            Self::Phi(..) => vec![],
            Self::Binop(_, _, _, _)
                => vec![],
            Self::Branch(_, _, _, l1, l2)
                => vec![*l1, *l2],
            Self::RvCall(_) => vec![],
            Self::GenericCall(_, _, _) => vec![],
            Self::Unop(_, _, _, _)
                => vec![],
            Self::Jump(l) => vec![*l],
            Self::Li(_, _) => vec![],
            Self::La(_, _) => vec![],
            Self::Ls(_, _) => vec![],
            Self::Return(_) => vec![],
            Self::Load(_, _, _) => vec![],
            Self::Store(_, _, _) => vec![],
            Self::Ret => vec![],
        }
    }

    fn exit_block(&self) -> bool {
        match self {
            Self::Phi(..) => false,
            Self::Binop(_, _, _, _)
                => false,
            Self::Branch(_, _, _, _, _)
                => true,
            Self::RvCall(_) => false,
            Self::GenericCall(_, _, _) => false,
            Self::Unop(_, _, _, _)
                => false,
            Self::Jump(_) => true,
            Self::Li(_, _) => false,
            Self::La(_, _) => false,
            Self::Ls(_, _) => false,
            Self::Return(_) => true,
            Self::Load(_, _, _) => false,
            Self::Store(_, _, _) => false,
            Self::Ret => true,
        }
    }

    fn may_have_side_effect(&self) -> bool {
        match self {
            Self::Phi(..) => false,
            Self::Binop(_, _, _, _)
                => false,
            Self::Branch(_, _, _, _, _)
                => true,
            Self::RvCall(_) => true,
            Self::GenericCall(_, _, _) => true,
            Self::Unop(_, _, _, _)
                => false,
            Self::Jump(_) => true,
            Self::Li(_, _) => false,
            Self::La(_, _) => false,
            Self::Ls(_, _) => false,
            Self::Return(_) => true,
            Self::Load(_, _, _) => true,
            Self::Store(_, _, _) => true,
            Self::Ret => true,
        }
    }
}

impl crate::out_of_ssa::HasMove for RvInstr {
    fn mv(dst: Var, src: Lit) -> Self {
        match src {
            Lit::Var(v) => RvInstr::Unop(Reg::Virt(dst), RvUnop::Add, Reg::Virt(v), 0),
            Lit::Addr(s) => RvInstr::La(Reg::Virt(dst), s),
            Lit::Int(i) => RvInstr::Li(Reg::Virt(dst), i),
            Lit::Stack(s) => RvInstr::Ls(Reg::Virt(dst), s),
        }
    }
}

impl crate::out_of_ssa::HasPhi for RvInstr {
    fn from_phi(dest: Var, args: Vec<(Var, Label)>) -> Self {
        Self::Phi(
            Reg::Virt(dest),
            args.into_iter().map(|(v, l)| (Reg::Virt(v), l)).collect())
    }

    fn to_phi(&self) -> Option<(Var, Vec<(Lit, Label)>)> {
        match self {
            Self::Phi(dest, args) => {
                Some((
                    dest.to_virt().unwrap(),
                    args.iter()
                    .map(|(v,l)| {
                        (Lit::Var(v.to_virt().unwrap()), *l)
                    }).collect()
                ))
            }
            _ => None,
        }
    }
}


pub struct Translator {
    new: Cfg<RvInstr>,
    labels: SecondaryMap<Label, Label>,
    vars: SparseSecondaryMap<Var, Var>,
    slots: SparseSecondaryMap<Slot, Slot>,
}

impl Translator {
    pub fn new(old: &Cfg<Instr>) -> Self {
        let mut labels = SecondaryMap::new();
        let mut slots = SparseSecondaryMap::new();
        let mut vars = SparseSecondaryMap::new();
        let mut new = Cfg::new(false, vec![]);

        for (b, _) in old.iter_blocks() {
            if b == old.entry() {
                labels.insert(b, new.entry());
                continue;
            }
            labels.insert(b, new.fresh_label());
        }

        for v in old.args.iter() {
            vars.insert(*v, new.fresh_arg());
        }

        for (slot, size) in old.stack.iter() {
            slots.insert(slot, new.fresh_stack_var(*size));
        }

        for (v, kind) in old.iter_vars() {
            match kind {
                VarKind::Arg => {}
                _ => {
                    vars.insert(v, new.fresh_var());
                }
            }
        }

        Self {
            labels,
            slots,
            vars,
            new,
        }
    }

    pub fn translate(mut self: Self, old: &Cfg<Instr>) -> Cfg<RvInstr> {
        for (b, _) in old.iter_blocks() {
            self.translate_block(old, b);
        }

        self.new
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
            Lit::Stack(s) => {
                let id = self.new.fresh_var();
                stmt.push(RvInstr::Ls(Reg::Virt(id), self.slots[s]));
                id
            }
            Lit::Var(v) => self.vars[v]
        }
    }

    pub fn translate_block(&mut self, old: &Cfg<Instr>, block: Label) {
        let mut stmt: Vec<RvInstr> = vec![];

        for instr in old[block].stmt.clone() {
            match instr {
                Instr::Binop(dst, binop, l1, l2) => {
                    let v1 = self.load_lit(&mut stmt, l1);
                    let v2 = self.load_lit(&mut stmt, l2);
                    stmt.extend(RvInstr::from_binop(self.vars[dst], binop, v1, v2));
                }
                Instr::Unop(dst, unop, l) => {
                    let v = self.load_lit(&mut stmt, l);
                    stmt.extend(RvInstr::from_unop(self.vars[dst], unop, v));
                }
                Instr::Return(l) => {
                    let v = self.load_lit(&mut stmt, l);
                    stmt.push(RvInstr::Return(Reg::Virt(v)));
                }
                Instr::Move(dst, l) => {
                    let v = self.load_lit(&mut stmt, l);
                    stmt.push(
                        RvInstr::addi(Reg::Virt(self.vars[dst]), Reg::Virt(v), 0));
                }
                Instr::Load{dest, addr, ..} => {
                    let v = self.load_lit(&mut stmt, addr);
                    stmt.push(RvInstr::Load(Reg::Virt(self.vars[dest]), Reg::Virt(v), 0));
                }
                Instr::Store{val, addr, ..} => {
                    let val = self.load_lit(&mut stmt, val);
                    let addr = self.load_lit(&mut stmt, addr);
                    stmt.push(RvInstr::Load(Reg::Virt(addr), Reg::Virt(val), 0));
                }
                Instr::Jump(l) => {
                    stmt.push(RvInstr::Jump(self.labels[l]));
                }
                Instr::Call(dest, name, args) => {
                    let args = args.into_iter()
                        .map(|a| {
                            Reg::Virt(self.load_lit(&mut stmt, a))
                        }).collect();
                    stmt.push(RvInstr::GenericCall(Reg::Virt(self.vars[dest]), name, args));
                }
                Instr::Branch(cond, l1, l2) => {
                    let cond = self.load_lit(&mut stmt, cond);
                    stmt.push(
                        RvInstr::Branch(
                            RvBranchKind::Ne,
                            Reg::Virt(cond),
                            Reg::Phys(0),
                            self.labels[l1],
                            self.labels[l2]));
                }
                Instr::Phi(dest, args) => {
                    let args = args.into_iter()
                        .map(|(lit, label)| {
                            (Reg::Virt(self.load_lit(&mut stmt, lit)), label)
                        }).collect();
                    stmt.push(RvInstr::Phi(Reg::Virt(self.vars[dest]), args));
                }
            }
        }

        self.new.set_block_stmt(self.labels[block], stmt);
    }
}
