use super::*;
use super::select::*;
use crate::*;
use crate::ssa::*;
use crate::pattern::*;

pub struct RvArch;

impl Arch for RvArch {
    type Op = RvOp;
    type Cond = RvCond;

    fn pp_mv(f: &mut Formatter<'_>, dest: Phys, src: Phys) -> Result {
        write!(f, "mv {dest}, {src}")
    }

    fn pp_from_int(f: &mut Formatter<'_>, dest: Phys, src: i32) -> Result {
        write!(f, "li {dest}, {src}")
    }

    fn pp_from_addr(f: &mut Formatter<'_>, dest: Phys, src: &str) -> Result {
        write!(f, "la {dest}, {src}")
    }

    fn pp_from_stack(f: &mut Formatter<'_>, dest: Phys, offset: i32) -> Result {
        write!(f, "addi {dest}, sp, {offset}")
    }

    fn pp_op(f: &mut Formatter<'_>, dest: Phys, op: RvOp, args: Vec<Phys>) -> Result {
        match op {
            RvOp::Neg => write!(f, "neg {dest}, {}", args[0]),
            RvOp::Not => write!(f, "not {dest}, {}", args[0]),
            RvOp::Seqz => write!(f, "seqz {dest}, {}", args[0]),
            RvOp::Snez => write!(f, "snez {dest}, {}", args[0]),
            RvOp::Binop(binop) =>
                write!(f, "{binop} {dest}, {}, {}", args[0], args[1]),
            RvOp::Unop(unop, imm) =>
                write!(f, "{unop} {dest}, {}, {}", args[0], imm),
        }
    }

    fn pp_jcc(f: &mut Formatter<'_>, cond: RvCond, args: Vec<Phys>, label: &str) -> Result {
        match cond {
            RvCond::Eq => write!(f, "beq {}, {}, {}", args[0], args[1], label),
            RvCond::Ne => write!(f, "bne {}, {}, {}", args[0], args[1], label),
            RvCond::Lt => write!(f, "blt {}, {}, {}", args[0], args[1], label),
            RvCond::Ltu => write!(f, "bltu {}, {}, {}", args[0], args[1], label),
            RvCond::Ge => write!(f, "bge {}, {}, {}", args[0], args[1], label),
            RvCond::Geu => write!(f, "bgeu {}, {}, {}", args[0], args[1], label),
            RvCond::Eqz => write!(f, "beqz {}, {}", args[0], label),
            RvCond::Nez => write!(f, "bnez {}, {}", args[0], label),
        }
    }

    fn pp_jump(f: &mut Formatter<'_>, label: &str) -> Result {
        write!(f, "j {label}")
    }

    fn pp_load(f: &mut Formatter<'_>, dest: Phys, addr: Phys) -> Result {
        write!(f, "lw {dest}, ({addr})")
    }

    fn pp_load_local(f: &mut Formatter<'_>, dest: Phys, offset: i32) -> Result {
        write!(f, "lw {dest}, {offset}(sp)")
    }

    fn pp_store(f: &mut Formatter<'_>, addr: Phys, val: Phys) -> Result {
        write!(f, "sw {val}, ({addr})")
    }

    fn pp_store_local(f: &mut Formatter<'_>, offset: i32, val: Phys) -> Result {
        write!(f, "sw {val}, {offset}(sp)")
    }

    fn pp_return(f: &mut Formatter<'_>) -> Result {
        write!(f, "ret")
    }

    fn pp_call(f: &mut Formatter<'_>, name: &str) -> Result {
        write!(f, "call {name}")
    }

    fn pp_push(f: &mut Formatter<'_>, size: i32) -> Result {
        write!(f, "addi sp, sp, {}\n", -size)?;
        write!(f, "\tsw ra, {}(sp)", size)
    }

    fn pp_pop(f: &mut Formatter<'_>, size: i32) -> Result {
        write!(f, "lw ra, {}(sp)\n", size)?;
        write!(f, "\taddi sp, sp, {}", size)
    }

    fn ret_reg() -> Phys {
        Phys(10)
    }

    fn arg_regs() -> Vec<Phys> {
        vec![
            Phys(10),
            Phys(11),
            Phys(12),
            Phys(13),
            Phys(14),
            Phys(15),
            Phys(16),
            Phys(17),
        ]
    }

    fn callee_saved() -> Vec<Phys> {
        vec![
            Phys(8),
            Phys(9),

            Phys(18),
            Phys(19),
            Phys(20),
            Phys(21),
            Phys(22),
            Phys(23),
            Phys(24),
            Phys(25),
            Phys(26),
            Phys(27),
        ]
    }

    fn caller_saved() -> Vec<Phys> {
        vec![
            Phys(5),
            Phys(6),
            Phys(7),

            Phys(10),
            Phys(11),
            Phys(12),
            Phys(13),
            Phys(14),
            Phys(15),
            Phys(16),
            Phys(17),

            Phys(28),
            Phys(29),
            Phys(30),
            Phys(31),
        ]
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
pub enum RvCond {
    Eq, Ne, Lt, Ltu, Ge, Geu, Eqz, Nez
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
            Self::Slt => write!(f, "slt"),
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

impl std::fmt::Display for RvCond {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Eq => write!(f, "beq"),
            Self::Ne => write!(f, "bne"),
            Self::Lt => write!(f, "blt"),
            Self::Ltu => write!(f, "bltu"),
            Self::Ge => write!(f, "bge"),
            Self::Geu => write!(f, "bgeu"),
            Self::Eqz => write!(f, "beqz"),
            Self::Nez => write!(f, "bnez"),
        }
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum RvOp {
    Binop(RvBinop),
    Unop(RvUnop, i32),
    Seqz,
    Snez,
    Neg,
    Not,

}

impl std::fmt::Display for RvOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Binop(b) => write!(f, "{b}"),
            Self::Unop(u, x) => write!(f, "{u}({x})"),
            Self::Seqz => write!(f, "seqz"),
            Self::Snez => write!(f, "snez"),
            Self::Not => write!(f, "not"),
            Self::Neg => write!(f, "neg"),
        }
    }
}

impl Operation for RvOp {
    fn arity(&self) -> usize {
        match self {
            Self::Binop(..) => 2,
            _ => 1
        }
    }

    fn may_have_side_effect(&self) -> bool { false }

    fn eval(&self, v: Vec<i32>) -> Option<i32> {
        match self {
            RvOp::Seqz => Some( (v[0] == 0) as i32 ),
            RvOp::Snez => Some( (v[0] != 0) as i32 ),
            RvOp::Not => Some( !v[0] ),
            RvOp::Neg => Some( -v[0] ),
            RvOp::Binop(RvBinop::Add) => Some( v[0] + v[1] ),
            RvOp::Binop(RvBinop::Sub) => Some( v[0] - v[1] ),
            RvOp::Binop(RvBinop::And) => Some( v[0] & v[1] ),
            RvOp::Binop(RvBinop::Or) => Some( v[0] | v[1] ),
            RvOp::Binop(RvBinop::Xor) => Some( v[0] ^ v[1] ),
            RvOp::Binop(RvBinop::Slt) => Some( (v[0] < v[1]) as i32 ),
            RvOp::Binop(RvBinop::Sltu) =>
                Some( (v[0].cast_unsigned() < v[1].cast_unsigned()) as i32 ),
            RvOp::Binop(RvBinop::Sll) => Some( crate::ast::sll(v[0], v[1]) ),
            RvOp::Binop(RvBinop::Srl) => Some( crate::ast::srl(v[0], v[1]) ),
            RvOp::Binop(RvBinop::Sra) => Some( v[0].wrapping_shr(v[1].cast_unsigned()) ),
            RvOp::Unop(RvUnop::Add, imm) => Some( v[0] + *imm ),
            RvOp::Unop(RvUnop::And, imm) => Some( v[0] & *imm ),
            RvOp::Unop(RvUnop::Or, imm) => Some( v[0] | *imm ),
            RvOp::Unop(RvUnop::Xor, imm) => Some( v[0] ^ *imm ),
            RvOp::Unop(RvUnop::Slt, imm) => Some( (v[0] < *imm) as i32 ),
            RvOp::Unop(RvUnop::Sltu, imm) =>
                Some( (v[0].cast_unsigned() < imm.cast_unsigned()) as i32 ),
            RvOp::Unop(RvUnop::Sll, imm) => Some( crate::ast::sll(v[0], *imm) ),
            RvOp::Unop(RvUnop::Srl, imm) => Some( crate::ast::srl(v[0], *imm) ),
            RvOp::Unop(RvUnop::Sra, imm) => Some( v[0].wrapping_shr(imm.cast_unsigned()) ),
        }
    }
}

impl Condition for RvCond {
    fn arity(&self) -> usize {
        match self {
            Self::Eqz => 1,
            Self::Nez => 1,
            _ => 2
        }
    }

    fn may_have_side_effect(&self) -> bool { false }

    fn eval(&self, v: Vec<i32>) -> Option<bool> {
        match self {
            RvCond::Eq => Some( v[0] == v[1] ),
            RvCond::Ne => Some( v[0] != v[1] ),
            RvCond::Nez => Some( v[0] != 0 ),
            RvCond::Eqz => Some( v[0] == 0 ),
            RvCond::Lt => Some( v[0] < v[1] ),
            RvCond::Ltu => Some( v[0].cast_unsigned() < v[1].cast_unsigned() ),
            RvCond::Ge => Some( v[0] >= v[1] ),
            RvCond::Geu => Some( v[0].cast_unsigned() >= v[1].cast_unsigned() ),
        }
    }
}

pub fn check_riscv_immediate(imm: i32) -> bool {
    imm >= -2048 && imm <= 2047
}

pub type RvInstr = RInstr<RvOp, RvCond>;

pub fn translate_operation
    (select: &mut Selection<RvOp, RvCond>, instr: &Instr, dest: Var) -> Vec<RvInstr> {

    let operation_rules = vec![
        // First: we try to detect some cases where we can propagate the immediate
        translate_operation_rule!(
            ( Add x (int y) ), check_riscv_immediate(y),
            select dest => vec![RvInstr::Operation(dest, RvOp::Unop(RvUnop::Add, y), vec![x])]
        ),
        translate_operation_rule!(
            ( Add (int y) x ), check_riscv_immediate(y),
            select dest => vec![RvInstr::Operation(dest, RvOp::Unop(RvUnop::Add, y), vec![x])]
        ),
        translate_operation_rule!(
            ( And x (int y) ), check_riscv_immediate(y),
            select dest => vec![RvInstr::Operation(dest, RvOp::Unop(RvUnop::And, y), vec![x])]
        ),
        translate_operation_rule!(
            ( And (int y) x ), check_riscv_immediate(y),
            select dest => vec![RvInstr::Operation(dest, RvOp::Unop(RvUnop::And, y), vec![x])]
        ),
        translate_operation_rule!(
            ( Or x (int y) ), check_riscv_immediate(y),
            select dest => vec![RvInstr::Operation(dest, RvOp::Unop(RvUnop::Or, y), vec![x])]
        ),
        translate_operation_rule!(
            ( Or (int y) x ), check_riscv_immediate(y),
            select dest => vec![RvInstr::Operation(dest, RvOp::Unop(RvUnop::Or, y), vec![x])]
        ),
        translate_operation_rule!(
            ( Xor x (int y) ), check_riscv_immediate(y),
            select dest => vec![RvInstr::Operation(dest, RvOp::Unop(RvUnop::Xor, y), vec![x])]
        ),
        translate_operation_rule!(
            ( Xor (int y) x ), check_riscv_immediate(y),
            select dest => vec![RvInstr::Operation(dest, RvOp::Unop(RvUnop::Xor, y), vec![x])]
        ),
        translate_operation_rule!(
            ( Sub x (int y) ), check_riscv_immediate(y.wrapping_neg()),
            select dest => vec![
                RvInstr::Operation(dest, RvOp::Unop(RvUnop::Add, y.wrapping_neg()), vec![x])
            ]
        ),
        translate_operation_rule!(
            ( Sll x (int y) ), 0 <= y && y < 32,
            select dest => vec![RvInstr::Operation(dest, RvOp::Unop(RvUnop::Sll, y), vec![x])]
        ),
        translate_operation_rule!(
            ( Sra x (int y) ), 0 <= y && y < 32,
            select dest => vec![RvInstr::Operation(dest, RvOp::Unop(RvUnop::Sra, y), vec![x])]
        ),
        translate_operation_rule!(
            ( Srl x (int y) ), 0 <= y && y < 32,
            select dest => vec![RvInstr::Operation(dest, RvOp::Unop(RvUnop::Srl, y), vec![x])]
        ),
        translate_operation_rule!(
            ( Equal x (int y) ), check_riscv_immediate(y.wrapping_neg()),
            select dest => {
                let tmp = select.fresh();
                vec![
                    RvInstr::Operation(tmp, RvOp::Unop(RvUnop::Add, y.wrapping_neg()), vec![x]),
                    RvInstr::Operation(dest, RvOp::Seqz, vec![tmp])
                ]
            }
        ),
        translate_operation_rule!(
            ( Equal (int y) x ), check_riscv_immediate(y.wrapping_neg()),
            select dest => {
                let tmp = select.fresh();
                vec![
                    RvInstr::Operation(tmp, RvOp::Unop(RvUnop::Add, y.wrapping_neg()), vec![x]),
                    RvInstr::Operation(dest, RvOp::Seqz, vec![tmp])
                ]
            }
        ),

        // Then we simplify some kind of equalities to replace them by pseudo-instructions
        translate_operation_rule!(
            ( Equal x 0 ), true,
            select dest => vec![RvInstr::Operation(dest, RvOp::Seqz, vec![x])]
        ),
        translate_operation_rule!(
            ( NotEqual x 0 ), true,
            select dest => vec![RvInstr::Operation(dest, RvOp::Snez, vec![x])]
        ),
        translate_operation_rule!(
            ( Equal 0 x ), true,
            select dest => vec![RvInstr::Operation(dest, RvOp::Seqz, vec![x])]
        ),
        translate_operation_rule!(
            ( NotEqual 0 x ), true,
            select dest => vec![RvInstr::Operation(dest, RvOp::Snez, vec![x])]
        ),


        // Default patterns in case we don't reconize a possible simplification
        translate_operation_rule!(
            ( Not x ), true,
            select dest => vec![RvInstr::Operation(dest, RvOp::Not, vec![x])]
        ),
        translate_operation_rule!(
            ( Neg x ), true,
            select dest => vec![RvInstr::Operation(dest, RvOp::Neg, vec![x])]
        ),
        translate_operation_rule!(
            ( ULessThan x y ), true,
            select dest => vec![RvInstr::Operation(dest, RvOp::Binop(RvBinop::Sltu), vec![x, y])]
        ),
        translate_operation_rule!(
            ( Add x y ), true,
            select dest => vec![RvInstr::Operation(dest, RvOp::Binop(RvBinop::Add), vec![x, y])]
        ),
        translate_operation_rule!(
            ( Sub x y ), true,
            select dest => vec![RvInstr::Operation(dest, RvOp::Binop(RvBinop::Sub), vec![x, y])]
        ),
        translate_operation_rule!(
            ( And x y ), true,
            select dest => vec![RvInstr::Operation(dest, RvOp::Binop(RvBinop::And), vec![x, y])]
        ),
        translate_operation_rule!(
            ( Or x y ), true,
            select dest => vec![RvInstr::Operation(dest, RvOp::Binop(RvBinop::Or), vec![x, y])]
        ),
        translate_operation_rule!(
            ( Xor x y ), true,
            select dest => vec![RvInstr::Operation(dest, RvOp::Binop(RvBinop::Xor), vec![x, y])]
        ),
        translate_operation_rule!(
            ( Sll x y ), true,
            select dest => vec![RvInstr::Operation(dest, RvOp::Binop(RvBinop::Sll), vec![x, y])]
        ),
        translate_operation_rule!(
            ( Sra x y ), true,
            select dest => vec![RvInstr::Operation(dest, RvOp::Binop(RvBinop::Sra), vec![x, y])]
        ),
        translate_operation_rule!(
            ( Srl x y ), true,
            select dest => vec![RvInstr::Operation(dest, RvOp::Binop(RvBinop::Srl), vec![x, y])]
        ),
        translate_operation_rule!(
            ( LessThan x y ), true,
            select dest => vec![RvInstr::Operation(dest, RvOp::Binop(RvBinop::Slt), vec![x, y])]
        ),
        translate_operation_rule!(
            ( ULessThan x y ), true,
            select dest => vec![RvInstr::Operation(dest, RvOp::Binop(RvBinop::Sltu), vec![x, y])]
        ),
        translate_operation_rule!(
            ( Equal x y ), true,
            select dest => {
                let tmp = select.fresh();
                vec![
                    RvInstr::Operation(tmp, RvOp::Binop(RvBinop::Xor), vec![x, y]),
                    RvInstr::Operation(dest, RvOp::Seqz, vec![tmp])
                ]
            }
        ),
        translate_operation_rule!(
            ( NotEqual x y ), true,
            select dest => {
                let tmp = select.fresh();
                vec![
                    RvInstr::Operation(tmp, RvOp::Binop(RvBinop::Xor), vec![x, y]),
                    RvInstr::Operation(dest, RvOp::Snez, vec![tmp])
                ]
            }
        ),
        translate_operation_rule!(
            ( LessEqual x y ), true,
            select dest => {
                let tmp1 = select.fresh();
                let tmp2 = select.fresh();
                vec![
                    RvInstr::Move(tmp2, Lit::Int(1)),
                    RvInstr::Operation(tmp1, RvOp::Binop(RvBinop::Slt), vec![y, x]),
                    RvInstr::Operation(dest, RvOp::Binop(RvBinop::Sub), vec![tmp2, tmp1])
                ]
            }
        ),
        translate_operation_rule!(
            ( ULessEqual x y ), true,
            select dest => {
                let tmp1 = select.fresh();
                let tmp2 = select.fresh();
                vec![
                    RvInstr::Move(tmp2, Lit::Int(1)),
                    RvInstr::Operation(tmp1, RvOp::Binop(RvBinop::Sltu), vec![y, x]),
                    RvInstr::Operation(dest, RvOp::Binop(RvBinop::Sub), vec![tmp2, tmp1])
                ]
            }
        ),
    ];

    for rule in operation_rules.iter() {
        let result =
            search_pattern(rule.pattern(), &mut select.old, instr);

        if let Some(occ) = result && rule.test(&occ) {
            return rule.transform(select, occ, dest);
        }
    }


    unreachable!();
}

pub fn translate_condition
    (select: &mut Selection<RvOp, RvCond>, lit: Lit, l1: Label, l2: Label) -> Vec<RvInstr> {

    let condition_rules = vec![
        translate_condition_rule!(
            (NotEqual 0 x), true, select l1 l2 =>
                vec![RvInstr::Branch(RvCond::Nez, vec![x], l1, l2)]
        ),
        translate_condition_rule!(
            (NotEqual x 0), true, select l1 l2 =>
                vec![RvInstr::Branch(RvCond::Nez, vec![x], l1, l2)]
        ),
        translate_condition_rule!(
            (Equal 0 x), true, select l1 l2 =>
                vec![RvInstr::Branch(RvCond::Nez, vec![x], l2, l1)]
        ),
        translate_condition_rule!(
            (Equal x 0), true, select l1 l2 =>
                vec![RvInstr::Branch(RvCond::Nez, vec![x], l2, l1)]
        ),


        translate_condition_rule!(
            (NotEqual x y), true, select l1 l2 =>
                vec![RvInstr::Branch(RvCond::Ne, vec![x, y], l1, l2)]
        ),
        translate_condition_rule!(
            (Equal x y), true, select l1 l2 =>
                vec![RvInstr::Branch(RvCond::Ne, vec![x, y], l2, l1)]
        ),

        translate_condition_rule!(
            (LessThan x y), true, select l1 l2 =>
                vec![RvInstr::Branch(RvCond::Lt, vec![x, y], l1, l2)]
        ),
        translate_condition_rule!(
            (ULessThan x y), true, select l1 l2 =>
                vec![RvInstr::Branch(RvCond::Ltu, vec![x, y], l1, l2)]
        ),
        translate_condition_rule!(
            (LessEqual x y), true, select l1 l2 =>
                vec![RvInstr::Branch(RvCond::Ge, vec![y, x], l1, l2)]
        ),
        translate_condition_rule!(
            (ULessEqual x y), true, select l1 l2 =>
                vec![RvInstr::Branch(RvCond::Geu, vec![y, x], l1, l2)]
        ),

        // Default pattern if we don't find a way to optimize the branch
        translate_condition_rule!(
            x, true, select l1 l2 =>
                vec![RvInstr::Branch(RvCond::Nez, vec![x], l1, l2)]
        ),
    ];

    for rule in condition_rules.iter() {
        let result =
            search_pattern_in_lit(rule.pattern(), &mut select.old, lit.clone());

        if let Some(occ) = result && rule.test(&occ) {
            return rule.transform(select, occ, l1, l2);
        }
    }

    unreachable!();

}

pub fn translate(cfg: Cfg<Instr>) -> Cfg<RvInstr> {
    let mut select = Selection::new(cfg);


    select.run(
        |select, instr, dest|
            translate_operation(select, instr, dest),
        |select, lit, l1, l2|
            translate_condition(select, lit, l1, l2),
    );

    select.rtl()
}
