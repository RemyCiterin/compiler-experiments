use super::*;
use super::select::*;
use crate::*;
use crate::ssa::*;
use crate::pattern::*;



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
            Self::Seqz => write!(f, "sltiu(1)"),
            Self::Snez => write!(f, "sltu x0,"),
            Self::Not => write!(f, "xori(-1)"),
            Self::Neg => write!(f, "sub x0,"),
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

    fn eval(&self, _: Vec<i32>) -> Option<i32> { None }
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

    fn eval(&self, _: Vec<i32>) -> Option<bool> { None }
}

pub fn check_riscv_immediate(imm: i32) -> bool {
    imm >= -2048 && imm <= 2047
}

pub type RvInstr = RInstr<RvOp, RvCond>;

pub fn translate_operation
    (select: &mut Selection<RvOp, RvCond>, instr: &Instr, dest: Var) -> Vec<RvInstr> {

    let operation_rules = vec![
        translate_operation_rule!(
            ( Add x (int y) ), check_riscv_immediate(y),
            select dest => vec![RvInstr::Operation(dest, RvOp::Unop(RvUnop::Add, y), vec![x])]
        ),
        translate_operation_rule!(
            ( Add (int y) x ), check_riscv_immediate(y),
            select dest => vec![RvInstr::Operation(dest, RvOp::Unop(RvUnop::Add, y), vec![x])]
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

        // Default patterns in case we don't reconize an immediate
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
                    RvInstr::Operation(tmp, RvOp::Binop(RvBinop::Sub), vec![x, y]),
                    RvInstr::Operation(dest, RvOp::Seqz, vec![tmp])
                ]
            }
        ),
        translate_operation_rule!(
            ( NotEqual x y ), true,
            select dest => {
                let tmp = select.fresh();
                vec![
                    RvInstr::Operation(tmp, RvOp::Binop(RvBinop::Sub), vec![x, y]),
                    RvInstr::Operation(dest, RvOp::Snez, vec![tmp])
                ]
            }
        ),
        translate_operation_rule!(
            ( LessEqual x y ), true,
            select dest => {
                let tmp = select.fresh();
                vec![
                    RvInstr::Operation(tmp, RvOp::Binop(RvBinop::Slt), vec![y, x]),
                    RvInstr::Operation(dest, RvOp::Not, vec![tmp])
                ]
            }
        ),
        translate_operation_rule!(
            ( ULessEqual x y ), true,
            select dest => {
                let tmp = select.fresh();
                vec![
                    RvInstr::Operation(tmp, RvOp::Binop(RvBinop::Sltu), vec![y, x]),
                    RvInstr::Operation(dest, RvOp::Not, vec![tmp])
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
                vec![RvInstr::Branch(RvCond::Eqz, vec![x], l1, l2)]
        ),
        translate_condition_rule!(
            (Equal x 0), true, select l1 l2 =>
                vec![RvInstr::Branch(RvCond::Eqz, vec![x], l1, l2)]
        ),


        translate_condition_rule!(
            (NotEqual x y), true, select l1 l2 =>
                vec![RvInstr::Branch(RvCond::Ne, vec![x, y], l1, l2)]
        ),
        translate_condition_rule!(
            (Equal x y), true, select l1 l2 =>
                vec![RvInstr::Branch(RvCond::Eq, vec![x, y], l1, l2)]
        ),

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
        |select, instr, dest| translate_operation(select, instr, dest),
        |select, lit, l1, l2| translate_condition(select, lit, l1, l2),
    );

    select.rtl()
}
