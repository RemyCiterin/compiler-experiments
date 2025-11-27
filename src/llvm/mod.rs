use llvm_ir::module::Module;
use llvm_ir::predicates::*;
use llvm_ir::constant::*;
use llvm_ir::types::*;
use llvm_ir::*;

use either::*;

use crate::ssa::*;
use crate::ast::*;
use std::collections::HashMap;


pub fn constant_as_int(cst: ConstantRef) -> Option<u64> {
    if let Constant::Int { value, .. } = cst.as_ref() { Some(*value) }
    else { None }
}

pub fn operand_as_int(op: Operand) -> Option<u64> {
    if let Operand::ConstantOperand(cst) = op { constant_as_int(cst) }
    else { None }
}

/// Number of `word` used to represent a type
pub fn type_words(types: &Types, ty: &TypeRef) -> usize {
    (type_size(types, ty) + 3) / 4
}

/// Number of `byte` used to represent a type
pub fn type_bytes(types: &Types, ty: &TypeRef) -> usize {
    type_size(types, ty)
}

/// Number of `short` used to represent a type
pub fn type_shorts(types: &Types, ty: &TypeRef) -> usize {
    (type_size(types, ty) + 1) / 2
}

/// return the number of bits of an integer type
pub fn type_bits(ty: &TypeRef) -> u32 {
    match ty.as_ref() {
        Type::IntegerType { bits } => *bits,
        _ => panic!("{ty} is not an integer type"),
    }
}

/// Alignment of an element of a given type in memory
pub fn type_alignment(types: &Types, ty: &TypeRef) -> usize {
    match ty.as_ref() {
        Type::VoidType => 0,
        Type::LabelType => 4,
        Type::FuncType { .. } => 4,
        Type::PointerType { .. } => 4,
        Type::IntegerType { bits } =>
            (*bits+7) as usize / 8,
        Type::ArrayType { element_type, .. } =>
            type_alignment(types, &element_type),
        Type::StructType { element_types, .. } =>
            StructLayout::align(types, element_types.as_slice()),
        Type::NamedStructType { name } => {
            match types.named_struct_def(name).unwrap() {
                NamedStructDef::Opaque => panic!("Can't infer the alignment of an opaque struct"),
                NamedStructDef::Defined(def) => type_alignment(types, def),
            }
        }
        Type::VectorType { .. } => todo!(),
        Type::FPType(..) => todo!(),
        _ => todo!()
    }
}

/// Number of bytes used to represent a type, the alignment of a type must divide it's size
pub fn type_size(types: &Types, ty: &TypeRef) -> usize {
    match ty.as_ref() {
        Type::VoidType => 0,
        Type::LabelType => 4,
        Type::FuncType { .. } => 4,
        Type::PointerType { .. } => 4,
        Type::IntegerType { bits } => (*bits+7) as usize / 8,
        Type::ArrayType { element_type, num_elements } =>
            *num_elements as usize * type_alignment(types, &element_type),
        Type::StructType { element_types, .. } =>
            StructLayout::size(types, element_types.as_slice()),
        Type::NamedStructType { name } => {
            match types.named_struct_def(name).unwrap() {
                NamedStructDef::Opaque => panic!("Can't infer the size of an opaque struct"),
                NamedStructDef::Defined(def) => type_alignment(types, def),
            }
        }
        Type::VectorType { .. } => todo!(),
        Type::FPType(..) => todo!(),
        _ => todo!()
    }
}

pub struct StructLayout{
    pub offsets: Vec<usize>,
    pub align: usize,
    pub size: usize,
}

impl StructLayout {
    /// Compute the alignment of a struct
    pub fn align(types: &Types, fields: &[TypeRef]) -> usize {
        let mut align = 0;

        for op in fields {
            align = usize::max(type_alignment(types, op), align);
        }

        return align;
    }

    /// Compute the size of a struct
    pub fn size(types: &Types, fields: &[TypeRef]) -> usize {
        let mut align = 0;

        let mut offset = 0;
        for field in fields {
            let a = type_alignment(types, field);
            let s = type_size(types, field);

            align = usize::max(a, align);

            if offset % a != 0 {
                offset += a - (offset % a);
            }

            offset += s;
        }

        if offset % align != 0 {
            offset += align - (offset % align);
        }

        offset
    }

    /// Compute the layout of a struct
    pub fn new(types: &Types, fields: &[TypeRef]) -> Self {
        let mut offsets = Vec::new();
        let mut align = 0;

        let mut offset = 0;
        for field in fields {
            let a = type_alignment(types, field);
            let s = type_size(types, field);

            align = usize::max(a, align);

            if offset % a != 0 {
                offset += a - (offset % a);
            }

            offsets.push(offset);
            offset += s;
        }

        if offset % align != 0 {
            offset += align - (offset % align);
        }

        Self { align, size: offset, offsets }
    }
}

pub type Value = Vec<Var>;

pub struct CfgBuilder<'a> {
    // Control flow raph under construction
    cfg: Cfg<COp, CCond>,

    // Label of the block under construction
    label: Label,

    // Body of the block under construction
    stmt: Vec<Instr<COp, CCond>>,

    // Map from llvm labels to CFG input/output labels
    labels: HashMap<Name, Label>,

    // map from names to values
    names: HashMap<Name, Value>,

    types: &'a Types,

    exits: HashMap<(Label, Label), Label>,

    blocks_addresses: HashMap<Name, i32>,

    next_block_address: i32,

    current_block: Option<Name>,
}


impl<'a> CfgBuilder<'a> {
    pub fn new(module: &'a Module) -> Self {
        let cfg = Cfg::new(true);
        let entry = cfg.entry();

        Self {
            cfg,
            label: entry,
            types: &module.types,
            labels: HashMap::new(),
            names: HashMap::new(),
            exits: HashMap::new(),
            blocks_addresses: HashMap::new(),
            next_block_address: 0,
            current_block: None,
            stmt: vec![],
        }
    }

    pub fn add_parameter(&mut self, paramter: llvm_ir::function::Parameter) {
        let words = type_words(self.types, &paramter.ty);

        let mut vars = vec![];
        for _ in 0..words { vars.push(self.cfg.fresh_arg()); }

        self.new_value_with(paramter.name, vars);
    }

    /// Return the entry point of an LLVM block into the control flow graph
    pub fn label(&mut self, name: Name) -> Label {
        if let Some(l) = self.labels.get(&name) {
            return *l;
        }

        let l = self.cfg.fresh_label();
        self.labels.insert(name, l);
        return l;
    }

    /// A unique identifier used to compute the result of the `blockaddress` instruction
    pub fn block_address(&mut self, name: Name) -> i32 {
        if let Some(x) = self.blocks_addresses.get(&name) { return *x; }

        let ret = self.next_block_address;
        self.blocks_addresses.insert(name, ret);
        self.next_block_address += 1;
        return ret;
    }

    /// Each LLVM register of type `ty` is represented with a vector of `type_words(&ty)` CFG
    /// variables, this function create a variables for the register `name` of type `ty` or return
    /// the existing one if the register is already defined (for a PHI instruction as example)
    pub fn new_value(&mut self, name: Name, ty: TypeRef) -> Value {
        if let Some(v) = self.names.get(&name) { return v.clone(); }

        let words = type_words(self.types, &ty);
        let mut val = vec![];

        for _ in 0..words {
            val.push(self.cfg.fresh_var());
        }

        self.names.insert(name, val.clone());
        return val;
    }

    /// Each LLVM register of type `ty` is represented with a vector of `type_words(&ty)` CFG
    /// variables, this function create a variables for the register `name` of type `ty` with the
    /// given vector of CFG variables, of perform a set of move instructions if the `register` is
    /// already defined.
    pub fn new_value_with(&mut self, name: Name, v: Value) {
        if let Some(old) = self.names.get(&name) {
            // Copy the new value into old
            assert!(v.len() == old.len());

            for i in 0..v.len() {
                self.stmt.push(Instr::Move(old[i], Lit::Var(v[i])));
            }

            return;
        }

        self.names.insert(name, v);
    }

    pub fn value(&mut self, name: Name) -> Value {
        self.names[&name].clone()
    }

    pub fn finish_block(&mut self) {
        self.cfg.set_block_stmt(self.label, std::mem::take(&mut self.stmt));
    }

    /// Generate a constant integer
    pub fn mk_int(&mut self, bits: usize, value: u64) -> Value {
        let lsb: u32 = (value & ((1u64 << 32) - 1)) as u32;
        let msb: u32 = (value >> 32) as u32;

        if bits <= 32 {
            let ret = self.cfg.fresh_var();
            self.stmt.push(Instr::Move(ret, Lit::Int(lsb.cast_signed())));
            return vec![ret];
        }

        assert!(bits <= 64);
        let r1 = self.cfg.fresh_var();
        let r2 = self.cfg.fresh_var();
        self.stmt.push(Instr::Move(r1, Lit::Int(lsb.cast_signed())));
        self.stmt.push(Instr::Move(r2, Lit::Int(msb.cast_signed())));
        return vec![r1,r2];
    }

    /// Perform a bitwise operation
    pub fn mk_bitwise_binop(&mut self, binop: COp, lhs: Value, rhs: Value) -> Value {
        assert!(lhs.len() == rhs.len());
        let mut ret = vec![];

        for (x,y) in lhs.into_iter().zip(rhs.into_iter()) {
            let r = self.cfg.fresh_var();
            self.stmt.push(Instr::Operation(r, binop, vec![x,y]));
            ret.push(r);
        }

        ret
    }

    /// Perform an addition of arbitrary size integers
    pub fn mk_add(&mut self, lhs: Value, rhs: Value) -> Value {
        assert!(lhs.len() == rhs.len());
        let mut ret = vec![];

        let mut carry = self.mk_int(32, 0)[0];

        for (x,y) in lhs.into_iter().zip(rhs.into_iter()) {
            let tmp1 = self.cfg.fresh_var();
            let r = self.cfg.fresh_var();

            self.stmt.push(Instr::Operation(tmp1, COp::Add, vec![x, y]));
            self.stmt.push(Instr::Operation(r, COp::Add, vec![tmp1, carry]));
            ret.push(r);

            carry = self.cfg.fresh_var();
            self.stmt.push(Instr::Operation(carry, COp::ULessThan, vec![r, x]));
        }

        ret
    }

    /// Perform an addition of arbitrary size integers
    pub fn mk_ptr_add(&mut self, ptr: Var, offset: Value) -> Var {
        let off = self.mk_truncate(offset, 32)[0];
        let r = self.cfg.fresh_var();

        self.stmt.push(Instr::Operation(r, COp::PtrAdd, vec![ptr,off]));
        r
    }

    pub fn mk_sub(&mut self, lhs: Value, rhs: Value) -> Value {
        assert!(lhs.len() == rhs.len());
        let mut ret = vec![];

        let mut carry = self.mk_int(32, 0)[0];

        for (x,y) in lhs.into_iter().zip(rhs.into_iter()) {
            let tmp1 = self.cfg.fresh_var();
            let r = self.cfg.fresh_var();

            self.stmt.push(Instr::Operation(tmp1, COp::Sub, vec![x, y]));
            self.stmt.push(Instr::Operation(r, COp::Add, vec![tmp1, carry]));
            ret.push(r);

            carry = self.cfg.fresh_var();
            self.stmt.push(Instr::Operation(carry, COp::ULessThan, vec![x, r]));
        }

        ret
    }

    pub fn mk_mul(&mut self, lhs: Value, rhs: Value) -> Value {
        todo!()
    }

    pub fn mk_neq(&mut self, lhs: Value, rhs: Value) -> Var {
        let mut ret = self.mk_int(32, 0)[0];
        assert!(lhs.len() == rhs.len());

        for (i, (x,y)) in lhs.into_iter().zip(rhs.into_iter()).enumerate() {
            let tmp1 = self.cfg.fresh_var();

            if i == 0 {
                self.stmt.push(Instr::Operation(tmp1, COp::NotEqual, vec![x,y]));
                ret = tmp1;
            } else {
                let tmp2 = self.cfg.fresh_var();
                self.stmt.push(Instr::Operation(tmp1, COp::NotEqual, vec![x,y]));
                self.stmt.push(Instr::Operation(tmp2, COp::Or, vec![ret,tmp1]));
                ret = tmp2;
            }
        }

        ret
    }

    pub fn mk_eq(&mut self, lhs: Value, rhs: Value) -> Var {
        let mut ret = self.mk_int(32, 1)[0];
        assert!(lhs.len() == rhs.len());

        for (i, (x,y)) in lhs.into_iter().zip(rhs.into_iter()).enumerate() {
            let tmp1 = self.cfg.fresh_var();

            if i == 0 {
                self.stmt.push(Instr::Operation(tmp1, COp::Equal, vec![x,y]));
                ret = tmp1;
            } else {
                let tmp2 = self.cfg.fresh_var();
                self.stmt.push(Instr::Operation(tmp1, COp::Equal, vec![x,y]));
                self.stmt.push(Instr::Operation(tmp2, COp::And, vec![ret,tmp1]));
                ret = tmp2;
            }
        }

        ret
    }

    pub fn mk_logical_not(&mut self, val: Var) -> Var {
        let ret = self.cfg.fresh_var();
        let zero = self.mk_int(32, 0)[0];
        self.stmt.push(Instr::Operation(ret, COp::Equal, vec![val, zero]));
        return ret;
    }


    pub fn mk_logical_and(&mut self, lhs: Var, rhs: Var) -> Var {
        self.mk_select(lhs,
            |_| vec![rhs],
            |builder|
                builder.mk_int(32,0),
        )[0]
    }


    pub fn mk_logical_or(&mut self, lhs: Var, rhs: Var) -> Var {
        self.mk_select(lhs,
            |builder|
                builder.mk_int(32,1),
            |_| vec![rhs],
        )[0]
    }

    pub fn mk_select<F1,F2>(&mut self, cond: Var, f1: F1, f2: F2) -> Value
    where F1: FnOnce(&mut Self) -> Value, F2: FnOnce(&mut Self) -> Value {

        let t_begin = self.cfg.fresh_label();
        let e_begin = self.cfg.fresh_label();
        let join = self.cfg.fresh_label();

        self.stmt.push(Instr::Branch(CCond::Nez, vec![cond], t_begin, e_begin));
        self.finish_block();

        self.label = t_begin;
        let v1 = f1(self);
        let t_end = self.label;
        self.stmt.push(Instr::Jump(join));
        self.finish_block();

        self.label = e_begin;
        let v2 = f2(self);
        let e_end = self.label;
        self.stmt.push(Instr::Jump(join));
        self.finish_block();


        self.label = join;

        let mut ret = vec![];
        assert!(v1.len() == v2.len());

        for (x,y) in v1.into_iter().zip(v2.into_iter()) {
            let dest = self.cfg.fresh_var();
            self.stmt.push(Instr::Phi(dest, vec![(Lit::Var(x), t_end), (Lit::Var(y), e_end)]));
            ret.push(dest);
        }

        ret
    }

    pub fn mk_ugreater(&mut self, equal: bool, lhs: &[Var], rhs: &[Var]) -> Var {
        self.mk_uless(equal, rhs, lhs)
    }

    pub fn mk_sgreater(&mut self, equal: bool, lhs: &[Var], rhs: &[Var]) -> Var {
        self.mk_sless(equal, rhs, lhs)
    }

    pub fn mk_uless(&mut self, equal: bool, lhs: &[Var], rhs: &[Var]) -> Var {
        assert!(lhs.len() == rhs.len());

        if lhs.len() == 0 { return self.mk_int(32,if equal {1} else {0})[0]; }

        let l = *lhs.last().unwrap();
        let r = *rhs.last().unwrap();

        let less = self.cfg.fresh_var();

        if lhs.len() == 1 {
            let op = if equal { COp::ULessEqual } else { COp::ULessThan };
            self.stmt.push(Instr::Operation(less, op, vec![l, r]));
            return less;
        }

        self.stmt.push(Instr::Operation(less, COp::ULessEqual, vec![l, r]));

        let eq = self.cfg.fresh_var();
        self.stmt.push(Instr::Operation(eq, COp::Equal, vec![l, r]));

        self.mk_select(less,
            |builder| {
                builder.mk_select(eq,
                    |builder|
                        vec![builder.mk_uless(equal, &lhs[0..lhs.len()-1], &rhs[0..rhs.len()-1])],
                    |builder|
                        builder.mk_int(32, 1)
                )
            },
            |builder| {
                builder.mk_int(32, 0)
            }
        )[0]
    }

    pub fn mk_sless(&mut self, equal: bool, lhs: &[Var], rhs: &[Var]) -> Var {
        assert!(lhs.len() == rhs.len());

        if lhs.len() == 0 { return self.mk_int(32,if equal {1} else {0})[0]; }

        let l = *lhs.last().unwrap();
        let r = *rhs.last().unwrap();

        let less = self.cfg.fresh_var();

        if lhs.len() == 1 {
            let op = if equal { COp::LessEqual } else { COp::LessThan };
            self.stmt.push(Instr::Operation(less, op, vec![l, r]));
            return less;
        }

        self.stmt.push(Instr::Operation(less, COp::LessEqual, vec![l, r]));

        let eq = self.cfg.fresh_var();
        self.stmt.push(Instr::Operation(eq, COp::Equal, vec![l, r]));

        self.mk_select(less,
            |builder| {
                builder.mk_select(eq,
                    |builder|
                        vec![builder.mk_uless(equal, &lhs[0..lhs.len()-1], &rhs[0..rhs.len()-1])],
                    |builder|
                        builder.mk_int(32, 1)
                )
            },
            |builder| {
                builder.mk_int(32, 0)
            }
        )[0]
    }

    pub fn mk_icmp(&mut self, cmp: IntPredicate, lhs: Value, rhs: Value) -> Var {
        match cmp {
            IntPredicate::EQ =>
                self.mk_eq(lhs, rhs),
            IntPredicate::NE =>
                self.mk_neq(lhs, rhs),
            IntPredicate::UGT =>
                self.mk_ugreater(false, &lhs, &rhs),
            IntPredicate::UGE =>
                self.mk_ugreater(true, &lhs, &rhs),
            IntPredicate::ULT =>
                self.mk_uless(false, &lhs, &rhs),
            IntPredicate::ULE =>
                self.mk_uless(true, &lhs, &rhs),
            IntPredicate::SGT =>
                self.mk_sgreater(false, &lhs, &rhs),
            IntPredicate::SGE =>
                self.mk_sgreater(true, &lhs, &rhs),
            IntPredicate::SLT =>
                self.mk_sless(false, &lhs, &rhs),
            IntPredicate::SLE =>
                self.mk_sless(true, &lhs, &rhs),
        }
    }

    pub fn mk_undef(&mut self, words: usize) -> Value {
        let mut ret = vec![];
        for _ in 0..words {
            let dest = self.cfg.fresh_var();
            self.stmt.push(Instr::Move(dest, Lit::Undef));
            ret.push(dest);
        }

        ret
    }

    pub fn mk_zero(&mut self, words: usize) -> Value {
        let mut ret = vec![];
        for _ in 0..words {
            let dest = self.cfg.fresh_var();
            self.stmt.push(Instr::Move(dest, Lit::Int(0)));
            ret.push(dest);
        }

        ret
    }

    pub fn mk_global_reference(&mut self, name: String) -> Var {
        let ret = self.cfg.fresh_var();
        self.stmt.push(Instr::Move(ret, Lit::Addr(name)));
        ret
    }

    pub fn mk_truncate(&mut self, mut val: Value, bits: u32) -> Value {
        while val.len() as u32 > ((bits+31) / 32) { val.pop(); }

        if bits % 32 != 0 {
            assert!(val.len() > 0);
            let x = val.last_mut().unwrap();

            let r = self.cfg.fresh_var();
            let cst = self.mk_int(32, (1u64 << (bits % 32)) - 1)[0];
            self.stmt.push(Instr::Operation(r, COp::And, vec![*x, cst]));
            *x = r;
        }

        val
    }

    pub fn mk_zext(&mut self, mut val: Value, bits: u32) -> Value {
        while val.len() < ((bits+31) / 32) as usize {
            val.push(self.mk_int(32, 0)[0]);
        }

        val
    }

    pub fn mk_sext(&mut self, mut val: Value, bits: u32) -> Value {
        todo!()
    }

    pub fn mk_gep(&mut self, ptr: Var, ty: &TypeRef, indices: &[Operand]) -> Var {
        let rest = &indices[1..indices.len()];
        let index: Operand = indices[0].clone();

        let size = self.mk_int(32, type_size(self.types, ty) as u64);
        let mut value = self.mk_operand(&index);
        value = self.mk_truncate(value, 32);
        assert!(value.len() == 1);

        let tmp = self.cfg.fresh_var();
        self.stmt.push(Instr::Operation(tmp, COp::Mul, vec![size[0], value[0]]));
        let ret = self.mk_ptr_add(ptr, vec![tmp]);

        return self.mk_ptr_chain(ret, ty, rest);
    }

    pub fn mk_ptr_chain(&mut self, ptr: Var, ty: &TypeRef, indices: &[Operand]) -> Var {
        if indices.len() == 0 { return ptr; }
        let index: Operand = indices[0].clone();
        let rest = &indices[1..indices.len()];

        match ty.as_ref() {
            Type::StructType { element_types, .. } => {
                let layout = StructLayout::new(self.types, &element_types);
                let idx = operand_as_int(index).unwrap();

                let offset =
                    self.mk_int(32, layout.offsets[idx as usize] as u64);
                let ret = self.mk_ptr_add(ptr, offset);

                return
                    self.mk_ptr_chain(ret,
                        &element_types[idx as usize], rest);
            }
            Type::ArrayType { element_type, .. } => {
                let size =
                    self.mk_int(32, type_size(self.types, element_type) as u64)[0];

                let tmp = self.cfg.fresh_var();

                let mut value = self.mk_operand(&index);
                value = self.mk_truncate(value, 32);
                assert!(value.len() == 1);

                self.stmt.push(Instr::Operation(tmp, COp::Mul, vec![size, value[0]]));

                let ret = self.mk_ptr_add(ptr, vec![tmp]);

                return
                    self.mk_ptr_chain(ret,
                        element_type, rest);
            }
            Type::NamedStructType { name } => {
                match self.types.named_struct_def(name).unwrap() {
                    NamedStructDef::Opaque => panic!("Can't do GEP on an opaque struct"),
                    NamedStructDef::Defined(def) =>
                        self.mk_ptr_chain(ptr, def, indices),
                }
            }
            ty => panic!("type {ty} doesn't support getelementptr"),
        }
    }

    pub fn mk_bitcast(&mut self, mut val: Value, words: usize) -> Value {
        while val.len() > words { val.pop(); }
        while val.len() < words {
            val.push(self.mk_int(32, 0)[0]);
        }

        val
    }

    pub fn mk_struct_extract(&mut self, value: Value, offset: usize, bits: usize) -> Value {
        let words = (bits+31) / 32;
        let mut val = vec![];

        for i in 0..words {
            val.push(value[(offset / 4) + i]);
        }

        let error: i32 = (offset % 4) as i32 * 8;

        if error != 0 {
            let cst1 = self.cfg.fresh_var();
            let cst2 = self.cfg.fresh_var();
            self.stmt.push(Instr::Move(cst1, Lit::Int(error)));
            self.stmt.push(Instr::Move(cst2, Lit::Int(32-error)));

            for i in 0..words-1 {
                let x = self.cfg.fresh_var();
                let y = self.cfg.fresh_var();
                let z = self.cfg.fresh_var();
                self.stmt.push(Instr::Operation(x, COp::Srl, vec![val[i], cst1]));
                self.stmt.push(Instr::Operation(y, COp::Sll, vec![val[i+1], cst2]));
                self.stmt.push(Instr::Operation(z, COp::Or, vec![x, y]));
                val[i] = z;
            }

            let x = self.cfg.fresh_var();
            self.stmt.push(Instr::Operation(x, COp::Srl, vec![val[words-1], cst1]));
            val[words-1] = x;

            if bits > 32 * (words-1) + (offset % 4) {
                let y = self.cfg.fresh_var();
                let z = self.cfg.fresh_var();
                let idx = (offset/4) + words;
                self.stmt.push(Instr::Operation(y, COp::Sll, vec![value[idx], cst2]));
                self.stmt.push(Instr::Operation(z, COp::Or, vec![val[words-1], y]));
                val[words-1] = z;
            }
        }

        self.mk_zext(val, bits as u32)
    }

    pub fn mk_constant(&mut self, cst: &ConstantRef) -> Value {
        match cst.as_ref() {
            Constant::Int{ bits, value } =>
                self.mk_int(*bits as usize, *value),
            Constant::Add(op) => {
                let lhs = self.mk_constant(&op.operand0);
                let rhs = self.mk_constant(&op.operand1);
                self.mk_add(lhs, rhs)
            }
            Constant::Sub(op) => {
                let lhs = self.mk_constant(&op.operand0);
                let rhs = self.mk_constant(&op.operand1);
                self.mk_sub(lhs, rhs)
            }
            Constant::Mul(op) => {
                let lhs = self.mk_constant(&op.operand0);
                let rhs = self.mk_constant(&op.operand1);
                self.mk_mul(lhs, rhs)
            }
            Constant::Xor(op) => {
                let lhs = self.mk_constant(&op.operand0);
                let rhs = self.mk_constant(&op.operand1);
                self.mk_bitwise_binop(COp::Xor, lhs, rhs)
            }
            Constant::Trunc(op) => {
                let val: Value = self.mk_constant(&op.operand);
                let bits = if let Type::IntegerType { bits } = op.to_type.as_ref() {bits}
                else { panic!("can only truncate to integers are vectors are not supported!") };
                self.mk_truncate(val, *bits)
            }
            Constant::BitCast(op) => {
                let val: Value = self.mk_constant(&op.operand);
                self.mk_bitcast(val, type_words(self.types, &op.to_type))
            }
            Constant::IntToPtr(op) => {
                let val: Value = self.mk_constant(&op.operand);
                self.mk_bitcast(val, type_words(self.types, &op.to_type))
            }
            Constant::PtrToInt(op) => {
                let val: Value = self.mk_constant(&op.operand);
                self.mk_bitcast(val, type_words(self.types, &op.to_type))
            }
            Constant::AddrSpaceCast(op) => {
                let val: Value = self.mk_constant(&op.operand);
                self.mk_bitcast(val, type_words(self.types, &op.to_type))
            }
            Constant::GetElementPtr(gep) => {
                let addr = self.mk_constant(&gep.address);
                let indices: Vec<Operand> = gep.indices
                    .iter()
                    .map(|cst|
                        Operand::ConstantOperand(cst.clone()))
                    .collect();
                vec![self.mk_gep(addr[0], &gep.indexed_type, &indices)]
            }
            Constant::Null(..) =>
                self.mk_int(32, 0),
            Constant::Undef(ty) =>
                self.mk_undef(type_words(self.types, ty)),
            Constant::Poison(ty) =>
                self.mk_undef(type_words(self.types, ty)),
            Constant::AggregateZero(ty) =>
                self.mk_zero(type_words(self.types, ty)),
            Constant::GlobalReference{name: Name::Name(name), ..} =>
                vec![self.mk_global_reference(name.to_string())],
            Constant::GlobalReference{name: Name::Number(..), ..} =>
                panic!("A global reference must be a string"),
            Constant::BlockAddress => {
                let id = self.current_block.clone().unwrap();
                let val = self.block_address(id) as u64;
                self.mk_int(32, val)
            }
            Constant::Struct {name, values, ..} =>
                todo!(),
            Constant::Array { element_type, elements } =>
                todo!(),
            Constant::TokenNone =>
                todo!(),
            Constant::Vector(..) =>
                panic!("Vectors are not supported"),
            Constant::Float(..) =>
                panic!("Floats are not supported"),
            Constant::ExtractElement( .. ) =>
                panic!("Vectors are not supported"),
            Constant::InsertElement( .. ) =>
                panic!("Vectors are not supported"),
            Constant::ShuffleVector( .. ) =>
                panic!("Vectors are not supported"),
            Constant::PtrAuth { .. } =>
                panic!("Pointer Authentication Code is only supported for Armv8"),
        }
    }

    pub fn mk_operand(&mut self, operand: &Operand) -> Value {
        match operand {
            Operand::LocalOperand { name, ty } =>
                if let Some(op) = self.names.get(name) { op.clone() }
                else { println!("{}", type_size(self.types, ty)); self.new_value(name.clone(), ty.clone()) },
            Operand::ConstantOperand(cst) =>
                self.mk_constant(cst),
            Operand::MetadataOperand =>
                panic!("unsupported operand type")
        }
    }

    pub fn store(&mut self, mut pointer: Var, buf: Value, bytes: usize, align: usize) {
        let kind = match align {
            1 => MemopKind::Unsigned8,
            2 => MemopKind::Unsigned16,
            _ => MemopKind::Word
        };

        let elem_size = if align > 4 {4} else {align};
        let num_elem = match align {
            1 => bytes,
            2 => (bytes+1) / 2,
            _ => (bytes+3) / 4,
        };

        let cst = self.cfg.fresh_var();
        if num_elem > 1 { self.stmt.push(Instr::Move(cst, Lit::Int(elem_size as i32 * 8))); }

        for i in 0..(bytes+3)/4 {
            let mut val = buf[i];

            for j in 0..4/elem_size {
                if i * 4 + j * elem_size >= bytes { break; }

                // Increment the pointer if necessary
                if i != 0 || j != 0 {
                    let cst = self.mk_int(32, elem_size as u64);
                    pointer = self.mk_ptr_add(pointer, cst);
                }

                self.stmt.push(
                    Instr::Store{
                        addr: pointer,
                        volatile: false,
                        kind,
                        val,
                    }
                );

                if j != 0 {
                    let ret = self.cfg.fresh_var();
                    self.stmt.push(Instr::Operation(ret, COp::Srl, vec![val, cst]));
                    val = ret;
                }
            }
        }
    }

    pub fn load(&mut self, mut pointer: Var, bytes: usize, align: usize, signed: bool)
        -> Value {
        let kind = match align {
            1 => if signed {MemopKind::Signed8} else {MemopKind::Unsigned8},
            2 => if signed {MemopKind::Signed16} else {MemopKind::Unsigned16},
            _ => MemopKind::Word
        };

        let elem_size = if align > 4 {4} else {align};

        let mut result = vec![];
        for i in 0..(bytes+3)/4 {

            let mut value = self.cfg.fresh_var();

            for j in 0..4/elem_size {
                if i * 4 + j * elem_size >= bytes { break; }

                // Increment the pointer if necessary
                if i != 0 || j != 0 {
                    let cst = self.mk_int(32, elem_size as u64);
                    pointer = self.mk_ptr_add(pointer, cst);
                }

                let dest = if j == 0 {value} else {self.cfg.fresh_var()};
                self.stmt.push(
                    Instr::Load{
                        addr: pointer,
                        volatile: false,
                        kind,
                        dest,
                    }
                );

                if j != 0 {
                    let tmp = self.cfg.fresh_var();
                    let ret = self.cfg.fresh_var();
                    let cst = self.mk_int(32, (elem_size * j * 8) as u64)[0];
                    self.stmt.push(Instr::Operation(tmp, COp::Srl, vec![dest, cst]));
                    self.stmt.push(Instr::Operation(ret, COp::Or, vec![tmp, value]));
                    value = ret;
                }
            }

            result.push(value);
        }

        return result;
    }

    pub fn mk_phi(&mut self, phi: &instruction::Phi) {
        let mut args: Vec<Vec<(Lit, Label)>> = Vec::new();
        let words: usize = type_words(self.types, &phi.to_type);
        for _ in 0..words { args.push(vec![]); }

        for (operand, block) in phi.incoming_values.iter() {
            let label: Label = self.label(block.clone());
            let op: Value = self.mk_operand(operand);
            assert!(op.len() == words);

            for i in 0..words {
                args[i].push((Lit::Var(op[i]), label));
            }
        }

        let dest: Value = self.new_value(phi.dest.clone(), phi.to_type.clone());

        for (i, arg) in args.into_iter().enumerate() {
            self.stmt.push(Instr::Phi(dest[i], arg));
        }
    }

    pub fn compile_instruction(&mut self, instr: &Instruction) {
        use llvm_ir::instruction::*;
        match instr {
            Instruction::Trunc(op) => {
                let op0: Value = self.mk_operand(&op.operand);
                let bits = type_bits(&op.to_type);
                let ret: Value = self.mk_truncate(op0, bits);
                self.new_value_with(op.dest.clone(), ret);
            }

            Instruction::ZExt(op) => {
                let op0: Value = self.mk_operand(&op.operand);
                let bits = type_bits(&op.to_type);
                let ret: Value = self.mk_zext(op0, bits);
                self.new_value_with(op.dest.clone(), ret);
            }

            Instruction::SExt(op) => {
                let op0: Value = self.mk_operand(&op.operand);
                let bits = type_bits(&op.to_type);
                let ret: Value = self.mk_sext(op0, bits);
                self.new_value_with(op.dest.clone(), ret);
            }

            Instruction::GetElementPtr(gep) => {
                let ptr = self.mk_operand(&gep.address)[0];
                let ret =
                    self.mk_gep(ptr, &gep.indexed_type, gep.indices.as_slice());
                self.new_value_with(gep.dest.clone(), vec![ret]);
            }

            Instruction::Select(op) => {
                let cond = self.mk_operand(&op.condition)[0];

                let ret: Value = self.mk_select(cond,
                    |builder| builder.mk_operand(&op.true_value),
                    |builder| builder.mk_operand(&op.false_value));
                self.new_value_with(op.dest.clone(), ret);
            }

            Instruction::Add(op) => {
                let op0: Value = self.mk_operand(&op.operand0);
                let op1: Value = self.mk_operand(&op.operand1);
                let mut ret: Value = self.mk_add(op0, op1);
                let bits = type_bits(&op.operand0.get_type(self.types));
                if !op.nuw && !op .nsw { ret = self.mk_truncate(ret, bits); }
                self.new_value_with(op.dest.clone(), ret);
            }

            Instruction::Sub(op) => {
                let op0: Value = self.mk_operand(&op.operand0);
                let op1: Value = self.mk_operand(&op.operand1);
                let mut ret: Value = self.mk_sub(op0, op1);
                let bits = type_bits(&op.operand0.get_type(self.types));
                if !op.nuw && !op .nsw { ret = self.mk_truncate(ret, bits); }
                self.new_value_with(op.dest.clone(), ret);
            }

            Instruction::Mul(op) => {
                let op0: Value = self.mk_operand(&op.operand0);
                let op1: Value = self.mk_operand(&op.operand1);
                let mut ret: Value = self.mk_mul(op0, op1);
                let bits = type_bits(&op.operand0.get_type(self.types));
                if !op.nuw && !op .nsw { ret = self.mk_truncate(ret, bits); }
                self.new_value_with(op.dest.clone(), ret);
            }

            Instruction::UDiv(op) => {
                let op0: Value = self.mk_operand(&op.operand0);
                let op1: Value = self.mk_operand(&op.operand1);
                let ret: Value = self.mk_udiv(op0, op1);
                self.new_value_with(op.dest.clone(), ret);
            }

            Instruction::SDiv(op) => {
                let op0: Value = self.mk_operand(&op.operand0);
                let op1: Value = self.mk_operand(&op.operand1);
                let ret: Value = self.mk_sdiv(op0, op1);
                self.new_value_with(op.dest.clone(), ret);
            }

            Instruction::URem(op) => {
                let op0: Value = self.mk_operand(&op.operand0);
                let op1: Value = self.mk_operand(&op.operand1);
                let ret: Value = self.mk_urem(op0, op1);
                self.new_value_with(op.dest.clone(), ret);
            }

            Instruction::SRem(op) => {
                let op0: Value = self.mk_operand(&op.operand0);
                let op1: Value = self.mk_operand(&op.operand1);
                let ret: Value = self.mk_srem(op0, op1);
                self.new_value_with(op.dest.clone(), ret);
            }

            Instruction::And(op) => {
                let op0: Value = self.mk_operand(&op.operand0);
                let op1: Value = self.mk_operand(&op.operand1);
                let ret: Value = self.mk_bitwise_binop(COp::And, op0, op1);
                self.new_value_with(op.dest.clone(), ret);
            }

            Instruction::Or(op) => {
                let op0: Value = self.mk_operand(&op.operand0);
                let op1: Value = self.mk_operand(&op.operand1);
                let ret: Value = self.mk_bitwise_binop(COp::Or, op0, op1);
                self.new_value_with(op.dest.clone(), ret);
            }

            Instruction::Xor(op) => {
                let op0: Value = self.mk_operand(&op.operand0);
                let op1: Value = self.mk_operand(&op.operand1);
                let ret: Value = self.mk_bitwise_binop(COp::Xor, op0, op1);
                self.new_value_with(op.dest.clone(), ret);
            }

            Instruction::ICmp(op) => {
                let op0: Value = self.mk_operand(&op.operand0);
                let op1: Value = self.mk_operand(&op.operand1);
                let ret = self.mk_icmp(op.predicate, op0, op1);
                self.new_value_with(op.dest.clone(), vec![ret]);
            }

            Instruction::Shl(op) => {
                let op0: Value = self.mk_operand(&op.operand0);
                let op1: Value = self.mk_operand(&op.operand1);
                let mut ret: Value = self.mk_sll(op0, op1);
                let bits = type_bits(&op.operand0.get_type(self.types));
                if !op.nuw && !op .nsw { ret = self.mk_truncate(ret, bits); }
                self.new_value_with(op.dest.clone(), ret);
            }

            Instruction::LShr(op) => {
                let op0: Value = self.mk_operand(&op.operand0);
                let op1: Value = self.mk_operand(&op.operand1);
                let ret: Value = self.mk_srl(op0, op1);
                self.new_value_with(op.dest.clone(), ret);
            }

            Instruction::AShr(op) => {
                let op0: Value = self.mk_operand(&op.operand0);
                let op1: Value = self.mk_operand(&op.operand1);
                let ret: Value = self.mk_sra(op0, op1);
                self.new_value_with(op.dest.clone(), ret);
            }

            Instruction::FAdd(..)
                | Instruction::FSub(..)
                | Instruction::FMul(..)
                | Instruction::FDiv(..)
                | Instruction::FRem(..)
                | Instruction::FNeg(..)
                | Instruction::FCmp(..)
                | Instruction::FPExt(..)
                | Instruction::FPToUI(..)
                | Instruction::FPToSI(..)
                | Instruction::UIToFP(..)
                | Instruction::SIToFP(..)
                | Instruction::FPTrunc(..)
                => panic!("floating points are not implemented yet"),

            Instruction::InsertElement(..)
                | Instruction::ShuffleVector(..)
                | Instruction::ExtractElement(..)
                => panic!("vector are not implemented yet"),

            Instruction::ExtractValue(_op) => {
                println!("TODO: add instruction {instr}");
            }

            Instruction::InsertValue(_op) => {
                println!("TODO: add instruction {instr}");
            }

            Instruction::Alloca(_op) => {
                println!("TODO: add instruction {instr}");
            }

            Instruction::Load(op) => {
                let pointer: Var = self.mk_operand(&op.address)[0];
                let ty = op.loaded_ty.get_type(self.types);
                let ret: Value =
                    self.load(
                        pointer,
                        type_bytes(self.types, &ty),
                        type_alignment(self.types, &ty), false);
                self.new_value_with(op.dest.clone(), ret);
            }

            Instruction::Store(op) => {
                let value: Value = self.mk_operand(&op.value);
                let pointer: Var = self.mk_operand(&op.address)[0];
                let ty = op.value.get_type(self.types);
                self.store(
                    pointer,
                    value, type_bytes(self.types, &ty),
                    type_alignment(self.types, &ty));
            }

            Instruction::Fence(_) => {}

            Instruction::CmpXchg(_)
                | Instruction::AtomicRMW(_)
                => panic!("atomic operations are not implemented yet"),

            Instruction::BitCast(op) => {
                let x: Value = self.mk_operand(&op.operand);
                let ret: Value = self.mk_bitcast(x, type_words(self.types, &op.to_type));
                self.new_value_with(op.dest.clone(), ret);
            }

            Instruction::PtrToInt(op) => {
                let x: Value = self.mk_operand(&op.operand);
                let ret: Value = self.mk_bitcast(x, type_words(self.types, &op.to_type));
                self.new_value_with(op.dest.clone(), ret);
            }

            Instruction::IntToPtr(op) => {
                let x: Value = self.mk_operand(&op.operand);
                let ret: Value = self.mk_bitcast(x, type_words(self.types, &op.to_type));
                self.new_value_with(op.dest.clone(), ret);
            }

            Instruction::Phi(phi) => self.mk_phi(phi),

            Instruction::Call(call) => {
                let mut args = vec![];
                for (arg, _) in call.arguments.iter() {
                    args.extend_from_slice(&self.mk_operand(arg));
                }

                let op = match &call.function {
                    Either::Left(_) => panic!("inline assembly is not supported"),
                    Either::Right(op) => op,
                };

                let ret = self.cfg.fresh_var();
                let mut indirect = true;

                if let Operand::ConstantOperand(cst) = op {
                    if let Constant::GlobalReference { name, .. } = cst.as_ref() {
                        if let Name::Name(funname) = name {
                            self.stmt.push(Instr::Call(ret, funname.as_ref().clone(), args));
                            indirect = false;
                        }
                    }
                }

                if indirect {
                    panic!("indirect calls are not supported yet");
                }

                if let Some(dest) = call.dest.clone() {
                    self.new_value_with(dest, vec![ret]);
                }
            }

            _ => {
                println!("TODO: add {}", instr);
            }

        }
    }

    pub fn mk_custom_intrinsic(&mut self, name: &str, words: usize, mut args: Value) -> Value {
        let slot = self.cfg.fresh_stack_var(words, 4);

        let pointer = self.cfg.fresh_var();
        self.stmt.push(Instr::Move(pointer, Lit::Stack(slot)));
        args.insert(0, pointer);

        let _x = self.cfg.fresh_var();
        self.stmt.push(Instr::Call(_x, name.to_string(), args));
        self.load(pointer, words * 4, 16, false)
    }


    pub fn mk_sll(&mut self, mut lhs: Value, rhs: Value) -> Value {
        if lhs.len() != 1 {
            lhs.extend(&rhs);
            lhs.insert(0, self.mk_int(32, rhs.len() as u64)[0]);
            return self.mk_custom_intrinsic("__sll", rhs.len()+1, lhs);
        }

        let ret = self.cfg.fresh_var();
        self.stmt.push(Instr::Operation(ret, COp::Sll, vec![lhs[0], rhs[0]]));
        vec![ret]
    }

    pub fn mk_sra(&mut self, mut lhs: Value, rhs: Value) -> Value {
        if lhs.len() != 1 {
            lhs.extend(&rhs);
            lhs.insert(0, self.mk_int(32, rhs.len() as u64)[0]);
            return self.mk_custom_intrinsic("__sra", rhs.len()+1, lhs);
        }

        let ret = self.cfg.fresh_var();
        self.stmt.push(Instr::Operation(ret, COp::Sra, vec![lhs[0], rhs[0]]));
        vec![ret]
    }

    pub fn mk_srl(&mut self, mut lhs: Value, rhs: Value) -> Value {
        if lhs.len() != 1 {
            lhs.extend(&rhs);
            lhs.insert(0, self.mk_int(32, rhs.len() as u64)[0]);
            return self.mk_custom_intrinsic("__srl", rhs.len()+1, lhs);
        }

        let ret = self.cfg.fresh_var();
        self.stmt.push(Instr::Operation(ret, COp::Srl, vec![lhs[0], rhs[0]]));
        vec![ret]
    }

    pub fn mk_udiv(&mut self, mut lhs: Value, rhs: Value) -> Value {
        lhs.extend(&rhs);
        lhs.insert(0, self.mk_int(32, rhs.len() as u64)[0]);
        return self.mk_custom_intrinsic("__udiv", rhs.len()+1, lhs);
    }

    pub fn mk_sdiv(&mut self, mut lhs: Value, rhs: Value) -> Value {
        lhs.extend(&rhs);
        lhs.insert(0, self.mk_int(32, rhs.len() as u64)[0]);
        return self.mk_custom_intrinsic("__sdiv", rhs.len()+1, lhs);
    }

    pub fn mk_urem(&mut self, mut lhs: Value, rhs: Value) -> Value {
        lhs.extend(&rhs);
        lhs.insert(0, self.mk_int(32, rhs.len() as u64)[0]);
        return self.mk_custom_intrinsic("__urem", rhs.len()+1, lhs);
    }

    pub fn mk_srem(&mut self, mut lhs: Value, rhs: Value) -> Value {
        lhs.extend(&rhs);
        lhs.insert(0, self.mk_int(32, rhs.len() as u64)[0]);
        return self.mk_custom_intrinsic("__srem", rhs.len()+1, lhs);
    }

    pub fn compile_terminator(&mut self, from: Label, term: &Terminator) {
        use llvm_ir::terminator::*;

        match term {
            Terminator::Ret(op) => {
                match &op.return_operand {
                    None => {
                        let zero = self.mk_int(32, 0)[0];
                        self.stmt.push(Instr::Return(zero));
                        self.finish_block();
                    }

                    Some(val) => {
                        let value = self.mk_operand(&val);
                        self.stmt.push(Instr::Return(value[0]));
                        self.finish_block();
                    }
                }
            }

            Terminator::Br(op) => {
                let label = self.label(op.dest.clone());
                self.exits.insert((from, label), self.label);

                self.stmt.push(Instr::Jump(label));
                self.finish_block();
            }

            Terminator::CondBr(op) => {
                let cond = self.mk_operand(&op.condition)[0];
                let l1 = self.label(op.true_dest.clone());
                let l2 = self.label(op.false_dest.clone());
                self.exits.insert((from, l1), self.label);
                self.exits.insert((from, l2), self.label);

                self.stmt.push(Instr::Branch(CCond::Nez, vec![cond], l1, l2));
                self.finish_block();
            }

            Terminator::Unreachable(..) => {
                let zero = self.mk_int(32, 0)[0];
                self.stmt.push(Instr::Return(zero));
                self.finish_block();
            }

            Terminator::IndirectBr(op) => {
                let id = self.mk_operand(&op.operand)[0];

                for (i, dest) in op.possible_dests.iter().enumerate() {
                    let l = self.label(dest.clone());
                    self.exits.insert((from, l), self.label);
                    let v = self.block_address(dest.clone());

                    if i == op.possible_dests.len() - 1 {
                        self.stmt.push(Instr::Jump(l));
                        self.finish_block();
                    } else {
                        let tmp = self.cfg.fresh_var();
                        let next = self.cfg.fresh_label();
                        let val = self.mk_int(32, v as u64)[0];
                        self.stmt.push(Instr::Operation(tmp, COp::Sub, vec![id, val]));
                        self.stmt.push(Instr::Branch(CCond::Nez, vec![tmp], next, l));
                        self.finish_block();
                        self.label = next;
                    }
                }
            }

            Terminator::Switch(op) => {
                let id = self.mk_operand(&op.operand);
                let bits = type_bits(&op.operand.get_type(self.types));

                for (cst, dest) in op.dests.iter() {
                    let value = constant_as_int(cst.clone()).unwrap();
                    let l = self.label(dest.clone());
                    self.exits.insert((from, l), self.label);

                    let next = self.cfg.fresh_label();
                    let val = self.mk_int(bits as usize, value);
                    let tmp = self.mk_eq(id.clone(), val);

                    self.stmt.push(Instr::Branch(CCond::Nez, vec![tmp], l, next));
                    self.finish_block();
                    self.label = next;
                }

                let l = self.label(op.default_dest.clone());
                self.exits.insert((from, l), self.label);
                self.stmt.push(Instr::Jump(l));
                self.finish_block();
            }

            Terminator::Invoke(_op) => {
                println!("TODO: implement invoke");
            }

            Terminator::Resume(_op) => {
                println!("TODO: implement resume");
            }

            Terminator::CleanupRet(_op) => {
                println!("TODO: implement cleanupret");
            }

            Terminator::CatchRet(_op) => {
                println!("TODO: implement catchret");
            }

            Terminator::CatchSwitch(_op) => {
                println!("TODO: implement catchswitch");
            }

            Terminator::CallBr(_op) =>
                panic!("inline assembly is unsupported"),
        }
    }

    pub fn cfg(mut self) -> Cfg<COp, CCond> {
        let mut cfg: Cfg<COp, CCond> =
            std::mem::replace(&mut self.cfg, Cfg::new(true));

        for label in cfg.labels() {
            let mut block =
                cfg[label].stmt.clone();

            for instr in block.iter_mut() {
                if let Instr::Phi(_, args) = instr {
                    for (_, src) in args.iter_mut() {
                        if let Some(new_src) = self.exits.get(&(*src, label)) {
                            *src = *new_src;
                        }
                    }
                }
            }

            cfg.set_block_stmt(label, block);
        }

        cfg
    }
}


pub fn run() {
    let module =
        module::Module::from_bc_path("build/main.bc").unwrap();

    println!("Module name: {:?}", module.name);

    for var in module.global_vars.iter() {
        println!("variable: {}", var.name);
        println!("  type: {}", var.ty.as_ref());
        println!("  {:?}", var.initializer);

        //let mut builder = CfgBuilder::new(&module);
        //if let Some(cst) = var.initializer.clone() {
        //    builder.mk_constant(&cst);
        //}
    }

    for fun in module.functions.iter() {
        if fun.basic_blocks.len() == 0 { continue; }
        println!("\n\n\n====================== {} ======================", fun.name);

        let mut builder = CfgBuilder::new(&module);
        let begin_fun = builder.label(fun.basic_blocks[0].name.clone());
        builder.stmt.push(Instr::Jump(begin_fun));
        builder.finish_block();

        for arg in fun.parameters.iter() {
            builder.add_parameter(arg.clone());
        }

        for block in fun.basic_blocks.iter() {
            let begin = builder.label(block.name.clone());
            builder.current_block = Some(block.name.clone());
            builder.label = begin;

            for inst in block.instrs.iter() {
                //println!("{}", inst);
                builder.compile_instruction(inst);
            }

            let term = &block.term;

            //println!("{term}");
            builder.compile_terminator(begin, &term);
        }

        println!("cfg: \n{}", builder.cfg());
    }
}
