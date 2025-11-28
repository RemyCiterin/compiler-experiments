//! TODO:
//!     - alloca
//!     - insertvalue
//!     - extractvalue
//!     - constant struct
//!     - constant array
//!     - non-string global reference
//!     - initialize global variables
//!     - llvm intrisics
//!     - exceptions handling

use llvm_ir::module::Module;
use llvm_ir::predicates::*;
use llvm_ir::constant::*;
use llvm_ir::types::*;
use llvm_ir::*;

use either::*;

use super::*;
use std::collections::HashMap;


pub fn constant_as_int(cst: ConstantRef) -> Option<u64> {
    if let Constant::Int { value, .. } = cst.as_ref() { Some(*value) }
    else { None }
}

pub fn operand_as_int(op: Operand) -> Option<u64> {
    if let Operand::ConstantOperand(cst) = op { constant_as_int(cst) }
    else { None }
}

pub fn unsigned(bits: usize) -> TYPE {
    return TYPE{bits};
}

pub fn type_repr(types: &Types, ty: &TypeRef) -> TYPE {
    return TYPE{
        bits: type_bits(types, ty),
    };
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

pub fn type_bits(types: &Types, ty: &TypeRef) -> usize {
    match ty.as_ref() {
        Type::VoidType => 0,
        Type::LabelType => 32,
        Type::FuncType { .. } => 32,
        Type::PointerType { .. } => 32,
        Type::IntegerType { bits } => *bits as usize,
        Type::ArrayType { element_type, num_elements } =>
            *num_elements as usize * type_bits(types, &element_type),
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

pub fn type_size(types: &Types, ty: &TypeRef) -> usize {
    return (type_bits(types, ty) + 7) / 8;
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
            let s = type_bits(types, field) * 8;

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
            let s = type_bits(types, field) * 8;

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

pub struct CfgBuilder<'a> {
    // Control flow raph under construction
    cfg: Builder,

    // Map from llvm labels to CFG input/output labels
    labels: HashMap<Name, Label>,

    // map from names to values
    names: HashMap<Name, Ref>,

    types: &'a Types,

    module: &'a Module,

    exits: HashMap<(Label, Label), Label>,

    /// A map used to associate to each label a unique integer, used for indirect branches
    blocks_addresses: HashMap<Name, i32>,

    /// A counter used to associate to each label a unique integer, used to indirect branches
    next_block_address: i32,

    /// Return the name of the current LLVM block, used to compute `Constant::BlockAddress`
    current_block: Option<Name>,
}


impl<'a> CfgBuilder<'a> {
    pub fn new(module: &'a Module) -> Self {
        let cfg = Builder::new();

        Self {
            cfg,
            module,
            types: &module.types,
            labels: HashMap::new(),
            names: HashMap::new(),
            exits: HashMap::new(),
            blocks_addresses: HashMap::new(),
            next_block_address: 0,
            current_block: None,
        }
    }

    pub fn add_parameter(&mut self, parameter: llvm_ir::function::Parameter) {
        let var = self.cfg.fresh_arg(type_repr(self.types, &parameter.ty));
        self.new_value_with(parameter.name, var);
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
    pub fn new_value(&mut self, name: Name, ty: TypeRef) -> Ref {
        if let Some(v) = self.names.get(&name) { return v.clone(); }

        let var = self.cfg.fresh_ref(type_repr(self.types, &ty));
        self.names.insert(name, var);
        return var;
    }

    /// Each LLVM register of type `ty` is represented with a vector of `type_words(&ty)` CFG
    /// variables, this function create a variables for the register `name` of type `ty` with the
    /// given vector of CFG variables, of perform a set of move instructions if the `register` is
    /// already defined.
    pub fn new_value_with(&mut self, name: Name, v: Ref) {
        if let Some(old) = self.names.get(&name) {
            // Copy the new value into old
            let ty = self.cfg[*old];
            assert!(self.cfg[*old] == self.cfg[v]);
            self.cfg.push(Instr::Move{ ty, dest: *old, value: v });

            return;
        }

        self.names.insert(name, v);
    }

    pub fn mk_select<F1, F2>(&mut self, cond: Ref, f1: F1, f2: F2) -> Ref where
        F1: FnOnce(&mut Self) -> Ref, F2: FnOnce(&mut Self) -> Ref {
        let t_begin = self.cfg.fresh_label();
        let e_begin = self.cfg.fresh_label();
        let join = self.cfg.fresh_label();

        self.cfg.push(Instr::Branch(cond, t_begin, e_begin));
        self.cfg.finish_block();

        self.cfg.label = t_begin;
        let v1 = f1(self);
        let t_end = self.cfg.label;
        self.cfg.stmt.push(Instr::Jump(join));
        self.cfg.finish_block();

        self.cfg.label = e_begin;
        let v2 = f2(self);
        let e_end = self.cfg.label;
        self.cfg.stmt.push(Instr::Jump(join));
        self.cfg.finish_block();

        assert!(self.cfg.type_of(v1) == self.cfg.type_of(v2));

        self.cfg.label = join;
        let ty = self.cfg.type_of(v1);
        let dest = self.cfg.fresh_ref(self.cfg.type_of(v1));
        self.cfg.push(Instr::Phi{dest, ty, args: vec![(v1, t_end), (v2, e_end)]});
        dest
    }

    pub fn value(&mut self, name: Name) -> Ref {
        self.names[&name]
    }

    pub fn finish_block(&mut self) {
        self.cfg.finish_block();
    }

    /// Generate a constant unsigned integer of a given size
    pub fn mk_uint(&mut self, bits: usize, int: usize) -> Ref {
        let ty = unsigned(bits);
        let dest = self.cfg.fresh_ref(ty);

        self.cfg.push(Instr::Constant{ ty, dest, value: int as usize });
        dest
    }

    pub fn mk_truncate(&mut self, value: Ref, bits: usize) -> Ref {
        let ty = unsigned(bits);
        let dest = self.cfg.fresh_ref(ty);

        self.cfg.push(Instr::GetBits{dest, ty, value, start: 0});

        dest
    }

    /// Perform an addition of arbitrary size integers
    pub fn mk_ptr_add(&mut self, ptr: Ref, offset: Ref) -> Ref {
        let off = self.mk_truncate(offset, 32);
        let dest = self.cfg.fresh_ref(self.cfg[ptr]);

        self.cfg.push(
            Instr::Binop{ dest, ty: unsigned(32), lhs: ptr, rhs: off, binop: Binop::PtrAdd });
        dest
    }

    pub fn mk_icmp(&mut self, cmp: IntPredicate, lhs: Ref, rhs: Ref) -> Ref {
        let ty = BOOL;
        let dest = self.cfg.fresh_ref(ty);
        match cmp {
            IntPredicate::EQ =>
                self.cfg.push(Instr::Binop{ ty, binop: Binop::Equal, dest, lhs, rhs }),
            IntPredicate::NE =>
                self.cfg.push(Instr::Binop{ ty, binop: Binop::Neq, dest, lhs, rhs }),
            IntPredicate::SGT =>
                self.cfg.push(Instr::Binop{ ty, binop: Binop::Slt, dest, lhs: rhs, rhs: lhs }),
            IntPredicate::SGE =>
                self.cfg.push(Instr::Binop{ ty, binop: Binop::Sle, dest, lhs: rhs, rhs: lhs }),
            IntPredicate::SLT =>
                self.cfg.push(Instr::Binop{ ty, binop: Binop::Slt, dest, lhs, rhs }),
            IntPredicate::SLE =>
                self.cfg.push(Instr::Binop{ ty, binop: Binop::Sle, dest, lhs, rhs }),
            IntPredicate::UGT =>
                self.cfg.push(Instr::Binop{ ty, binop: Binop::Ult, dest, lhs: rhs, rhs: lhs }),
            IntPredicate::UGE =>
                self.cfg.push(Instr::Binop{ ty, binop: Binop::Ule, dest, lhs: rhs, rhs: lhs }),
            IntPredicate::ULT =>
                self.cfg.push(Instr::Binop{ ty, binop: Binop::Ult, dest, lhs, rhs }),
            IntPredicate::ULE =>
                self.cfg.push(Instr::Binop{ ty, binop: Binop::Ule, dest, lhs, rhs }),
        }

        dest
    }

    pub fn mk_undef(&mut self, bits: usize) -> Ref {
        self.mk_uint(bits, 0)
    }

    pub fn mk_zero(&mut self, bits: usize) -> Ref {
        self.mk_uint(bits, 0)
    }

    pub fn mk_global_reference(&mut self, name: String) -> Ref {
        let dest = self.cfg.fresh_ref(unsigned(32));
        self.cfg.push(Instr::Symbol{ dest, ty: unsigned(32), value: name });
        dest
    }

    pub fn mk_zext(&mut self, value: Ref, bits: usize) -> Ref {
        let dest = self.cfg.fresh_ref(unsigned(bits));
        self.cfg.push(Instr::ZExt{ dest, ty: unsigned(bits), value });
        dest
    }

    pub fn mk_sext(&mut self, value: Ref, bits: usize) -> Ref {
        let dest = self.cfg.fresh_ref(unsigned(bits));
        self.cfg.push(Instr::SExt{ dest, ty: unsigned(bits), value });
        dest
    }

    pub fn mk_gep(&mut self, ptr: Ref, ty: &TypeRef, indices: &[Operand]) -> Ref {
        let rest = &indices[1..indices.len()];
        let index: Operand = indices[0].clone();

        let size = self.mk_uint(32, type_size(self.types, ty));
        let mut value = self.mk_operand(&index);
        value = self.mk_truncate(value, 32);

        let tmp = self.cfg.fresh_ref(unsigned(32));
        self.cfg.push(
            Instr::Binop{ binop: Binop::Mul, dest: tmp, ty: unsigned(32), lhs: size, rhs: value});
        let ret = self.mk_ptr_add(ptr, tmp);

        return self.mk_ptr_chain(ret, ty, rest);
    }

    pub fn mk_ptr_chain(&mut self, ptr: Ref, ty: &TypeRef, indices: &[Operand]) -> Ref {
        if indices.len() == 0 { return ptr; }
        let index: Operand = indices[0].clone();
        let rest = &indices[1..indices.len()];

        match ty.as_ref() {
            Type::StructType { element_types, .. } => {
                let layout = StructLayout::new(self.types, &element_types);
                let idx = operand_as_int(index).unwrap();

                let offset =
                    self.mk_uint(32, layout.offsets[idx as usize]);
                let ret = self.mk_ptr_add(ptr, offset);

                return
                    self.mk_ptr_chain(ret,
                        &element_types[idx as usize], rest);
            }
            Type::ArrayType { element_type, .. } => {
                let size =
                    self.mk_uint(32, type_size(self.types, element_type));

                let tmp = self.cfg.fresh_ref(unsigned(32));

                let mut value = self.mk_operand(&index);
                value = self.mk_truncate(value, 32);

                self.cfg.push(
                    Instr::Binop{binop: Binop::Mul, dest: tmp, ty: unsigned(32), lhs: size, rhs: value});

                let ret = self.mk_ptr_add(ptr, tmp);

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

    pub fn mk_constant(&mut self, cst: &ConstantRef) -> Ref {
        let ty = type_repr(self.types, &cst.get_type(self.types));
        match cst.as_ref() {
            Constant::Int{ bits, value } =>
                self.mk_uint(*bits as usize, *value as usize),
            Constant::Add(op) => {
                let dest = self.cfg.fresh_ref(ty);
                let lhs = self.mk_constant(&op.operand0);
                let rhs = self.mk_constant(&op.operand1);
                self.cfg.push(Instr::Binop{ binop: Binop::Add, dest, ty, lhs, rhs });
                dest
            }
            Constant::Sub(op) => {
                let dest = self.cfg.fresh_ref(ty);
                let lhs = self.mk_constant(&op.operand0);
                let rhs = self.mk_constant(&op.operand1);
                self.cfg.push(Instr::Binop{ binop: Binop::Sub, dest, ty, lhs, rhs });
                dest
            }
            Constant::Mul(op) => {
                let dest = self.cfg.fresh_ref(ty);
                let lhs = self.mk_constant(&op.operand0);
                let rhs = self.mk_constant(&op.operand1);
                self.cfg.push(Instr::Binop{ binop: Binop::Mul, dest, ty, lhs, rhs });
                dest
            }
            Constant::Xor(op) => {
                let dest = self.cfg.fresh_ref(ty);
                let lhs = self.mk_constant(&op.operand0);
                let rhs = self.mk_constant(&op.operand1);
                self.cfg.push(Instr::Binop{ binop: Binop::Xor, dest, ty, lhs, rhs });
                dest
            }
            Constant::Trunc(op) => {
                let val: Ref = self.mk_constant(&op.operand);
                let bits = if let Type::IntegerType { bits } = op.to_type.as_ref() {bits}
                else { panic!("can only truncate to integers are vectors are not supported!") };
                self.mk_truncate(val, *bits as usize)
            }
            Constant::BitCast(op) => {
                self.mk_constant(&op.operand)
            }
            Constant::IntToPtr(op) => {
                self.mk_constant(&op.operand)
            }
            Constant::PtrToInt(op) => {
                self.mk_constant(&op.operand)
            }
            Constant::AddrSpaceCast(op) => {
                self.mk_constant(&op.operand)
            }
            Constant::GetElementPtr(gep) => {
                let addr = self.mk_constant(&gep.address);
                let indices: Vec<Operand> = gep.indices
                    .iter()
                    .map(|cst|
                        Operand::ConstantOperand(cst.clone()))
                    .collect();
                self.mk_gep(addr, &gep.indexed_type, &indices)
            }
            Constant::Null(..) =>
                self.mk_uint(32, 0),
            Constant::Undef(ty) =>
                self.mk_undef(type_bits(self.types, ty)),
            Constant::Poison(ty) =>
                self.mk_undef(type_bits(self.types, ty)),
            Constant::AggregateZero(ty) =>
                self.mk_zero(type_bits(self.types, ty)),
            Constant::GlobalReference{name: Name::Name(name), ..} =>
                self.mk_global_reference(name.to_string()),
            Constant::GlobalReference{name, ..} => {
                println!("name: {name}");
                let alias = self.module.get_global_var_by_name(name);
                //self.mk_constant(&alias.unwrap().)
                println!("{:?}", alias);
                //panic!()
                self.mk_uint(32, 0)
            }
            Constant::BlockAddress => {
                let id = self.current_block.clone().unwrap();
                let val = self.block_address(id) as usize;
                self.mk_uint(32, val)
            }
            Constant::Struct {name, values, ..} => {
                println!("TODO: add struct constants");
                self.mk_uint(32, 0)
            }
            Constant::Array { element_type, elements } => {
                println!("TODO: add array constants");
                self.mk_uint(32, 0)
            }
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

    pub fn mk_operand(&mut self, operand: &Operand) -> Ref {
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

    pub fn mk_phi(&mut self, phi: &instruction::Phi) {
        let ty: TYPE = type_repr(self.types, &phi.to_type);
        let dest: Ref = self.new_value(phi.dest.clone(), phi.to_type.clone());

        let mut args = vec![];
        for (operand, block) in phi.incoming_values.iter() {
            let label: Label = self.label(block.clone());
            let op: Ref = self.mk_operand(operand);

            args.push((op, label));
        }

        self.cfg.push(Instr::Phi{ dest, ty, args });
    }

    pub fn compile_instruction(&mut self, instr: &Instruction) {
        use llvm_ir::instruction::*;
        match instr {
            Instruction::Trunc(op) => {
                let op0: Ref = self.mk_operand(&op.operand);
                let bits = type_bits(self.types, &op.to_type);
                let ret: Ref = self.mk_truncate(op0, bits);
                self.new_value_with(op.dest.clone(), ret);
            }

            Instruction::ZExt(op) => {
                let op0: Ref = self.mk_operand(&op.operand);
                let bits = type_bits(self.types, &op.to_type);
                let ret: Ref = self.mk_zext(op0, bits);
                self.new_value_with(op.dest.clone(), ret);
            }

            Instruction::SExt(op) => {
                let op0: Ref = self.mk_operand(&op.operand);
                let to = type_bits(self.types, &op.to_type);
                let ret: Ref = self.mk_sext(op0, to);
                self.new_value_with(op.dest.clone(), ret);
            }

            Instruction::GetElementPtr(gep) => {
                let ptr = self.mk_operand(&gep.address);
                let ret =
                    self.mk_gep(ptr, &gep.indexed_type, gep.indices.as_slice());
                self.new_value_with(gep.dest.clone(), ret);
            }

            Instruction::Select(op) => {
                let cond = self.mk_operand(&op.condition);

                let ret: Ref = self.mk_select(cond,
                    |builder| builder.mk_operand(&op.true_value),
                    |builder| builder.mk_operand(&op.false_value));
                self.new_value_with(op.dest.clone(), ret);
            }

            Instruction::Add(op) => {
                let ty = type_repr(self.types, &op.operand0.get_type(&self.types));
                let lhs: Ref = self.mk_operand(&op.operand0);
                let rhs: Ref = self.mk_operand(&op.operand1);
                let dest = self.cfg.fresh_ref(ty);

                self.cfg.push(Instr::Binop{ dest, binop: Binop::Add, ty, lhs, rhs });
                self.new_value_with(op.dest.clone(), dest);
            }

            Instruction::Sub(op) => {
                let ty = type_repr(self.types, &op.operand0.get_type(&self.types));
                let lhs: Ref = self.mk_operand(&op.operand0);
                let rhs: Ref = self.mk_operand(&op.operand1);
                let dest = self.cfg.fresh_ref(ty);

                self.cfg.push(Instr::Binop{ dest, ty, lhs, rhs, binop: Binop::Sub });
                self.new_value_with(op.dest.clone(), dest);
            }

            Instruction::Mul(op) => {
                let ty = type_repr(self.types, &op.operand0.get_type(&self.types));
                let lhs: Ref = self.mk_operand(&op.operand0);
                let rhs: Ref = self.mk_operand(&op.operand1);
                let dest = self.cfg.fresh_ref(ty);

                self.cfg.push(Instr::Binop{ dest, ty, lhs, rhs, binop: Binop::Mul });
                self.new_value_with(op.dest.clone(), dest);
            }

            Instruction::UDiv(op) => {
                let ty = type_repr(self.types, &op.operand0.get_type(&self.types));
                let lhs: Ref = self.mk_operand(&op.operand0);
                let rhs: Ref = self.mk_operand(&op.operand1);
                let dest = self.cfg.fresh_ref(ty);

                self.cfg.push(Instr::Binop{ dest, ty, lhs, rhs, binop: Binop::UDiv });
                self.new_value_with(op.dest.clone(), dest);
            }

            Instruction::SDiv(op) => {
                let ty = type_repr(self.types, &op.operand0.get_type(&self.types));
                let lhs: Ref = self.mk_operand(&op.operand0);
                let rhs: Ref = self.mk_operand(&op.operand1);
                let dest = self.cfg.fresh_ref(ty);

                self.cfg.push(Instr::Binop{ dest, ty, lhs, rhs, binop: Binop::SDiv });
                self.new_value_with(op.dest.clone(), dest);
            }

            Instruction::URem(op) => {
                let ty = type_repr(self.types, &op.operand0.get_type(&self.types));
                let lhs: Ref = self.mk_operand(&op.operand0);
                let rhs: Ref = self.mk_operand(&op.operand1);
                let dest = self.cfg.fresh_ref(ty);

                self.cfg.push(Instr::Binop{ dest, ty, lhs, rhs, binop: Binop::URem });
                self.new_value_with(op.dest.clone(), dest);
            }

            Instruction::SRem(op) => {
                let ty = type_repr(self.types, &op.operand0.get_type(&self.types));
                let lhs: Ref = self.mk_operand(&op.operand0);
                let rhs: Ref = self.mk_operand(&op.operand1);
                let dest = self.cfg.fresh_ref(ty);

                self.cfg.push(Instr::Binop{ dest, ty, lhs, rhs, binop: Binop::SRem });
                self.new_value_with(op.dest.clone(), dest);
            }

            Instruction::And(op) => {
                let ty = type_repr(self.types, &op.operand0.get_type(&self.types));
                let lhs: Ref = self.mk_operand(&op.operand0);
                let rhs: Ref = self.mk_operand(&op.operand1);
                let dest = self.cfg.fresh_ref(ty);

                self.cfg.push(Instr::Binop{ dest, ty, lhs, rhs, binop: Binop::And });
                self.new_value_with(op.dest.clone(), dest);
            }

            Instruction::Or(op) => {
                let ty = type_repr(self.types, &op.operand0.get_type(&self.types));
                let lhs: Ref = self.mk_operand(&op.operand0);
                let rhs: Ref = self.mk_operand(&op.operand1);
                let dest = self.cfg.fresh_ref(ty);

                self.cfg.push(Instr::Binop{ dest, ty, lhs, rhs, binop: Binop::Or });
                self.new_value_with(op.dest.clone(), dest);
            }

            Instruction::Xor(op) => {
                let ty = type_repr(self.types, &op.operand0.get_type(&self.types));
                let lhs: Ref = self.mk_operand(&op.operand0);
                let rhs: Ref = self.mk_operand(&op.operand1);
                let dest = self.cfg.fresh_ref(ty);

                self.cfg.push(Instr::Binop{ dest, ty, lhs, rhs, binop: Binop::Xor });
                self.new_value_with(op.dest.clone(), dest);
            }

            Instruction::ICmp(op) => {
                let op0: Ref = self.mk_operand(&op.operand0);
                let op1: Ref = self.mk_operand(&op.operand1);
                let ret = self.mk_icmp(op.predicate, op0, op1);
                self.new_value_with(op.dest.clone(), ret);
            }

            Instruction::Shl(op) => {
                let ty = type_repr(self.types, &op.operand0.get_type(&self.types));
                let lhs: Ref = self.mk_operand(&op.operand0);
                let rhs: Ref = self.mk_operand(&op.operand1);
                let dest = self.cfg.fresh_ref(ty);

                self.cfg.push(Instr::Binop{ dest, ty, lhs, rhs, binop: Binop::Sll });
                self.new_value_with(op.dest.clone(), dest);
            }

            Instruction::LShr(op) => {
                let ty = type_repr(self.types, &op.operand0.get_type(&self.types));
                let lhs: Ref = self.mk_operand(&op.operand0);
                let rhs: Ref = self.mk_operand(&op.operand1);
                let dest = self.cfg.fresh_ref(ty);

                self.cfg.push(Instr::Binop{ dest, ty, lhs, rhs, binop: Binop::Srl });
                self.new_value_with(op.dest.clone(), dest);
            }

            Instruction::AShr(op) => {
                let ty = type_repr(self.types, &op.operand0.get_type(&self.types));
                let lhs: Ref = self.mk_operand(&op.operand0);
                let rhs: Ref = self.mk_operand(&op.operand1);
                let dest = self.cfg.fresh_ref(ty);

                self.cfg.push(Instr::Binop{ dest, ty, lhs, rhs, binop: Binop::Sra });
                self.new_value_with(op.dest.clone(), dest);
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
                let align = type_alignment(self.types, &op.loaded_ty.get_type(self.types));
                let ty = type_repr(self.types, &op.loaded_ty.get_type(self.types));
                let pointer: Ref = self.mk_operand(&op.address);
                let dest = self.cfg.fresh_ref(ty);

                self.cfg.push(Instr::Load{ dest, ty, addr: pointer, volatile: false, align });
                self.new_value_with(op.dest.clone(), dest);
            }

            Instruction::Store(op) => {
                let value: Ref = self.mk_operand(&op.value);
                let pointer: Ref = self.mk_operand(&op.address);
                let align = type_alignment(self.types, &op.address.get_type(self.types));
                self.cfg.push(Instr::Store{ val: value, addr: pointer, volatile: false, align });
            }

            Instruction::Fence(_)
                | Instruction::CmpXchg(_)
                | Instruction::AtomicRMW(_)
                => panic!("atomic operations are not implemented yet"),

            Instruction::BitCast(op) => {
                let x: Ref = self.mk_operand(&op.operand);
                self.new_value_with(op.dest.clone(), x);
            }

            Instruction::PtrToInt(op) => {
                let x: Ref = self.mk_operand(&op.operand);
                self.new_value_with(op.dest.clone(), x);
            }

            Instruction::IntToPtr(op) => {
                let x: Ref = self.mk_operand(&op.operand);
                self.new_value_with(op.dest.clone(), x);
            }

            Instruction::Phi(phi) => self.mk_phi(phi),

            Instruction::Call(call) => {
                let mut args = vec![];
                for (arg, _) in call.arguments.iter() {
                    args.push(self.mk_operand(arg));
                }

                let op = match &call.function {
                    Either::Left(_) => panic!("inline assembly is not supported"),
                    Either::Right(op) => op,
                };

                let func = self.mk_operand(&op);

                let ty = match call.function_ty.as_ref() {
                    Type::FuncType { result_type, .. } =>
                        type_repr(self.types, result_type),
                    _ => panic!("function call without a function type"),
                };

                let ret = self.cfg.fresh_ref(ty);
                self.cfg.push(Instr::Call { dest: ret, ty, func, args });

                if let Some(dest) = call.dest.clone() {
                    self.new_value_with(dest, ret);
                }
            }

            _ => {
                println!("TODO: add {}", instr);
            }

        }
    }

    pub fn compile_terminator(&mut self, from: Label, term: &Terminator) {
        use llvm_ir::terminator::*;

        match term {
            Terminator::Ret(op) => {
                match &op.return_operand {
                    None => {
                        let zero = self.mk_uint(32, 0);
                        self.cfg.push(Instr::Return(zero));
                        self.finish_block();
                    }

                    Some(val) => {
                        let value = self.mk_operand(&val);
                        self.cfg.push(Instr::Return(value));
                        self.finish_block();
                    }
                }
            }

            Terminator::Br(op) => {
                let label = self.label(op.dest.clone());
                self.exits.insert((from, label), self.cfg.label);

                self.cfg.push(Instr::Jump(label));
                self.finish_block();
            }

            Terminator::CondBr(op) => {
                let cond = self.mk_operand(&op.condition);
                let l1 = self.label(op.true_dest.clone());
                let l2 = self.label(op.false_dest.clone());
                self.exits.insert((from, l1), self.cfg.label);
                self.exits.insert((from, l2), self.cfg.label);

                self.cfg.push(Instr::Branch(cond, l1, l2));
                self.finish_block();
            }

            Terminator::Unreachable(..) => {
                let zero = self.mk_uint(32, 0);
                self.cfg.push(Instr::Return(zero));
                self.finish_block();
            }

            Terminator::IndirectBr(op) => {
                let id = self.mk_operand(&op.operand);

                for (i, dest) in op.possible_dests.iter().enumerate() {
                    let l = self.label(dest.clone());
                    self.exits.insert((from, l), self.cfg.label);
                    let v = self.block_address(dest.clone());

                    if i == op.possible_dests.len() - 1 {
                        self.cfg.push(Instr::Jump(l));
                        self.finish_block();
                    } else {
                        let tmp = self.cfg.fresh_ref(BOOL);
                        let next = self.cfg.fresh_label();
                        let val = self.mk_uint(32, v as usize);
                        self.cfg.push(
                            Instr::Binop { dest: tmp, ty: BOOL, lhs: id, binop: Binop::Equal, rhs: val });
                        self.cfg.push(Instr::Branch(tmp, l, next));
                        self.finish_block();
                        self.cfg.label = next;
                    }
                }
            }

            Terminator::Switch(op) => {
                let id = self.mk_operand(&op.operand);
                let bits = type_bits(self.types, &op.operand.get_type(self.types));

                for (cst, dest) in op.dests.iter() {
                    let value = constant_as_int(cst.clone()).unwrap();
                    let l = self.label(dest.clone());
                    self.exits.insert((from, l), self.cfg.label);

                    let tmp = self.cfg.fresh_ref(BOOL);
                    let next = self.cfg.fresh_label();
                    let val = self.mk_uint(bits as usize, value as usize);
                    self.cfg.push(
                        Instr::Binop { dest: tmp, ty: BOOL, lhs: id, binop: Binop::Equal, rhs: val });

                    self.cfg.push(Instr::Branch(tmp, l, next));
                    self.finish_block();
                    self.cfg.label = next;
                }

                let l = self.label(op.default_dest.clone());
                self.exits.insert((from, l), self.cfg.label);
                self.cfg.push(Instr::Jump(l));
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

    pub fn cfg(mut self) -> Builder {
        let mut cfg: Builder =
            std::mem::replace(&mut self.cfg, Builder::new());

        let labels: Vec<Label> =
            cfg.blocks.iter()
            .map(|(l,_)| l)
            .collect();

        for label in labels {
            let mut block = cfg[label].clone();

            for instr in block.iter_mut() {
                if let Instr::Phi{args, ..} = instr {
                    for (_, src) in args.iter_mut() {
                        if let Some(new_src) = self.exits.get(&(*src, label)) {
                            *src = *new_src;
                        }
                    }
                }
            }

            cfg.write_block(label, block);
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
    }

    for fun in module.functions.iter() {
        if fun.basic_blocks.len() == 0 { continue; }
        println!("\n\n\n============== {} ==============", fun.name);

        let mut builder = CfgBuilder::new(&module);
        let begin_fun = builder.label(fun.basic_blocks[0].name.clone());
        builder.cfg.push(Instr::Jump(begin_fun));
        builder.finish_block();

        for arg in fun.parameters.iter() {
            builder.add_parameter(arg.clone());
        }

        for block in fun.basic_blocks.iter() {
            let begin = builder.label(block.name.clone());
            builder.current_block = Some(block.name.clone());
            builder.cfg.label = begin;

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
