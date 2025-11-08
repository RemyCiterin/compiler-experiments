use rspirv::dr::*;
use rspirv::spirv::{Op, Word};
use rspirv::binary::parse_bytes;

use std::collections::HashMap;

use crate::ssa::*;

type SWord = crate::ssa::Word;

#[derive(Clone)]
pub enum Type {
    /// A pointer to a type identifier, the size/alignment of a pointer is always 32-bits
    Pointer(Word),

    /// A struct as a list of structs, this type contains the offset of each field in memory, the
    /// type identifiers of each fields, and the global size/alignment of the type.
    Struct{
        fields: Vec<Word>,
        offsets: Vec<usize>,
        align: usize,
        size: usize,
    },

    /// A type of arrays of `count` elements of type identifier `item`
    Array{
        items: Word,
        count: usize
    },


    /// A type of vectors of `count` elements of type identifier `item`. Vectors can only be
    /// constructed for builtin types like I*, U*, Pointer(*), Void and Bool.
    Vector{
        items: Word,
        count: usize,
    },

    Function{
        args: Vec<Word>,
        ret: Word
    },

    /// 64 bit signed integer
    I64,

    /// 32 bit signed integer
    I32,

    /// 16 bit signed integer
    I16,

    /// 8 bit signed integer
    I8,

    /// 64 bit signed integer
    U64,

    /// 32 bit signed integer
    U32,

    /// 16 bit signed integer
    U16,

    /// 8 bit signed integer
    U8,

    /// 32 bit floating point
    Float,

    /// 64 bit floating point
    Double,

    /// 16 bit floating point
    Half,

    /// Zero bits type
    Void,

    /// One byte, can be only zero (false) or one (true)
    Bool,
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool => write!(f, "bool"),
            Self::Void => write!(f, "void"),
            Self::Pointer(raw) => write!(f, "*t{raw}"),
            Self::Array{items, count} => write!(f, "t{items}[{count}]"),
            Self::Vector{items, count} => write!(f, "t{items}[{count}]"),
            Self::Function{args, ret} => {
                write!(f, "t{ret} (")?;

                for i in 0..args.len() {
                    if i != 0 {write!(f, ", ")?;}
                    write!(f, "t{}", args[i])?;
                }

                write!(f, ")")
            }
            Self::Struct{fields, align, size, ..} => {
                write!(f, "struct({align}, {size}){{")?;

                for i in 0..fields.len() {
                    if i != 0 {write!(f, ", ")?;}
                    write!(f, "t{}", fields[i])?;
                }

                write!(f, "}}")
            }
            Self::Float => write!(f, "f32"),
            Self::Double => write!(f, "f64"),
            Self::Half => write!(f, "f16"),
            Self::U64 => write!(f, "u64"),
            Self::U32 => write!(f, "u32"),
            Self::U16 => write!(f, "u16"),
            Self::U8 => write!(f, "u8"),
            Self::I64 => write!(f, "i64"),
            Self::I32 => write!(f, "i32"),
            Self::I16 => write!(f, "i16"),
            Self::I8 => write!(f, "i8"),
        }
    }
}

#[derive(Clone)]
pub struct Env {
    // Map the type identifiers to their type construction
    types: Vec<Option<Type>>,

    // Return the type of each global variables
    globals: Vec<Option<(Word, Vec<SWord>)>>,

    variables: Vec<Vec<SWord>>,
}

pub fn up2(x: usize) -> usize {
    if x < 3 {return x;}
    usize::pow(2, usize::ilog2(x-1)+1)
}

impl Env {
    pub fn new() -> Self {
        Self {
            types: Vec::new(),
            globals: Vec::new(),
            variables: Vec::new(),
        }
    }

    pub fn get_type_align(&self, x: Word) -> usize {
        match self.get_type(x) {
            Type::Void => 0,
            Type::Bool => 1,
            Type::Pointer(..) => 4,
            Type::Function{..} => 4,
            Type::Vector{items, count} => up2(count * self.get_type_align(*items)),
            Type::Struct{align, ..} => *align,
            Type::Array{items, ..} => self.get_type_align(*items),
            Type::I64 | Type::U64 | Type::Double => 8,
            Type::I32 | Type::U32 | Type::Float => 4,
            Type::I16 | Type::U16 | Type::Half => 2,
            Type::I8 | Type::U8 => 1,
        }
    }

    // Return the size of a type in bytes
    pub fn get_type_size(&self, x: Word) -> usize {
        match self.get_type(x) {
            Type::Void => 0,
            Type::Bool => 1,
            Type::Pointer(..) => 4,
            Type::Function{..} => 4,
            Type::Vector{items, count} => up2(count * self.get_type_size(*items)),
            Type::Array{items, count} => count * self.get_type_size(*items),
            Type::Struct{size, ..} => *size,
            Type::I64 | Type::U64 | Type::Double => 8,
            Type::I32 | Type::U32 | Type::Float => 4,
            Type::I16 | Type::U16 | Type::Half => 2,
            Type::I8 | Type::U8 => 1,
        }
    }

    // Return the number of bytes used to represent the type, in case of a vector, if the type is
    // not aligned then a smaller number of bytes can be used that the result of sizeof. As example
    // only three words are necessary to represent an int3, so it is possible to comute their
    // additions in 3 operations instead of 4 if we use sizeof words to represent the object
    pub fn get_type_bytes(&self, x: Word) -> usize {
        match self.get_type(x) {
            Type::Void => 0,
            Type::Bool => 1,
            Type::Pointer(..) => 4,
            Type::Function{..} => 4,
            Type::Vector{items, count} => count * self.get_type_size(*items),
            Type::Array{items, count} => count * self.get_type_size(*items),
            Type::Struct{size, ..} => *size,
            Type::I64 | Type::U64 | Type::Double => 8,
            Type::I32 | Type::U32 | Type::Float => 4,
            Type::I16 | Type::U16 | Type::Half => 2,
            Type::I8 | Type::U8 => 1,
        }
    }

    #[inline]
    pub fn get_type_words(&self, x: Word) -> usize {
        let bytes = self.get_type_bytes(x);
        return (bytes+3)/4;
    }

    pub fn get_type(&self, x: Word) -> &Type {
        self.types[x as usize].as_ref().unwrap()
    }

    pub fn get_global(&self, x: Word) -> Word {
        self.globals[x as usize].as_ref().unwrap().0
    }

    pub fn get_global_data(&self, x: Word) -> &[SWord] {
        &self.globals[x as usize].as_ref().unwrap().1
    }

    fn add_type(&mut self, x: Word, ty: Type) {
        println!("t{x} := {ty}");

        while self.types.len() as u32 <= x {self.types.push(None);}
        self.types[x as usize] = Some(ty);
    }

    fn add_global(&mut self, x: Word, ty: Word, vec: Vec<SWord>) {
        while self.globals.len() as u32 <= x {self.globals.push(None);}
        self.globals[x as usize] = Some((ty, vec.clone()));

        print!("c{x} := ");
        for i in 0..vec.len() {
            if i != 0 { print!(", "); }
            print!("{}", vec[i]);
        }

        println!();
    }

    fn add_type_int(&mut self, instr: &Instruction) {
        let width = instr.operands[0].unwrap_literal_bit32() as usize;
        let signedness = instr.operands[1].unwrap_literal_bit32();

        let ty = match (width, signedness) {
            (64, 0) => Type::U64,
            (64, 1) => Type::I64,
            (32, 0) => Type::U32,
            (32, 1) => Type::I32,
            (16, 0) => Type::U16,
            (16, 1) => Type::I16,
            (8, 0) => Type::U8,
            (8, 1) => Type::I8,
            _ => {
                panic!("The only possible integer width are 8,16,32, or 64 bits")
            }
        };


        self.add_type(instr.result_id.unwrap(), ty);
    }

    fn add_type_vector(&mut self, instr: &Instruction) {
        let items = instr.operands[0].unwrap_id_ref();
        let count = instr.operands[1].unwrap_literal_bit32();

        self.add_type(
            instr.result_id.unwrap(),
            Type::Vector{items, count: count as usize}
        );
    }

    fn add_type_array(&mut self, instr: &Instruction) {
        let items = instr.operands[0].unwrap_id_ref();
        let count = self.get_global_data(instr.operands[1].unwrap_id_ref())[0].clone();

        match count {
            SWord::Int(size) =>
                self.add_type(
                    instr.result_id.unwrap(),
                    Type::Array{items, count: size as usize}
                ),
            _ => unreachable!()
        }
    }

    fn add_type_pointer(&mut self, instr: &Instruction) {
        let items = instr.operands[1].unwrap_id_ref();

        self.add_type(instr.result_id.unwrap(), Type::Pointer(items));
    }

    fn add_type_float(&mut self, instr: &Instruction) {
        let size = instr.operands[0].unwrap_literal_bit32() as usize;

        let ty = match size {
            64 => Type::Double,
            32 => Type::Float,
            16 => Type::Half,
            _ => {
                panic!("only 64,32 or 16 bits floating points are supported");
            }
        };

        self.add_type(instr.result_id.unwrap(), ty);
    }

    fn add_type_function(&mut self, instr: &Instruction) {
        let ret = instr.operands[0].unwrap_id_ref();
        let mut args = Vec::new();

        for i in 1..instr.operands.len() {
            args.push(instr.operands[i].unwrap_id_ref());
        }

        self.add_type(instr.result_id.unwrap(), Type::Function{ret, args});
    }

    fn add_type_struct(&mut self, instr: &Instruction) {
        let mut offsets = Vec::new();
        let mut aligns = Vec::new();
        let mut sizes = Vec::new();
        let mut fields = Vec::new();
        let mut align = 0;

        for op in instr.operands.iter() {
            let operand = op.unwrap_id_ref();
            let field_align = self.get_type_align(operand);
            align = usize::max(field_align, align);
            sizes.push(self.get_type_size(operand));
            aligns.push(field_align);
            fields.push(operand);
        }

        let mut offset = 0;
        for i in 0..instr.operands.len() {

            if offset % aligns[i] != 0 {
                offset += aligns[i] - (offset % aligns[i]);
            }

            offsets.push(offset);
            offset += sizes[i];
        }

        if offset % align != 0 {
            offset += align - (offset % align);
        }

        self.add_type(
            instr.result_id.unwrap(),
            Type::Struct{
                size: offset,
                offsets,
                fields,
                align
            }
        );

    }

    fn add_variable(&mut self, instr: &Instruction) {
        let ty = instr.result_type.unwrap();

        let raw_type: u32 = if let Type::Pointer(raw) = self.get_type(ty) { *raw }
        else { unreachable!() };

        let size = self.get_type_words(raw_type);

        let mut vec = vec![];
        for i in 0..size {
            if instr.operands.len() > i + 1 {
                let op = instr.operands[i+1].unwrap_id_ref();
                vec.push(self.get_global_data(op)[0].clone());
            } else {
                vec.push(SWord::Int(0));
            }
        }

        let name = format!("__anonymous{}", self.variables.len());
        self.add_global(instr.result_id.unwrap(), ty, vec![SWord::Addr(name, 0)]);
        self.variables.push(vec);
    }

    fn add_constant(&mut self, instr: &Instruction) {
        let ty = instr.result_type.unwrap();
        let mut words = vec![];

        for op in instr.operands.iter() {
            words.push(SWord::Int(op.unwrap_literal_bit32().cast_signed()));
        }

        self.add_global(instr.result_id.unwrap(), ty, words);
    }

    fn add_constant_null(&mut self, instr: &Instruction) {
        let ty = instr.result_type.unwrap();

        let size = self.get_type_words(ty);

        let mut ret = vec![];
        for _ in 0..size { ret.push(SWord::Int(0)); }

        self.add_global(instr.result_id.unwrap(), ty, ret);
    }

    fn add_constant_true(&mut self, instr: &Instruction) {
        let ty = instr.result_type.unwrap();

        self.add_global(instr.result_id.unwrap(), ty, vec![SWord::Int(1)]);
    }

    fn add_constant_false(&mut self, instr: &Instruction) {
        let ty = instr.result_type.unwrap();

        self.add_global(instr.result_id.unwrap(), ty, vec![SWord::Int(0)]);
    }

    fn add_spec_constant_op(&mut self, _instr: &Instruction) {
        // TODO: fixme
        unreachable!();
    }

    pub fn build(&mut self, block: &Vec<Instruction>) {
        for instr in block.iter() {
            //println!("\n{:?}", instr);

            match instr.class.opcode {
                Op::TypeVoid => _ = self.add_type(instr.result_id.unwrap(), Type::Void),
                Op::TypeBool => _ = self.add_type(instr.result_id.unwrap(), Type::Bool),
                Op::TypeInt => _ = self.add_type_int(instr),
                Op::TypeVector => _ = self.add_type_vector(instr),
                Op::TypeArray => _ = self.add_type_array(instr),
                Op::TypePointer => _ = self.add_type_pointer(instr),
                Op::TypeFloat => _ = self.add_type_float(instr),
                Op::TypeFunction => _ = self.add_type_function(instr),
                Op::TypeStruct => _ = self.add_type_struct(instr),
                Op::ConstantNull => _ = self.add_constant_null(instr),
                Op::ConstantTrue | Op::SpecConstantTrue => _ = self.add_constant_true(instr),
                Op::ConstantFalse | Op::SpecConstantFalse => _ = self.add_constant_false(instr),
                Op::SpecConstant | Op::Constant => _ = self.add_constant(instr),
                Op::Variable => _ = self.add_variable(instr),
                Op::SpecConstantOp => _ = self.add_spec_constant_op(instr),
                _ => unreachable!(),
            }
        }
    }
}

/// This type represent a value composed of a list of CFG variables (representing 32-bit integers)
/// and their associated spir-v type. Values can be combined to construct new composites, like
/// structs, vetors or arrays. Values can also be destruct to extracts their components.
#[derive(Clone)]
pub struct Value {
    pub ty: Word,
    pub val: Vec<Var>,
}

pub struct CfgBuilder {
    cfg: Cfg<COp, CCond>,

    // Global variables and types
    env: Env,

    // Associate a cfg label to each spir-v label
    labels: HashMap<Word, Label>,

    // As some spir-v variables are more than 32 bits long,
    // variables are represented using multiple words
    vars: HashMap<Word, Vec<Var>>,

    types: HashMap<Word, Word>,

    // Associate a stack slot to each spir-v variable
    slots: HashMap<Word, Slot>,

    // Label of the current block
    label: Label,

    // Current statement
    stmt: Vec<Instr<COp, CCond>>,
}

impl CfgBuilder {
    pub fn new(env: Env) -> Self {
        let mut cfg = Cfg::new(true);
        let label = cfg.fresh_label();
        cfg.set_block_stmt(cfg.entry(), vec![Instr::Jump(label)]);
        Self {
            cfg: cfg,
            stmt: vec![],
            labels: HashMap::new(),
            slots: HashMap::new(),
            types: HashMap::new(),
            vars: HashMap::new(),
            label,
            env,
        }
    }

    pub fn to_label(&mut self, k: Word) -> Label {
        if !self.labels.contains_key(&k) {self.labels.insert(k, self.cfg.fresh_label());}
        self.labels[&k]
    }

    pub fn new_value_with(&mut self, k: Word, v: Value) {
        assert!( !self.vars.contains_key(&k) );
        self.types.insert(k, v.ty);
        self.vars.insert(k, v.val);
    }

    pub fn new_value(&mut self, k: Word, ty: Word) {
        if self.vars.contains_key(&k) {return;}
        self.types.insert(k, ty);

        let num_var = self.env.get_type_words(ty);

        let mut vec = vec![];
        for _ in 0..num_var {
            vec.push(self.cfg.fresh_var());
        }

        self.vars.insert(k, vec);
    }

    pub fn value(&mut self, k: Word) -> Value {
        if (k as usize) < self.env.globals.len() {
            if let Some((ty, dat)) = &self.env.globals[k as usize] {
                let mut val = vec![];

                for s in dat.iter() {
                    let id = self.cfg.fresh_var();
                    val.push(id);
                    match s {
                        SWord::Int(i) => _ = self.stmt.push(Instr::Move(id, Lit::Int(*i))),
                        SWord::Addr(name, offset) => if *offset == 0 {
                            self.stmt.push(Instr::Move(id, Lit::Addr(name.clone())));
                        } else {
                            let o = self.cfg.fresh_var();
                            let n = self.cfg.fresh_var();
                            self.stmt.push(Instr::Move(o, Lit::Int(*offset)));
                            self.stmt.push(Instr::Move(n, Lit::Addr(name.clone())));
                            self.stmt.push(Instr::Operation(id, COp::PtrAdd, vec![n,o]));
                        }
                    }
                }

                return Value{ty: *ty, val};
            }
        }

        let val = self.vars[&k].clone();
        let ty = self.types[&k].clone();
        return Value{ty, val};
    }

    pub fn to_vars(&mut self, k: Word) -> Vec<Var> {
        self.value(k).val
    }

    pub fn new_slot(&mut self, k: Word, size: usize, align: usize) {
        self.slots.insert(k, self.cfg.fresh_stack_var(size, align as u8));
    }

    pub fn to_slot(&self, k: Word) -> Slot {
        self.slots[&k]
    }

    // Sign-extend the 8 less significant bits of an integer
    pub fn sign_extend(&mut self, x: Var) -> Var {
        let cst24 = self.cfg.fresh_var();
        let y = self.cfg.fresh_var();
        let z = self.cfg.fresh_var();

        self.stmt.push(Instr::Move(cst24, Lit::Int(24)));
        self.stmt.push(Instr::Operation(y, COp::Sll, vec![x, cst24]));
        self.stmt.push(Instr::Operation(z, COp::Sra, vec![y, cst24]));

        return z;
    }

    // Sign-extend the 8 less significant bits of an integer
    pub fn sign_extend_8(&mut self, x: Var) -> Var {
        let cst24 = self.cfg.fresh_var();
        let y = self.cfg.fresh_var();
        let z = self.cfg.fresh_var();

        self.stmt.push(Instr::Move(cst24, Lit::Int(24)));
        self.stmt.push(Instr::Operation(y, COp::Sll, vec![x, cst24]));
        self.stmt.push(Instr::Operation(z, COp::Sra, vec![y, cst24]));

        return z;
    }

    // Zero-extend the 8 less significant bits of an integer
    pub fn zero_extend_8(&mut self, x: Var) -> Var {
        let cst255 = self.cfg.fresh_var();
        let y = self.cfg.fresh_var();

        self.stmt.push(Instr::Move(cst255, Lit::Int(255)));
        self.stmt.push(Instr::Operation(y, COp::And, vec![x, cst255]));

        return y;
    }

    // Sign-extend the 16 less significant bits of an integer
    pub fn sign_extend_16(&mut self, x: Var) -> Var {
        let cst16 = self.cfg.fresh_var();
        let y = self.cfg.fresh_var();
        let z = self.cfg.fresh_var();

        self.stmt.push(Instr::Move(cst16, Lit::Int(16)));
        self.stmt.push(Instr::Operation(y, COp::Sll, vec![x, cst16]));
        self.stmt.push(Instr::Operation(z, COp::Sra, vec![y, cst16]));

        return z;
    }

    // Zero-extend the 16 less significant bits of an integer, we don't use 65535 as an
    // immediate because some risc-v can't represent it as an immediate for `andi`
    pub fn zero_extend_16(&mut self, x: Var) -> Var {
        let cst16 = self.cfg.fresh_var();
        let y = self.cfg.fresh_var();
        let z = self.cfg.fresh_var();

        self.stmt.push(Instr::Move(cst16, Lit::Int(16)));
        self.stmt.push(Instr::Operation(y, COp::Sll, vec![x, cst16]));
        self.stmt.push(Instr::Operation(z, COp::Srl, vec![y, cst16]));

        return y;
    }

    // Take a pointer `ptr` to a type `ty` and an index into a struct generated by an `OpConstant`
    // and return the pointer to the `index`-th element of the struct and it's type
    pub fn chain_struct(&mut self, ptr: Var, ty: Word, index: Word) -> (Var,Word) {
        let Type::Struct{offsets, fields, ..} =
            self.env.get_type(ty) else {panic!()};

        let idx_vec = self.env.get_global_data(index);
        assert!(idx_vec.len() == 1);
        let mut count = 0;

        match idx_vec[0] {
            SWord::Int(x) => count += x,
            _ => panic!()
        }

        let cst = self.cfg.fresh_var();
        let ptr2 = self.cfg.fresh_var();
        self.stmt.push(Instr::Move(cst, Lit::Int(offsets[count as usize] as i32)));
        self.stmt.push(Instr::Operation(ptr2, COp::PtrAdd, vec![ptr, cst]));

        return (ptr, fields[count as usize]);
    }

    // Take a pointer `ptr` to a type `ty` and an index
    // and return the pointer to the `index`-th element of the struct and it's type
    pub fn chain(&mut self, ptr: Var, ty: Word, index: Word) -> (Var,Word) {
        let item = match self.env.get_type(ty) {
            Type::Struct{..} => return self.chain_struct(ptr, ty, index),
            Type::Vector{items, ..}
            | Type::Array{items, ..} =>
                *items,
            _ => panic!("not a composite type")
        };

        let sz = self.env.get_type_size(item);

        let cst = self.cfg.fresh_var();
        let tmp = self.cfg.fresh_var();
        let ret = self.cfg.fresh_var();
        let idx = self.value(index).val[0];
        assert!(self.value(index).val.len() == 1);
        self.stmt.push(Instr::Move(cst, Lit::Int(sz as i32)));
        self.stmt.push(Instr::Operation(tmp, COp::Mul, vec![cst, idx]));
        self.stmt.push(Instr::Operation(ret, COp::PtrAdd, vec![ptr, tmp]));

        return (ret, item);
    }

    pub fn gen_access_chain(&mut self, instr: &Instruction) {
        let base = self.value(instr.operands[0].unwrap_id_ref());

        let mut ty = match self.env.get_type(base.ty) {
            Type::Pointer(ty) => *ty,
            _ => panic!("not a pointer type")
        };

        let mut ret = base.val[0];

        for i in 2..instr.operands.len() {
            (ret, ty) = self.chain(ret, ty, instr.operands[i].unwrap_id_ref());
        }

        let out = instr.result_id.unwrap();
        self.new_value_with(out, Value{ty: instr.result_type.unwrap(), val: vec![ret]});
    }

    pub fn gen_ptr_access_chain(&mut self, instr: &Instruction) {
        let base = self.value(instr.operands[0].unwrap_id_ref());
        let elem = self.value(instr.operands[1].unwrap_id_ref());

        let mut ty = match self.env.get_type(base.ty) {
            Type::Pointer(ty) => *ty,
            _ => panic!("not a pointer type")
        };

        let ptr = base.val[0];
        let sz = self.env.get_type_size(ty);

        let idx = elem.val[0];
        assert!(elem.val.len() == 1);
        let cst = self.cfg.fresh_var();
        let tmp = self.cfg.fresh_var();
        let mut ret = self.cfg.fresh_var();
        self.stmt.push(Instr::Move(cst, Lit::Int(sz as i32)));
        self.stmt.push(Instr::Operation(tmp, COp::Mul, vec![cst, idx]));
        self.stmt.push(Instr::Operation(ret, COp::PtrAdd, vec![ptr, tmp]));

        for i in 2..instr.operands.len() {
            (ret, ty) = self.chain(ret, ty, instr.operands[i].unwrap_id_ref());
        }

        let out = instr.result_id.unwrap();
        self.new_value_with(out, Value{ty: instr.result_type.unwrap(), val: vec![ret]});
    }

    pub fn extract1(&mut self, value: Value, index: usize) -> Value {
        let offset = match self.env.get_type(value.ty) {
            Type::Struct{offsets, ..} => offsets[index],
            Type::Vector{items, ..}
            | Type::Array{items, ..} =>
                index * self.env.get_type_size(*items),
            _ => panic!("not a composite type")
        };

        let ty = match self.env.get_type(value.ty) {
            Type::Struct{fields, ..} => fields[index],
            Type::Vector{items, ..}
            | Type::Array{items, ..} => *items,
            _ => panic!("not a composite type")
        };

        let bytes = self.env.get_type_bytes(ty);
        let words = self.env.get_type_words(ty);
        let mut val = vec![];

        for i in 0..words {
            val.push(value.val[(offset / 4) + i]);
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

            if bytes > 4 * (words-1) + (offset % 4) {
                let y = self.cfg.fresh_var();
                let z = self.cfg.fresh_var();
                let idx = (offset/4) + words;
                self.stmt.push(Instr::Operation(y, COp::Sll, vec![value.val[idx], cst2]));
                self.stmt.push(Instr::Operation(z, COp::Or, vec![val[words-1], y]));
                val[words-1] = z;
            }
        }

        match self.env.get_type(ty) {
            Type::I8 => val[0] = self.sign_extend_8(val[0]),
            Type::U8 => val[0] = self.zero_extend_8(val[0]),
            Type::I16 => val[0] = self.sign_extend_16(val[0]),
            Type::U16 => val[0] = self.zero_extend_16(val[0]),
            _ => {}
        }

        return Value{val, ty};
    }

    pub fn gen_composite_extract(&mut self, instr: &Instruction) {
        let mut value = self.value(instr.operands[0].unwrap_id_ref());

        for i in 1..instr.operands.len() {
            value =
                self.extract1(
                    value,
                    instr.operands[i].unwrap_literal_bit32() as usize
                );
        }

        assert!(instr.result_type.unwrap() == value.ty);
        self.new_value_with(instr.result_id.unwrap(), value);
    }


    pub fn gen_jump(&mut self, id: Word) {
        let label = self.to_label(id);
        self.stmt.push(Instr::Jump(label));
        self.cfg.set_block_stmt(self.label, std::mem::take(&mut self.stmt));
    }

    pub fn gen_load(&mut self, instr: &Instruction) {
        let result = instr.result_id.unwrap();
        let mut pointer = self.to_vars(instr.operands[0].unwrap_id_ref())[0];

        self.new_value(result, instr.result_type.unwrap());

        let mut kind: MemopKind = match self.env.get_type_bytes(instr.result_type.unwrap()) {
            0 => MemopKind::Unsigned8,
            1 => MemopKind::Unsigned8,
            2 => MemopKind::Unsigned16,
            _ => MemopKind::Word,
        };

        if matches!(self.env.get_type(instr.result_type.unwrap()), Type::I8) {
            kind = MemopKind::Signed8;
        }

        if matches!(self.env.get_type(instr.result_type.unwrap()), Type::I16) {
            kind = MemopKind::Signed16;
        }

        for i in 0..self.to_vars(result).len() {
            if i != 0 {
                let four = self.cfg.fresh_var();
                let new_pointer = self.cfg.fresh_var();
                self.stmt.push(Instr::Move(four, Lit::Int(4)));
                self.stmt
                    .push(Instr::Operation(new_pointer, COp::PtrAdd, vec![new_pointer, four]));
                pointer = new_pointer;
            }

            let dest = self.to_vars(result)[i];

            self.stmt.push(
                Instr::Load{
                    addr: pointer,
                    volatile: false,
                    kind,
                    dest,
                }
            );
        }
    }

    pub fn gen_compare(&mut self, _instr: &Instruction) {
        todo!()
        //let lhs = self.to_vars(instr.operands[0].unwrap_id_ref());
        //let rhs = self.to_vars(instr.operands[0].unwrap_id_ref());

        //assert!(lhs.len() == rhs.len());

        //let result_reg = instr.result_id.unwrap();
        //self.new_value(result_reg, instr.result_type.unwrap());
        //let mut result = self.to_vars(result_reg)[0];

        //for (l,r) in lhs.into_iter().zip(rhs.into_iter()) {

        //    match instr.class.opcode {
        //        Op::UGreaterThan
        //        Op::UGreaterThanEqual
        //        Op::SLessThan
        //        Op::SLessThanEqual
        //        Op::SGreaterThan
        //        Op::SGreaterThanEqual
        //        _ => unreachable!(),
        //    }
        //}
    }

    pub fn gen_return(&mut self, _instr: &Instruction) {
        let id = self.cfg.fresh_var();
        self.stmt.push(Instr::Move(id, Lit::Int(0)));
        self.stmt.push(Instr::Return(id));
        self.cfg.set_block_stmt(self.label, std::mem::take(&mut self.stmt));
        self.label = self.cfg.fresh_label();
    }

    pub fn gen_store(&mut self, instr: &Instruction) {
        let mut pointer = self.value(instr.operands[0].unwrap_id_ref()).val[0];
        let object = self.value(instr.operands[1].unwrap_id_ref());

        let kind: MemopKind = match self.env.get_type_bytes(object.ty) {
            0 => MemopKind::Unsigned8,
            1 => MemopKind::Unsigned8,
            2 => MemopKind::Unsigned16,
            _ => MemopKind::Word,
        };

        for i in 0..object.val.len() {
            if i != 0 {
                let four = self.cfg.fresh_var();
                let new_pointer = self.cfg.fresh_var();
                self.stmt.push(Instr::Move(four, Lit::Int(4)));
                self.stmt
                    .push(Instr::Operation(new_pointer, COp::PtrAdd, vec![new_pointer, four]));
                pointer = new_pointer;
            }

            let val = object.val[i];

            self.stmt.push(
                Instr::Store{
                    addr: pointer,
                    volatile: false,
                    kind,
                    val,
                }
            );
        }
    }

    pub fn gen_branch_conditional(&mut self, instr: &Instruction) {
        let cond = instr.operands[0].unwrap_id_ref();
        let l1 = instr.operands[1].unwrap_id_ref();
        let l2 = instr.operands[2].unwrap_id_ref();

        let label1 = self.to_label(l1);
        let label2 = self.to_label(l2);

        let condition = self.to_vars(cond)[0];
        self.stmt.push(Instr::Branch(CCond::Nez, vec![condition], label1, label2));
        self.cfg.set_block_stmt(self.label, std::mem::take(&mut self.stmt));
    }

    pub fn gen_branch(&mut self, instr: &Instruction) {
        self.gen_jump(instr.operands[0].unwrap_id_ref());
    }

    pub fn build(&mut self, fun: &Function) {
        print!("**********************************\nfunction:");

        for instr in fun.parameters.iter() {
            assert!(instr.class.opcode == Op::FunctionParameter);
            let id = instr.result_id.unwrap();

            self.new_value(id, instr.result_type.unwrap());
            self.cfg.args.extend(&self.vars[&id]);

            for var in self.vars[&id].iter() {
                print!(" {var}");
            }
        }

        println!();

        let mut first_block: bool = true;

        for block in fun.blocks.iter() {
            // Generate the label of the current block
            assert!(self.stmt.len() == 0);
            if let Some(id) = block.label_id() {
                if first_block { self.gen_jump(id); }
                self.label = self.to_label(id);
            } else {
                println!("--------------------------------------------no label");
                let label = self.cfg.fresh_label();
                if first_block { assert!(false); }
                self.label = label;
            }

            println!("{}:", self.label);

            first_block = false;

            for instr in block.instructions.iter() {
                match instr.class.opcode {
                    Op::Load => self.gen_load(instr),
                    Op::Return => self.gen_return(instr),
                    Op::BranchConditional => self.gen_branch_conditional(instr),
                    Op::Branch => self.gen_branch(instr),
                    Op::Store => self.gen_store(instr),
                    Op::CompositeExtract => self.gen_composite_extract(instr),
                    Op::InBoundsPtrAccessChain | Op::PtrAccessChain =>
                        self.gen_ptr_access_chain(instr),
                    Op::InBoundsAccessChain | Op::AccessChain =>
                        self.gen_access_chain(instr),
                    //Op::ULessThan | Op::ULessThanEqual |
                    //    Op::UGreaterThan | Op::UGreaterThanEqual |
                    //    Op::SLessThan | Op::SLessThanEqual |
                    //    Op::SGreaterThan | Op::SGreaterThanEqual
                    //    Op::INotEqual | Op::IEqual =>
                    //    self.gen_compare(instr),
                    _ => {
                        println!("instruction {:?} is not implemented", instr.class.opcode);
                        println!("instr: {:?}\n", instr);

                        if instr.result_type.is_some() && instr.result_id.is_some() {
                            let ty = instr.result_type.unwrap();
                            let num_var = self.env.get_type_words(ty);
                            self.new_value(instr.result_id.unwrap(), ty);

                            let mut vec = vec![];
                            for _ in 0..num_var {
                                let fresh = self.cfg.fresh_var();
                                self.stmt.push(Instr::Move(fresh, Lit::Undef));
                                vec.push(fresh);
                            }

                            self.vars.insert(instr.result_id.unwrap(), vec);
                        }
                    }
                }
            }
        }

        println!("{}", self.cfg);
    }
}

pub fn parse_spirv_spec(bytes: &[u8]) -> Module {
    let mut loader = Loader::new();

    parse_bytes(bytes, &mut loader).unwrap();
    let module = loader.module();

    let mut env = Env::new();
    env.build(&module.types_global_values);

    for function in module.functions.iter() {
        let mut builder = CfgBuilder::new(env.clone());
        builder.build(function);
        break;
    }

    return module;
}

//pub fn build_function_cfg(fun: &Function) -> Cfg<COp, CCond> {
//    let mut cfg: Cfg<COp, CCond> = Cfg::new(true);
//
//    let mut blocks: Vec<Label> = Vec::new();
//
//    for _ in 0..fun.blocks.len() {
//        blocks.push(cfg.fresh_label());
//    }
//
//
//    cfg
//}
