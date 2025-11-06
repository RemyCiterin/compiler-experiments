use rspirv::dr::*;
use rspirv::spirv::{Op, Word};
use rspirv::binary::parse_bytes;

use std::collections::HashMap;

use crate::ssa::*;

type SWord = crate::ssa::Word;

#[derive(Clone)]
pub enum Type {
    // A pointer to a type identifier
    Pointer(Word),

    // A struct as a list of structs
    Struct{
        fields: Vec<Word>,
        offsets: Vec<usize>,
        align: usize,
        size: usize,
    },

    Array{
        items: Word,
        count: usize
    },

    Vector{
        items: Word,
        count: usize,
    },

    Function{
        args: Vec<Word>,
        ret: Word
    },

    // An integer of a given size and signedness
    Int(usize, bool),

    // A floating point of a given size
    Float(usize),

    Void,

    Bool,
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool => write!(f, "bool"),
            Self::Void => write!(f, "void"),
            Self::Pointer(raw) => write!(f, "*t{raw}"),
            Self::Int(w, false) => write!(f, "u{w}"),
            Self::Int(w, true) => write!(f, "i{w}"),
            Self::Float(w) => write!(f, "f{w}"),
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
            Type::Vector{items, ..} => self.get_type_align(*items),
            Type::Int(size, _) => up2(size / 8),
            Type::Float(size) => up2(size / 8),
            Type::Struct{align, ..} => *align,
            Type::Array{items, ..} => self.get_type_align(*items),
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
            Type::Int(size, _) => up2(size / 8),
            Type::Float(size) => up2(size / 8),
            Type::Struct{size, ..} => *size
        }
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
        self.globals[x as usize] = Some((ty, vec));
    }

    fn add_type_int(&mut self, instr: &Instruction) {
        let width = instr.operands[0].unwrap_literal_bit32() as usize;
        let signedness = instr.operands[1].unwrap_literal_bit32();

        self.add_type(instr.result_id.unwrap(), Type::Int(width, signedness == 1));
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

        self.add_type(instr.result_id.unwrap(), Type::Float(size));
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

        let mut size = self.get_type_size(raw_type) / 4;
        if self.get_type_size(raw_type) % 4 != 0 { size += 1; }

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

        let mut size = self.get_type_size(ty) / 4;
        if self.get_type_size(ty) % 4 != 0 { size += 1; }

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
        //for instr in block.iter() {
        //    println!("\n{:?}", instr);
        //}

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

    pub fn create_var(&mut self, k: Word, ty: Word) {
        if self.vars.contains_key(&k) {return;}
        self.types.insert(k, ty);

        let mut num_var = self.env.get_type_size(ty)/4;
        if self.env.get_type_size(ty) % 4 != 0 {num_var += 1;}

        let mut vec = vec![];
        for _ in 0..num_var {
            vec.push(self.cfg.fresh_var());
        }

        self.vars.insert(k, vec);
    }

    pub fn to_vars(&mut self, k: Word) -> Vec<Var> {
        if (k as usize) < self.env.globals.len() {
            if let Some((_, dat)) = &self.env.globals[k as usize] {
                let mut ret = vec![];

                for s in dat.iter() {
                    let id = self.cfg.fresh_var();
                    ret.push(id);
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

                return ret;
            }
        }

        return self.vars[&k].clone();
    }

    pub fn new_slot(&mut self, k: Word, size: usize, align: usize) {
        self.slots.insert(k, self.cfg.fresh_stack_var(size, align as u8));
    }

    pub fn to_slot(&self, k: Word) -> Slot {
        self.slots[&k]
    }

    pub fn gen_jump(&mut self, id: Word) {
        let label = self.to_label(id);
        self.stmt.push(Instr::Jump(label));
        self.cfg.set_block_stmt(self.label, std::mem::take(&mut self.stmt));
    }

    pub fn gen_load(&mut self, instr: &Instruction) {
        let result = instr.result_id.unwrap();
        let mut pointer = self.to_vars(instr.operands[0].unwrap_id_ref())[0];

        self.create_var(result, instr.result_type.unwrap());

        let kind: MemopKind = match self.env.get_type_size(instr.result_type.unwrap()) {
            0 => MemopKind::Unsigned8,
            1 => MemopKind::Unsigned8,
            2 => MemopKind::Unsigned16,
            _ => MemopKind::Word,
        };

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

    pub fn gen_return(&mut self, _instr: &Instruction) {
        let id = self.cfg.fresh_var();
        self.stmt.push(Instr::Move(id, Lit::Int(0)));
        self.stmt.push(Instr::Return(id));
        self.cfg.set_block_stmt(self.label, std::mem::take(&mut self.stmt));
        self.label = self.cfg.fresh_label();
    }

    pub fn gen_store(&mut self, instr: &Instruction) {
        let mut pointer = self.vars[&instr.operands[0].unwrap_id_ref()][0];
        let object = instr.operands[1].unwrap_id_ref();

        let kind: MemopKind = match self.env.get_type_size(self.types[&object]) {
            0 => MemopKind::Unsigned8,
            1 => MemopKind::Unsigned8,
            2 => MemopKind::Unsigned16,
            _ => MemopKind::Word,
        };

        for i in 0..self.to_vars(object).len() {
            if i != 0 {
                let four = self.cfg.fresh_var();
                let new_pointer = self.cfg.fresh_var();
                self.stmt.push(Instr::Move(four, Lit::Int(4)));
                self.stmt
                    .push(Instr::Operation(new_pointer, COp::PtrAdd, vec![new_pointer, four]));
                pointer = new_pointer;
            }

            let val = self.to_vars(object)[i];

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

    pub fn build(&mut self, fun: &Function) {
        print!("**********************************\nfunction:");

        for instr in fun.parameters.iter() {
            assert!(instr.class.opcode == Op::FunctionParameter);
            let id = instr.result_id.unwrap();

            self.create_var(id, instr.result_type.unwrap());
            self.cfg.args.extend(&self.vars[&id]);

            for var in self.vars[&id].iter() {
                print!(" {var}");
            }
        }

        println!();

        for block in fun.blocks.iter() {
            // Generate the label of the current block
            let label: Label;
            if let Some(id) = block.label_id() {
                label = self.to_label(id);
                self.gen_jump(id);
            } else {
                label = self.cfg.fresh_label();
            }

            self.label = label;
            println!("{label}:");


            for instr in block.instructions.iter() {
                println!("instr: {:?}\n", instr);
                match instr.class.opcode {
                    Op::Load => self.gen_load(instr),
                    Op::Return => self.gen_return(instr),
                    //Op::Store => self.gen_store(instr),
                    _ => {}
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
