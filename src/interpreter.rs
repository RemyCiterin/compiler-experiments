use std::collections::HashMap;

use crate::ssa::*;
use crate::ast::*;

pub struct Interpreter<'a>{
    table: &'a SymbolTable<Instr>,

    /// Associate a pointer to each symbol
    symbols: HashMap<String, i32>,

    /// Associate a value to each global memory location
    memory: HashMap<i32, i32>,

    /// Environment of variables
    envs: Vec<HashMap<Var, i32>>,

    /// Current symbol
    symbol: String,

    /// Stack pointer
    sp: i32,

    /// Base of the current stack frame
    frame: HashMap<Slot, i32>,

    /// Number of executed instructions
    pub instret: usize,

    /// Number of executed load instructions
    pub loads: usize,

    /// Number of executed store instructions
    pub stores: usize,

    pub calls: usize,
}

impl<'a> Interpreter<'a> {
    pub fn new(table: &'a SymbolTable<Instr>) -> Self {
        let mut symbols: HashMap<String, i32> = HashMap::new();
        let mut memory: HashMap<i32, i32> = HashMap::new();

        let mut offset: i32 = 0x1000;

        for (symbol, section) in table.symbols.iter() {
            if let Section::Data(vec) = section {
                symbols.insert(symbol.clone(), offset);
                offset += 0x1000 + 4 * vec.len() as i32;
            }
        }

        for (symbol, section) in table.symbols.iter() {
            if let Section::Data(vec) = section {
                let vec: Vec<i32> =
                    vec.iter()
                    .map(|word| {
                        match word {
                            Word::Int(i) => *i,
                            Word::Addr(s, i) => symbols[s] + i,
                        }
                    })
                    .collect();
                let mut offset = symbols[symbol];

                for x in vec {
                    memory.insert(offset / 4, x);
                    offset += 4;
                }
            }
        }

        Self {
            table,
            memory,
            symbols,
            loads: 0,
            calls: 0,
            stores: 0,
            instret: 0,
            sp: 0x100_0000,
            frame: HashMap::new(),
            envs: vec![HashMap::new()],
            symbol: "main".to_string(),
        }
    }

    pub fn cfg(&self) -> &'a Cfg<Instr> {
        self.table.symbols[&self.symbol].as_text().unwrap()
    }

    pub fn read_var(&self, var: Var) -> Option<i32> {
        self.envs.last().unwrap().get(&var).cloned()
    }

    pub fn write_var(&mut self, var: Var, val: i32) {
        self.envs.last_mut().unwrap().insert(var, val);
    }

    pub fn builtin(&mut self, dest: Var, name: String, args: Vec<i32>) -> bool {
        if name == "print_i32" {
            for arg in args {
                print!("{} ", arg);
            }
            println!("");

            self.write_var(dest, 0);
            return true;
        }

        if name == "print_endline" {
            println!("");

            self.write_var(dest, 0);
            return true;
        }

        if name == "puts" {
            let mut buf: Vec<u8> = Vec::new();
            let mut addr = args[0];

            while self.load(addr) != 0 {
                buf.push(self.load(addr) as u8);
                addr += 4;
            }

            buf.push(0);

            print!("{}", String::from_utf8(buf).unwrap());
            return true;
        }

        return false;
    }

    pub fn call(&mut self, dest: Var, name: String, args: Vec<i32>) {
        if self.builtin(dest, name.clone(), args.clone()) { return; }

        self.envs.push(HashMap::new());
        let symbol = std::mem::replace(&mut self.symbol, name);

        for i in 0..args.len() {
            self.write_var(self.cfg().args[i], args[i]);
        }

        let frame = std::mem::take(&mut self.frame);
        let result = self.interpret_function();
        self.symbol = symbol;
        self.frame = frame;
        self.envs.pop();

        self.write_var(dest, result);
    }

    pub fn lit(&self, lit: &Lit) -> i32 {
        match lit {
            Lit::Int(i) => *i,
            Lit::Var(v) => self.read_var(*v).unwrap(),
            Lit::Addr(s) => self.symbols[s],
            Lit::Stack(slot) => self.frame[slot]
        }
    }

    pub fn binop(&mut self, dest: Var, binop: Binop, lhs: i32, rhs: i32) {
        let result: i32 = match binop {
            Binop::And => lhs & rhs,
            Binop::Or => lhs | rhs,
            Binop::Xor => lhs ^ rhs,
            Binop::Add => lhs.wrapping_add(rhs),
            Binop::Sub => lhs.wrapping_sub(rhs),
            Binop::Sll => lhs << rhs,
            Binop::Sra => lhs >> rhs,
            Binop::Srl => ((lhs as u32) >> (rhs as u32)) as i32,
            Binop::Equal => (lhs == rhs) as i32,
            Binop::NotEqual => (lhs != rhs) as i32,
            Binop::LessThan => (lhs < rhs) as i32,
            Binop::LessEqual => (lhs <= rhs) as i32,
            Binop::ULessThan => ((lhs as u32) < (rhs as u32)) as i32,
            Binop::ULessEqual => ((lhs as u32) <= (rhs as u32)) as i32,
        };

        self.write_var(dest, result);
    }

    pub fn unop(&mut self, dest: Var, unop: Unop, e: i32) {
        let result = match unop {
            Unop::Neg => e.wrapping_neg(),
            Unop::Not => !e,
        };

        self.write_var(dest, result);
    }

    pub fn load(&self, addr: i32) -> i32 {
        assert!(addr % 4 == 0);
        self.memory[&(addr / 4)]
    }

    pub fn store(&mut self, addr: i32, val: i32) {
        assert!(addr % 4 == 0);
        self.memory.insert(addr / 4, val);
    }

    pub fn push(&mut self, slot: Slot, size: usize) {
        assert!(size % 4 == 0);
        self.frame.insert(slot, self.sp);
        self.memory.insert(self.sp / 4, 0);
        self.sp += size as i32;
    }

    pub fn interpret_function(&mut self) -> i32 {
        self.calls += 1;

        // Push variables into the stack
        let sp = self.sp;
        for (slot, size) in self.cfg().stack.iter() {
            self.push(slot, *size);
        }

        for (var, _) in self.cfg().iter_vars() {
            if self.read_var(var) == None {
                self.write_var(var, 0);
            }
        }

        let mut prev_label = self.cfg().entry();
        let mut label = self.cfg().entry();

        loop {
            for instr in self.cfg()[label].stmt.iter() {
                self.instret += 1;
                match instr {
                    Instr::Phi(dest, args) => {
                        let (l, _) =
                            args.iter().rfind(|(_, l)| *l == prev_label).unwrap();
                        self.write_var(*dest, self.lit(l))
                    }
                    Instr::Binop(dest, binop, l1, l2) => {
                        self.binop(*dest, *binop, self.lit(l1), self.lit(l2));
                    }
                    Instr::Unop(dest, unop, l) => {
                        self.unop(*dest, *unop, self.lit(l));
                    }
                    Instr::Move(dest, l) => {
                        self.write_var(*dest, self.lit(l));
                    }
                    Instr::Call(dest, name, args) => {
                        self.call(
                            *dest,
                            name.clone(),
                            args.iter().map(|x|self.lit(x)).collect()
                         );
                    }
                    Instr::Return(l) => {
                        self.sp = sp;
                        return self.lit(l)
                    },
                    Instr::Jump(l) => {
                        prev_label = label;
                        label = *l;
                        continue;
                    }
                    Instr::Branch(cond, l1, l2) => {
                        prev_label = label;
                        label = if self.lit(cond) != 0 {*l1} else {*l2};
                        continue;
                    }
                    Instr::Load{dest, addr, ..} => {
                        let val = self.load(self.lit(addr));
                        self.write_var(*dest, val);
                        self.loads += 1;
                    }
                    Instr::Store{val, addr, ..} => {
                        self.store(self.lit(addr), self.lit(val));
                        self.stores += 1;
                    }
                }
            }
        }
    }
}
