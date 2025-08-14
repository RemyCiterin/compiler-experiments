use std::collections::HashMap;

use crate::ssa::*;
use crate::rtl::*;
use super::*;

pub struct Statistics {
    /// Number of instructions
    pub instret: usize,

    /// Number of loads
    pub loads: usize,

    /// Number of stores
    pub stores: usize,

    /// Number of shitf operations
    pub shifts: usize,

    /// Number of call operations
    pub calls: usize,

    /// Number of branch operations
    pub branches: usize,

    /// Number of non-move/phi/return/jump instructions
    pub non_trivial: usize,
}

impl std::fmt::Display for Statistics {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
"Statistics:
    instret: {}
    non trivial ops: {} (including {} bit shifts)
    memop: {} (including {} loads and {} stores)
    conditional jump: {}
    function calls: {}",
            self.instret,
            self.non_trivial,
            self.shifts,
            self.loads + self.stores,
            self.loads,
            self.stores,
            self.branches,
            self.calls,
        )

    }
}

pub struct Interpreter<'a, A: Arch>{
    table: &'a LtlSymbolTable<A>,

    /// Associate a pointer to each symbol
    symbols: HashMap<String, i32>,

    /// Associate a value to each global memory location
    memory: HashMap<i32, i32>,

    /// Physical registers environment
    env: HashMap<Phys, i32>,

    /// Current symbol
    symbol: String,

    /// Stack pointer
    sp: i32,

    /// Base of the current stack frame
    frame: HashMap<Slot, i32>,

    /// Execution statistics
    pub stats: Statistics,
}

impl<'a, A: Arch> Interpreter<'a, A> {
    pub fn new(table: &'a LtlSymbolTable<A>) -> Self {
        let mut symbols: HashMap<String, i32> = HashMap::new();
        let mut memory: HashMap<i32, i32> = HashMap::new();

        let mut offset: i32 = 0x1000;

        for (symbol, section) in table.symbols.iter() {
            if let LtlSection::Data(vec) = section {
                symbols.insert(symbol.clone(), offset);
                offset += 0x1000 + 4 * vec.len() as i32;
            }
        }

        for (symbol, section) in table.symbols.iter() {
            if let LtlSection::Data(vec) = section {
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

        let mut env: HashMap<Phys, i32> = HashMap::new();

        for phy in A::callee_saved() {
            env.insert(phy, 0);
        }

        for phy in A::caller_saved() {
            env.insert(phy, 0);
        }

        Self {
            env,
            table,
            memory,
            symbols,
            sp: 0x100_0000,
            frame: HashMap::new(),
            symbol: "main".to_string(),
            stats: Statistics{
                instret: 0,
                loads: 0,
                stores: 0,
                branches: 0,
                calls: 0,
                non_trivial: 0,
                shifts: 0,
            }
        }
    }

    pub fn cfg(&self) -> &'a Ltl<A> {
        self.table.symbols[&self.symbol].as_text().unwrap()
    }

    pub fn read_var(&self, x: Phys) -> i32 {
        self.env[&x]
    }

    pub fn write_var(&mut self, var: Phys, val: i32) {
        self.env.insert(var, val);
    }

    pub fn builtin(&mut self, name: String) -> bool {
        if name == "print_i32" {
            print!("{}\n", self.env[&A::arg_regs()[0]]);

            self.write_var(A::ret_reg(), 0);
            return true;
        }

        if name == "print_endline" {
            println!("");

            self.write_var(A::ret_reg(), 0);
            return true;
        }

        if name == "puts" {
            let mut buf: Vec<u8> = Vec::new();
            let mut addr = self.env[&A::arg_regs()[0]];

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

    pub fn call(&mut self, name: String) {
        if self.builtin(name.clone()) { return; }

        let symbol = std::mem::replace(&mut self.symbol, name);

        let frame = std::mem::take(&mut self.frame);
        self.interpret_function();
        self.symbol = symbol;
        self.frame = frame;
    }

    pub fn operation(&mut self, dest: Phys, op: A::Op, args: Vec<i32>) {
        self.write_var(dest, op.eval(args).unwrap());
    }

    pub fn condition(&mut self, cond: A::Cond, args: Vec<i32>) -> bool {
        cond.eval(args).unwrap()
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

    pub fn interpret_function(&mut self) {
        //print!("{}:", self.symbol);
        //for i in 0..5 {
        //    let phy = A::arg_regs()[i];
        //    print!("  {phy} = {}", self.env[&phy]);
        //}
        //println!("");

        self.stats.calls += 1;

        // Push variables into the stack
        let sp = self.sp;
        for (slot, size) in self.cfg().stack.iter() {
            self.push(slot, *size);
        }

        let mut label = 0;

        'main_loop: loop {
            for instr in self.cfg().blocks[label].iter() {
                self.stats.instret += 1;
                //println!("{instr}");
                match instr {
                    LInstr::Operation(dest, op, args) => {
                        let args = args.iter().map(|p|self.env[p]).collect();
                        self.operation(*dest, op.clone(), args);
                        self.stats.non_trivial += 1;
                    }
                    LInstr::Move(dest, src) => {
                        self.write_var(*dest, self.env[src]);
                    }
                    LInstr::Li(dest, src) => {
                        self.write_var(*dest, *src);
                    }
                    LInstr::Ls(dest, src) => {
                        self.write_var(*dest, self.frame[src]);
                    }
                    LInstr::La(dest, src) => {
                        self.write_var(*dest, self.symbols[src]);
                    }
                    LInstr::Call(name) => {
                        self.stats.non_trivial += 1;
                        self.call(name.clone());
                    }
                    LInstr::Return => {
                        self.sp = sp;
                        return;
                    },
                    LInstr::Jump(l) => {
                        label = *l;
                        continue 'main_loop;
                    }
                    LInstr::Jcc(cond, args, l) => {
                        let args = args.iter().map(|p|self.env[p]).collect();
                        //println!("{:?}", args);
                        let jump =
                            self.condition(cond.clone(), args);
                        if jump {
                            label = *l;
                            continue 'main_loop;
                        }
                        self.stats.non_trivial += 1;
                        self.stats.branches += 1;
                    }
                    LInstr::Load{dest, addr, ..} => {
                        let val = self.load(self.env[addr]);
                        self.write_var(*dest, val);
                        self.stats.non_trivial += 1;
                        self.stats.loads += 1;
                    }
                    LInstr::Store{val, addr, ..} => {
                        self.store(self.env[addr], self.env[val]);
                        self.stats.non_trivial += 1;
                        self.stats.stores += 1;
                    }
                    LInstr::LoadLocal{dest, addr, ..} => {
                        let val = self.load(self.frame[addr]);
                        self.write_var(*dest, val);
                        self.stats.non_trivial += 1;
                        self.stats.loads += 1;
                    }
                    LInstr::StoreLocal{val, addr, ..} => {
                        self.store(self.frame[addr], self.env[val]);
                        self.stats.non_trivial += 1;
                        self.stats.stores += 1;
                    }
                }
            }

            label += 1;
        }
    }
}
