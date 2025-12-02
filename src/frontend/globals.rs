use llvm_ir::module::Module;
use llvm_ir::predicates::*;
use llvm_ir::constant::*;
use llvm_ir::types::*;
use llvm_ir::*;


use num_bigint::*;

pub enum Token {
    /// Represent a block of n bits plus an offset
    Int{bits: usize, data: BigUint},

    /// Represent the address of an address plus an offset
    Symbol(String, u32),
}

impl Token {
    pub fn canon(&mut self, other: &Token) -> bool {
        match (self, other) {
            (
                Token::Int{bits: b1, data: d1},
                Token::Int{bits: b2, data: d2}) => {

                *d1 = d1 as &BigUint | (d2 << *b1);
                *b1 = *b1 + *b2;
                true
            }

            _ => false
        }
    }
}

impl Default for Token {
    fn default() -> Self {
        Token::Int{bits: 0, data: BigUint::ZERO}
    }
}

pub struct Value(pub Vec<Token>);

impl Value {
    pub fn extend(&mut self, other: Value) {
        self.0.extend(other.0);
    }

//    pub fn canon(&mut self) {
//        let mut i = 0;
//
//        let mut buffer =
//        while i < self.0.len()-1 {
//        }
//    }


}
