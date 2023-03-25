pub mod error;
pub mod io;
pub mod number;
pub mod vm;

pub mod ast {
    mod cfg;
    mod inst;
    mod lex;
    mod parse;
    pub use cfg::*;
    pub use inst::*;
    pub use lex::*;
    pub use parse::*;
}

pub mod ir {
    mod exp;
    mod heap;
    mod stack;
    pub use exp::*;
    pub use heap::*;
    pub use stack::*;
}

#[cfg(test)]
mod tests;
