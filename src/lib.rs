pub mod error;
pub mod inst;
pub mod io;
pub mod lex;
pub mod number;
pub mod parse;
pub mod vm;

pub mod ir {
    mod number;
    mod stack;
    pub use number::*;
    pub use stack::*;
}

#[cfg(test)]
mod tests;
