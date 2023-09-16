pub mod instructions;

mod cfg;
mod graph;
mod heap;
mod peephole;
mod stack;
mod table;

pub use cfg::*;
pub use graph::*;
pub use heap::*;
pub use peephole::*;
pub use stack::*;
pub use table::*;
