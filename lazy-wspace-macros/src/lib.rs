mod ir_node;
mod opcodes;

use proc_macro::TokenStream;
use proc_macro_error::proc_macro_error;
use syn::parse_macro_input;

use crate::ir_node::generate_ir_node;
use crate::opcodes::derive_opcode;

#[proc_macro_attribute]
#[proc_macro_error]
pub fn ir_node(_metadata: TokenStream, input: TokenStream) -> TokenStream {
    generate_ir_node(parse_macro_input!(input)).into()
}

#[proc_macro_derive(Opcode)]
#[proc_macro_error]
pub fn opcode(input: TokenStream) -> TokenStream {
    derive_opcode(parse_macro_input!(input)).into()
}
