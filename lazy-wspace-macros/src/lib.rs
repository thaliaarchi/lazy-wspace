mod ir_node;

use proc_macro::TokenStream;
use proc_macro_error::proc_macro_error;
use syn::parse_macro_input;

use crate::ir_node::generate_ir_node;

#[proc_macro_attribute]
#[proc_macro_error]
pub fn ir_node(_metadata: TokenStream, input: TokenStream) -> TokenStream {
    generate_ir_node(parse_macro_input!(input)).into()
}
