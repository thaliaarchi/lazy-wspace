use proc_macro2::{Span, TokenStream};
use quote::quote;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Node {
    pub kind: NodeKind,
    pub variant: &'static str,
    pub mnemonic: &'static str,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum NodeKind {
    /// ```text
    /// Number(Rc<Integer>),
    /// ```
    Number,
    /// ```text
    /// Error(NumberError),
    /// ```
    Error,
    /// ```text
    /// Add(NodeRef, NodeRef),
    /// Sub(NodeRef, NodeRef),
    /// Mul(NodeRef, NodeRef),
    /// Div(NodeRef, NodeRef),
    /// Mod(NodeRef, NodeRef),
    /// And(NodeRef, NodeRef),
    /// Or(NodeRef, NodeRef),
    /// Xor(NodeRef, NodeRef),
    /// AndNot(NodeRef, NodeRef),
    /// NotAnd(NodeRef, NodeRef),
    /// Nand(NodeRef, NodeRef),
    /// Nor(NodeRef, NodeRef),
    /// Xnor(NodeRef, NodeRef),
    /// NandNot(NodeRef, NodeRef),
    /// NNotAnd(NodeRef, NodeRef),
    /// ```
    Op2,
    /// ```text
    /// Shl(NodeRef, u32),
    /// Shr(NodeRef, u32),
    /// GetBit(NodeRef, u32),
    /// NotGetBit(NodeRef, u32),
    /// ```
    Op2U32,
    /// ```text
    /// Neg(NodeRef),
    /// Popcnt(NodeRef),
    /// HeapRef(NodeRef),
    /// ```
    Op1,
    /// ```text
    /// StackRef(usize),
    /// CheckedStackRef(usize),
    /// ```
    Op1Usize,
}

pub static NODES: [Node; 26] = [
    Node::new(NodeKind::Number, "Number", "number"),
    Node::new(NodeKind::Error, "Error", "error"),
    Node::new(NodeKind::Op2, "Add", "add"),
    Node::new(NodeKind::Op2, "Sub", "sub"),
    Node::new(NodeKind::Op2, "Mul", "mul"),
    Node::new(NodeKind::Op2, "Div", "div"),
    Node::new(NodeKind::Op2, "Mod", "mod"),
    Node::new(NodeKind::Op2, "And", "and"),
    Node::new(NodeKind::Op2, "Or", "or"),
    Node::new(NodeKind::Op2, "Xor", "xor"),
    Node::new(NodeKind::Op2, "AndNot", "andnot"),
    Node::new(NodeKind::Op2, "NotAnd", "notand"),
    Node::new(NodeKind::Op2, "Nand", "nand"),
    Node::new(NodeKind::Op2, "Nor", "nor"),
    Node::new(NodeKind::Op2, "Xnor", "xnor"),
    Node::new(NodeKind::Op2, "NandNot", "nandnot"),
    Node::new(NodeKind::Op2, "NNotAnd", "nnotand"),
    Node::new(NodeKind::Op2U32, "Shl", "shl"),
    Node::new(NodeKind::Op2U32, "Shr", "shr"),
    Node::new(NodeKind::Op2U32, "GetBit", "getbit"),
    Node::new(NodeKind::Op2U32, "NGetBit", "ngetbit"),
    Node::new(NodeKind::Op1, "Neg", "neg"),
    Node::new(NodeKind::Op1, "Popcnt", "popcnt"),
    Node::new(NodeKind::Op1Usize, "StackRef", "stackref"),
    Node::new(NodeKind::Op1Usize, "CheckedStackRef", "checkedstackref"),
    Node::new(NodeKind::Op1, "HeapRef", "heapref"),
];

impl Node {
    pub const fn new(kind: NodeKind, variant: &'static str, mnemonic: &'static str) -> Self {
        Node {
            kind,
            variant,
            mnemonic,
        }
    }

    pub fn check_exhaustive() -> TokenStream {
        let cases = NODES.iter().map(|node| {
            let variant = syn::Ident::new(node.variant, Span::call_site());
            match node.kind {
                NodeKind::Number => quote! {
                    Node::#variant(n) => { let _: Rc<Integer> = n; },
                },
                NodeKind::Error => quote! {
                    Node::#variant(err) => { let _: NumberError = err; },
                },
                NodeKind::Op2 => quote! {
                    Node::#variant(lhs, rhs) => { let _: (NodeRef, NodeRef) = (lhs, rhs); },
                },
                NodeKind::Op2U32 => quote! {
                    Node::#variant(lhs, rhs) => { let _: (NodeRef, u32) = (lhs, rhs); },
                },
                NodeKind::Op1 => quote! {
                    Node::#variant(v) => { let _: NodeRef = v; },
                },
                NodeKind::Op1Usize => quote! {
                    Node::#variant(v) => { let _: usize = v; },
                },
            }
        });
        quote! {
            const _: fn(Node) = |node| match node {
                #(#cases)*
            };
        }
    }
}

pub fn pretty(tokens: TokenStream) -> syn::Result<String> {
    let file = syn::parse2(tokens)?;
    Ok(prettyplease::unparse(&file))
}
