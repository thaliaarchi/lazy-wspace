use proc_macro2::{Span, TokenStream};
use quote::quote;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Inst {
    pub kind: InstKind,
    pub variant: &'static str,
    pub mnemonic: &'static str,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum InstKind {
    /// ```text
    /// Number(Box<Integer>),
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

pub static NODES: [Inst; 26] = [
    Inst::new(InstKind::Number, "Number", "number"),
    Inst::new(InstKind::Error, "Error", "error"),
    Inst::new(InstKind::Op2, "Add", "add"),
    Inst::new(InstKind::Op2, "Sub", "sub"),
    Inst::new(InstKind::Op2, "Mul", "mul"),
    Inst::new(InstKind::Op2, "Div", "div"),
    Inst::new(InstKind::Op2, "Mod", "mod"),
    Inst::new(InstKind::Op2, "And", "and"),
    Inst::new(InstKind::Op2, "Or", "or"),
    Inst::new(InstKind::Op2, "Xor", "xor"),
    Inst::new(InstKind::Op2, "AndNot", "andnot"),
    Inst::new(InstKind::Op2, "NotAnd", "notand"),
    Inst::new(InstKind::Op2, "Nand", "nand"),
    Inst::new(InstKind::Op2, "Nor", "nor"),
    Inst::new(InstKind::Op2, "Xnor", "xnor"),
    Inst::new(InstKind::Op2, "NandNot", "nandnot"),
    Inst::new(InstKind::Op2, "NNotAnd", "nnotand"),
    Inst::new(InstKind::Op2U32, "Shl", "shl"),
    Inst::new(InstKind::Op2U32, "Shr", "shr"),
    Inst::new(InstKind::Op2U32, "GetBit", "getbit"),
    Inst::new(InstKind::Op2U32, "NGetBit", "ngetbit"),
    Inst::new(InstKind::Op1, "Neg", "neg"),
    Inst::new(InstKind::Op1, "Popcnt", "popcnt"),
    Inst::new(InstKind::Op1Usize, "StackRef", "stackref"),
    Inst::new(InstKind::Op1Usize, "CheckedStackRef", "checkedstackref"),
    Inst::new(InstKind::Op1, "HeapRef", "heapref"),
];

impl Inst {
    pub const fn new(kind: InstKind, variant: &'static str, mnemonic: &'static str) -> Self {
        Inst {
            kind,
            variant,
            mnemonic,
        }
    }

    pub fn check_exhaustive() -> TokenStream {
        let cases = NODES.iter().map(|inst| {
            let variant = syn::Ident::new(inst.variant, Span::call_site());
            match inst.kind {
                InstKind::Number => quote! {
                    Inst::#variant(n) => { let _: Box<Integer> = n; },
                },
                InstKind::Error => quote! {
                    Inst::#variant(err) => { let _: NumberError = err; },
                },
                InstKind::Op2 => quote! {
                    Inst::#variant(lhs, rhs) => { let _: (NodeRef, NodeRef) = (lhs, rhs); },
                },
                InstKind::Op2U32 => quote! {
                    Inst::#variant(lhs, rhs) => { let _: (NodeRef, u32) = (lhs, rhs); },
                },
                InstKind::Op1 => quote! {
                    Inst::#variant(v) => { let _: NodeRef = v; },
                },
                InstKind::Op1Usize => quote! {
                    Inst::#variant(v) => { let _: usize = v; },
                },
            }
        });
        quote! {
            const _: fn(Inst) = |inst| match inst {
                #(#cases)*
            };
        }
    }
}

pub fn pretty(tokens: TokenStream) -> syn::Result<String> {
    let file = syn::parse2(tokens)?;
    Ok(prettyplease::unparse(&file))
}
