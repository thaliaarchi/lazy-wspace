use proc_macro2::{Span, TokenStream};
use quote::quote;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Inst {
    pub variant: &'static str,
    pub mnemonic: &'static str,
    pub kind: InstKind,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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
    /// Store(NodeRef, NodeRef),
    /// ```
    Op2,
    /// ```text
    /// Shl(NodeRef, u32),
    /// Shr(NodeRef, u32),
    /// GetBit(NodeRef, u32),
    /// NGetBit(NodeRef, u32),
    /// ```
    Op2U32,
    /// ```text
    /// Eval(NodeRef),
    /// Neg(NodeRef),
    /// Popcnt(NodeRef),
    /// Push(NodeRef),
    /// HeapRef(NodeRef),
    /// ```
    Op1,
    /// ```text
    /// CheckedStackRef(usize),
    /// GuardStack(usize),
    /// Drop(usize),
    /// DropLazy(usize),
    /// ```
    Op1Usize,
    /// ```text
    /// Ret,
    /// Exit,
    /// ```
    Op0,
    /// ```text
    /// StackRef(usize, NodeRef),
    /// ```
    StackRef,
    /// ```text
    /// Print(IoKind, NodeRef),
    /// ```
    Print,
    /// ```text
    /// Read(IoKind),
    /// ```
    Read,
    /// ```text
    /// Call(BBlockId, BBlockId),
    /// ```
    Call,
    /// ```text
    /// Jmp(BBlockId),
    /// ```
    Jmp,
    /// ```text
    /// Br(Cond, NodeRef, BBlockId, BBlockId),
    /// ```
    Br,
    /// ```text
    /// Panic(Box<Error>),
    /// ```
    Panic,
}

pub static NODES: [Inst; 40] = [
    Inst::new("Number", "value", InstKind::Number),
    Inst::new("Error", "error", InstKind::Error),
    Inst::new("Eval", "eval", InstKind::Op1),
    Inst::new("Add", "add", InstKind::Op2),
    Inst::new("Sub", "sub", InstKind::Op2),
    Inst::new("Mul", "mul", InstKind::Op2),
    Inst::new("Div", "div", InstKind::Op2),
    Inst::new("Mod", "mod", InstKind::Op2),
    Inst::new("And", "and", InstKind::Op2),
    Inst::new("Or", "or", InstKind::Op2),
    Inst::new("Xor", "xor", InstKind::Op2),
    Inst::new("AndNot", "andnot", InstKind::Op2),
    Inst::new("NotAnd", "notand", InstKind::Op2),
    Inst::new("Nand", "nand", InstKind::Op2),
    Inst::new("Nor", "nor", InstKind::Op2),
    Inst::new("Xnor", "xnor", InstKind::Op2),
    Inst::new("NandNot", "nandnot", InstKind::Op2),
    Inst::new("NNotAnd", "nnotand", InstKind::Op2),
    Inst::new("Shl", "shl", InstKind::Op2U32),
    Inst::new("Shr", "shr", InstKind::Op2U32),
    Inst::new("GetBit", "getbit", InstKind::Op2U32),
    Inst::new("NGetBit", "ngetbit", InstKind::Op2U32),
    Inst::new("Neg", "neg", InstKind::Op1),
    Inst::new("Popcnt", "popcnt", InstKind::Op1),
    Inst::new("StackRef", "stack_ref", InstKind::StackRef),
    Inst::new("CheckedStackRef", "checked_stack_ref", InstKind::Op1Usize),
    Inst::new("GuardStack", "guard_stack", InstKind::Op1Usize),
    Inst::new("Push", "push", InstKind::Op1),
    Inst::new("Drop", "drop", InstKind::Op1Usize),
    Inst::new("DropLazy", "drop_lazy", InstKind::Op1Usize),
    Inst::new("HeapRef", "heap_ref", InstKind::Op1),
    Inst::new("Store", "store", InstKind::Op2),
    Inst::new("Print", "print", InstKind::Print),
    Inst::new("Read", "read", InstKind::Read),
    Inst::new("Call", "call", InstKind::Call),
    Inst::new("Jmp", "jmp", InstKind::Jmp),
    Inst::new("Br", "br", InstKind::Br),
    Inst::new("Ret", "ret", InstKind::Op0),
    Inst::new("Exit", "exit", InstKind::Op0),
    Inst::new("Panic", "panic", InstKind::Panic),
];

impl Inst {
    pub const fn new(variant: &'static str, mnemonic: &'static str, kind: InstKind) -> Self {
        Inst {
            variant,
            mnemonic,
            kind,
        }
    }

    pub fn check_exhaustive() -> TokenStream {
        let cases = NODES.iter().map(|inst| {
            let variant = syn::Ident::new(inst.variant, Span::call_site());
            match inst.kind {
                InstKind::Number => quote! {
                    Inst::#variant(n) => { let _: Box<Integer> = n; }
                },
                InstKind::Error => quote! {
                    Inst::#variant(err) => { let _: NumberError = err; }
                },
                InstKind::Op2 => quote! {
                    Inst::#variant(lhs, rhs) => { let _: (NodeRef, NodeRef) = (lhs, rhs); }
                },
                InstKind::Op2U32 => quote! {
                    Inst::#variant(lhs, rhs) => { let _: (NodeRef, u32) = (lhs, rhs); }
                },
                InstKind::Op1 => quote! {
                    Inst::#variant(v) => { let _: NodeRef = v; }
                },
                InstKind::Op1Usize => quote! {
                    Inst::#variant(v) => { let _: usize = v; }
                },
                InstKind::Op0 => quote! {
                    Inst::#variant => { }
                },
                InstKind::StackRef => quote! {
                    Inst::#variant(n, guard) => { let _: (usize, NodeRef) = (n, guard); }
                },
                InstKind::Print => quote! {
                    Inst::#variant(kind, v) => { let _: (IoKind, NodeRef) = (kind, v); }
                },
                InstKind::Read => quote! {
                    Inst::#variant(kind) => { let _: IoKind = kind; }
                },
                InstKind::Call => quote! {
                    Inst::#variant(target, next) => { let _: (BBlockId, BBlockId) = (target, next); }
                },
                InstKind::Jmp => quote! {
                    Inst::#variant(target) => { let _: BBlockId = target; }
                },
                InstKind::Br => quote! {
                    Inst::#variant(cond, v, t, e) => { let _: (Cond, NodeRef, BBlockId, BBlockId) = (cond, v, t, e); }
                },
                InstKind::Panic => quote! {
                    Inst::#variant(err) => { let _: Box<Error> = err; }
                },
            }
        });
        quote! {
            const _: fn(Inst) = |inst| {
                use crate::error::Error;
                use crate::ir::*;
                match inst {
                    #(#cases)*
                }
            };
        }
    }
}

pub fn pretty(tokens: TokenStream) -> syn::Result<String> {
    let file = syn::parse2(tokens)?;
    Ok(prettyplease::unparse(&file))
}
