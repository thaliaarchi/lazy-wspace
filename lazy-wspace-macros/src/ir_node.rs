use std::{collections::HashSet, iter};

use proc_macro2::TokenStream;
use proc_macro_error::{abort, emit_error};
use quote::quote;
use syn::{
    punctuated::Punctuated, Expr, Field, Fields, GenericArgument, Index, ItemStruct, Lit,
    PathArguments, Type, TypePath, Visibility,
};

// TODO:
// - Display impl
// - #[output] fields
// - #[doc] support
// - Initialization of and pushing to dynamic-length inputs
//   - Impl From<Vec<NodeRef>>, when it has only one #[input], a Vec<NodeRef>
// - Impl a Node trait
// - Marking non-#[input] field as #[default] to exclude it from new
// - Support for arrayvec/smallvec/tinyvec backing structures

pub fn generate_ir_node(input: ItemStruct) -> TokenStream {
    if !input.generics.params.is_empty() {
        emit_error!(input.generics.params, "generics are not allowed");
    } else if input.generics.where_clause.is_some() {
        emit_error!(input.generics.where_clause, "where clauses are not allowed");
    }

    let fields = match input.fields {
        Fields::Named(fields) => fields.named,
        Fields::Unnamed(_) => abort!(input.ident, "tuple structs are not supported"),
        Fields::Unit => Punctuated::new(),
    };

    let mut idents = HashSet::new();
    for field in &fields {
        let ident = field.ident.as_ref().unwrap().to_string();
        if !idents.insert(ident) {
            emit_error!(field, "duplicate field name");
        }
    }

    let fields = process_fields(fields);

    let mut min_len = 0;
    let mut fixed_len = true;
    for (field, input) in &fields {
        if let Some(input) = &input {
            match input {
                Input::Single => min_len += 1,
                Input::Array(n) | Input::Tuple(n) => min_len += n,
                Input::Vec => {
                    if !fixed_len {
                        emit_error!(field, "only one input field may have a dynamic length");
                    }
                    fixed_len = false;
                }
            }
        }
    }

    let mut new_params = Vec::new();
    let mut new_elements = Vec::new();
    let mut methods = Vec::new();
    let mut debug_fields = Vec::new();
    let mut other_fields = Vec::new();

    let mut i = 0;
    for (field, input) in &fields {
        let ident = field.ident.as_ref().unwrap();
        let start = i;
        let (ty, elems, getter, debug) = match input {
            Some(Input::Single) => {
                i += 1;
                (
                    quote! { NodeRef },
                    quote! { #ident },
                    quote! { *self._inputs.get_unchecked(#start) },
                    quote! { &self.#ident() },
                )
            }
            Some(Input::Array(n)) => {
                i += n;
                let elements = (0..*n).map(|i| quote! { #ident[#i] });
                (
                    quote! { &[NodeRef; #n] },
                    quote! { #(#elements),* },
                    quote! {
                        let slice = self._inputs.get_unchecked(#start..#start + #n);
                        &*(slice as *const [NodeRef] as *const [NodeRef; #n])
                    },
                    quote! { &self.#ident() },
                )
            }
            Some(Input::Tuple(n)) => {
                i += n;
                let types = iter::repeat(quote! { NodeRef }).take(*n);
                let values = (start..start + n).map(|i| {
                    quote! { *self._inputs.get_unchecked(#i) }
                });
                let elements = (0..*n).map(|i| {
                    let index = Index::from(i);
                    quote! { #ident.#index }
                });
                (
                    quote! { (#(#types),*) },
                    quote! { #(#elements),* },
                    quote! { (#(#values),*) },
                    quote! { &self.#ident() },
                )
            }
            Some(Input::Vec) => (
                quote! { &[NodeRef] },
                quote! {},
                quote! { self._inputs.get_unchecked(#min_len..) },
                quote! { &self.#ident() },
            ),
            None => {
                other_fields.push(field);
                let ty = &field.ty;
                (
                    quote! { #ty },
                    quote! {},
                    quote! {},
                    quote! { &self.#ident },
                )
            }
        };

        if input != &Some(Input::Vec) {
            new_params.push(quote! { #ident: #ty });
            if input.is_some() {
                new_elements.push(elems);
            }
        }
        if input.is_some() {
            methods.push(quote! {
                #[inline]
                pub fn #ident(&self) -> #ty {
                    unsafe { #getter }
                }
            });
        }
        debug_fields.push(quote! { stringify!(#ident), #debug });
    }

    let inputs_ty = if fixed_len {
        quote! { [NodeRef; #min_len] }
    } else {
        quote! { Vec<NodeRef> }
    };
    let inputs_new = if fixed_len {
        quote! {}
    } else {
        quote! { vec! }
    };
    let other_idents = other_fields
        .iter()
        .map(|field| field.ident.as_ref().unwrap());

    let vis = input.vis;
    let node = input.ident;
    let output = quote! {
        #[derive(Clone, PartialEq, Eq, Hash)]
        #vis struct #node {
            _inputs: #inputs_ty,
            #(#other_fields),*
        }

        impl #node {
            #[inline]
            pub fn new(#(#new_params),*) -> Self {
                #node {
                    _inputs: #inputs_new[#(#new_elements),*],
                    #(#other_idents),*
                }
            }

            #(#methods)*
        }

        impl crate::ir::nodes::Node for #node {
            const MIN_INPUTS: usize = #min_len;

            #[inline]
            fn inputs(&self) -> &[NodeRef] {
                &self._inputs
            }

            #[inline]
            fn as_exp(&self) -> crate::ir::nodes::Exp<'_> {
                crate::ir::nodes::Exp::from(self)
            }
        }

        impl ::std::fmt::Debug for #node {
            fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                f.debug_struct(stringify!(#node))
                    #(.field(#debug_fields))*
                    .finish()
            }
        }
    };
    output
}

#[derive(Debug, PartialEq, Eq)]
enum Input {
    Single,
    Array(usize),
    Tuple(usize),
    Vec,
}

fn process_fields(fields: impl IntoIterator<Item = Field>) -> Vec<(Field, Option<Input>)> {
    let mut node_fields = Vec::new();

    for field in fields {
        let mut is_input = false;
        for attr in &field.attrs {
            match attr.path().get_ident() {
                Some(path) if path == "input" => {
                    if is_input {
                        emit_error!(attr, "field marked as #[input] multiple times");
                    }
                    is_input = true;
                }
                _ if is_input => {
                    emit_error!(attr, "unrecognized attribute for an input");
                }
                _ => {}
            }
        }

        if field.vis != Visibility::Inherited && is_input {
            emit_error!(field.vis, "inputs must be private");
        }

        if is_input {
            match get_input_type(&field.ty) {
                Some(input) => node_fields.push((field, Some(input))),
                None => emit_error!(
                    field.ty,
                    "type must be NodeRef, an array or tuple of NodeRef, or Vec<NodeRef>",
                ),
            }
        } else {
            node_fields.push((field, None));
        }
    }

    node_fields
}

fn get_input_type(ty: &Type) -> Option<Input> {
    match strip_groups(ty) {
        Type::Path(path) if path.qself.is_none() && path.path.leading_colon.is_none() => {
            if path_is_node_ref(path) {
                return Some(Input::Single);
            } else if path.path.segments.len() == 1 {
                let segment = path.path.segments.first().unwrap();
                if segment.ident == "Vec" {
                    if let PathArguments::AngleBracketed(args) = &segment.arguments {
                        if args.args.len() == 1 {
                            if let GenericArgument::Type(arg) = args.args.first().unwrap() {
                                if is_node_ref(arg) {
                                    return Some(Input::Vec);
                                }
                            }
                        }
                    }
                }
            }
        }
        Type::Array(array) => {
            if let Type::Path(path) = &*array.elem {
                if path_is_node_ref(path) {
                    let mut len = &array.len;
                    while let Expr::Group(group) = len {
                        len = &*group.expr;
                    }
                    if let Expr::Lit(lit) = len {
                        if let Lit::Int(int) = &lit.lit {
                            match int.base10_parse() {
                                Ok(len) => return Some(Input::Array(len)),
                                Err(err) => emit_error!(int, err),
                            }
                        }
                    }
                }
            }
        }
        Type::Tuple(tuple) => {
            if (tuple.elems.iter()).all(
                |elem| matches!(strip_groups(elem), Type::Path(path) if path_is_node_ref(path)),
            ) {
                return Some(Input::Tuple(tuple.elems.len()));
            }
        }
        _ => {}
    }
    None
}

fn is_node_ref(ty: &Type) -> bool {
    let ty = strip_groups(ty);
    match ty {
        Type::Path(path) => path_is_node_ref(path),
        _ => false,
    }
}

fn path_is_node_ref(path: &TypePath) -> bool {
    path.path.is_ident("NodeRef")
}

fn strip_groups(mut ty: &Type) -> &Type {
    while let Type::Group(group) = ty {
        ty = &*group.elem;
    }
    ty
}
