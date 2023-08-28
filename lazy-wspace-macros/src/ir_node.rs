use std::{collections::HashSet, iter};

use proc_macro2::TokenStream;
use proc_macro_error::{abort, emit_error};
use quote::quote;
use syn::{
    Expr, Field, Fields, GenericArgument, Ident, ItemStruct, Lit, PathArguments, Type, TypePath,
    Visibility,
};

pub fn generate_ir_node(input: ItemStruct) -> TokenStream {
    if !input.generics.params.is_empty() {
        emit_error!(input.generics.params, "generics are not allowed");
    } else if !input.generics.where_clause.is_none() {
        emit_error!(input.generics.where_clause, "where clauses are not allowed");
    }

    let (inputs, other_fields) = match input.fields {
        Fields::Named(fields) => {
            let mut idents = HashSet::new();
            for field in &fields.named {
                if !idents.insert(field.ident.as_ref().unwrap().to_string()) {
                    emit_error!(field, "duplicate field name");
                }
            }
            process_fields(fields.named)
        }
        Fields::Unnamed(_) => abort!(input.ident, "tuple structs are not supported"),
        Fields::Unit => abort!(input.ident, "unit structs are not supported"),
    };

    let mut min_len = 0;
    let mut fixed_len = true;
    for (input, ident) in &inputs {
        match input {
            Input::Single => min_len += 1,
            Input::Array(n) => min_len += n,
            Input::Tuple(n) => min_len += n,
            Input::Vec => {
                if !fixed_len {
                    emit_error!(ident, "only one input field may be dynamically sized");
                }
                fixed_len = false;
            }
        }
    }

    let mut input_methods = Vec::new();
    let mut i = 0;
    for (input, ident) in inputs {
        let start = i;
        let method = match input {
            Input::Single => {
                i += 1;
                quote! {
                    #[inline]
                    pub fn #ident(&self) -> NodeRef {
                        self.inputs[#start]
                    }
                }
            }
            Input::Array(n) => {
                let end = start + n;
                i += n;
                quote! {
                    #[inline]
                    pub fn #ident(&self) -> &[NodeRef; #n] {
                        unsafe { &*(&self.inputs[#start..#end] as *const [NodeRef] as *const [NodeRef; #n]) }
                    }
                }
            }
            Input::Tuple(n) => {
                let types = iter::repeat(quote! { NodeRef }).take(n);
                let values = (start..start + n).map(|i| quote! { self.inputs[#i] });
                i += n;
                quote! {
                    #[inline]
                    pub fn #ident(&self) -> (#(#types),*) {
                        (#(#values),*)
                    }
                }
            }
            Input::Vec => {
                quote! {
                    #[inline]
                    pub fn #ident(&self) -> &[NodeRef] {
                        &self.inputs[#min_len..]
                    }
                }
            }
        };
        input_methods.push(method);
    }

    let inputs_type = if fixed_len {
        quote! { [NodeRef; #min_len] }
    } else {
        quote! { Vec<NodeRef> }
    };

    let vis = input.vis;
    let node = input.ident;
    let output = quote! {
        #vis struct #node {
            inputs: #inputs_type,
            #(#other_fields),*
        }

        impl #node {
            #(#input_methods)*

            #[inline]
            pub fn inputs(&self) -> &[NodeRef] {
                &self.inputs
            }

            #[inline]
            pub const fn min_inputs() -> usize {
                #min_len
            }
        }
    };
    output
}

enum Input {
    Single,
    Array(usize),
    Tuple(usize),
    Vec,
    // TODO: arrayvec/smallvec/tinyvec
}

fn process_fields(fields: impl IntoIterator<Item = Field>) -> (Vec<(Input, Ident)>, Vec<Field>) {
    let mut inputs = Vec::new();
    let mut other_fields = Vec::new();

    for field in fields {
        let mut is_input = false;
        for attr in &field.attrs {
            match attr.path().get_ident().as_deref() {
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
                Some(input) => inputs.push((input, field.ident.unwrap())),
                None => emit_error!(
                    field.ty,
                    "type must be NodeRef, an array or tuple of NodeRef, or Vec<NodeRef>",
                ),
            }
        } else {
            other_fields.push(field);
        }
    }

    (inputs, other_fields)
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
    path.qself.is_none()
        && path.path.leading_colon.is_none()
        && path.path.get_ident().is_some_and(|id| id == "NodeRef")
}

fn strip_groups(mut ty: &Type) -> &Type {
    while let Type::Group(group) = ty {
        ty = &*group.elem;
    }
    ty
}
