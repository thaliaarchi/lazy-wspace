use proc_macro2::{Span, TokenStream};
use proc_macro_error::abort_call_site;
use quote::quote;
use syn::{Data, DeriveInput, Fields, Ident};

pub fn derive_opcode(input: DeriveInput) -> TokenStream {
    let data = match input.data {
        Data::Enum(data) => data,
        _ => abort_call_site!("#[derive(Opcodes)] only supports enums"),
    };

    let vis = &input.vis;
    let name = &input.ident;
    let opcode_name = if name == "Inst" {
        Ident::new("Opcode", Span::call_site())
    } else {
        Ident::new(&format!("{}Opcode", name), Span::call_site())
    };
    let opcode_doc = format!("Opcode for [{name}].");

    let opcode_variants = data.variants.iter().map(|variant| &variant.ident);

    let opcode_arms = data.variants.iter().map(|variant| {
        let ident = &variant.ident;
        let params = match &variant.fields {
            Fields::Unit => quote! {},
            Fields::Unnamed(_) => quote! { (..) },
            Fields::Named(_) => quote! { { .. } },
        };
        quote! { #name::#ident #params => #opcode_name::#ident }
    });

    quote! {
        #[doc = #opcode_doc]
        #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
        #vis enum #opcode_name {
            #(#opcode_variants),*
        }

        impl #name {
            #vis fn opcode(&self) -> #opcode_name {
                match self {
                    #(#opcode_arms),*
                }
            }
        }
    }
}
