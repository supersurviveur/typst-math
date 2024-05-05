/// Stealed from https://github.com/typst/typst/blob/main/crates/typst-macros/src/symbols.rs
/// Edited to handle symbol name, symbol unicode, symbol type and symbol category

use proc_macro2::TokenStream;
use quote::quote;
use syn::ext::IdentExt;
use syn::parse::{Parse, ParseStream, Parser};
use syn::punctuated::Punctuated;
use syn::{Ident, Result, Token};

/// Expand the `symbols!` macro.
pub fn symbols(stream: TokenStream) -> Result<TokenStream> {
    let list: Punctuated<Symbol, Token![,]> =
        Punctuated::parse_terminated.parse2(stream)?;
    let pairs = list.iter().map(|symbol| {
        let name = symbol.name.to_string();
        let symbol = match &symbol.kind {
            Kind::Single(c) => quote! { #name => Symbol{ symbol: #c, category: Category::DEFAULT } },
            Kind::Multiple(variants) => {
                let first = variants.first().unwrap();
                let first = &first.c;
                let variants = variants.iter().filter_map(|variant| {
                    if !variant.name.is_empty() {
                        let new_name = format!("{}.{}", &name, &variant.name);
                        let c = &variant.c;
                        Some(quote! { #new_name => Symbol{ symbol: #c, category: Category::DEFAULT } })
                    } else {
                        None
                    }
                });
                quote! {
                    #name => Symbol { symbol: #first, category: Category::DEFAULT }, #(#variants),*
                }
            }
        };
        quote! { #symbol }
    });
    // Ok(quote! { stringify! { #(#pairs),* }})
    Ok(quote! { phf_map! { #(#pairs),* } })
}

struct Symbol {
    name: syn::Ident,
    kind: Kind,
}

enum Kind {
    Single(syn::LitChar),
    Multiple(Punctuated<Variant, Token![,]>),
}

struct Variant {
    name: String,
    c: syn::LitChar,
}

impl Parse for Symbol {
    fn parse(input: ParseStream) -> Result<Self> {
        let name = input.call(Ident::parse_any)?;
        input.parse::<Token![:]>()?;
        let kind = input.parse()?;
        Ok(Self { name, kind })
    }
}

impl Parse for Kind {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.peek(syn::LitChar) {
            Ok(Self::Single(input.parse()?))
        } else {
            let content;
            syn::bracketed!(content in input);
            Ok(Self::Multiple(Punctuated::parse_terminated(&content)?))
        }
    }
}

impl Parse for Variant {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut name = String::new();
        if input.peek(syn::Ident::peek_any) {
            name.push_str(&input.call(Ident::parse_any)?.to_string());
            while input.peek(Token![.]) {
                input.parse::<Token![.]>()?;
                name.push('.');
                name.push_str(&input.call(Ident::parse_any)?.to_string());
            }
            input.parse::<Token![:]>()?;
        }
        let c = input.parse()?;
        Ok(Self { name, c })
    }
}