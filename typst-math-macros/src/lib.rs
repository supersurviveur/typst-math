mod symbols;
use proc_macro::TokenStream;

#[proc_macro]
pub fn symbols(stream: TokenStream) -> TokenStream {
    symbols::symbols(stream.into())
        .unwrap_or_else(|err| err.to_compile_error())
        .into()
}