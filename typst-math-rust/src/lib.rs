mod interface;
mod parser;
mod utils;

use std::collections::HashMap;
// use std::time::Instant;

use crate::parser::parser::State;
use interface::{CustomSymbol, Decoration, Options};
use parser::parser::ast_dfs;
use utils::hook::set_panic_hook;
use wasm_bindgen::prelude::*;

/// Initialize the WASM library
#[wasm_bindgen]
pub fn init_lib() {
    set_panic_hook();
}

/// Parse a document and return the decorations to apply
#[wasm_bindgen]
pub fn parse_document(
    content: &str,
    rendering_mode: u8,
    render_outside_math: bool,
    render_spaces: bool,
    blacklisted_symbols: Vec<String>,
    custom_symbols: Vec<CustomSymbol>,
) -> Vec<Decoration> {
    // Generate a fake source
    // let now = Instant::now();
    let source = typst_syntax::Source::detached(content.to_string());
    // let elapsed_time = now.elapsed();
    // println!("Running slow_function() took {} miliseconds.", elapsed_time.as_millis());
    // println!("{:#?}", source.root());

    // Generate custom symbols hashmap
    let custom_symbols = custom_symbols
        .iter()
        .map(|pair| {
            return (
                pair.name.clone(),
                CustomSymbol {
                    name: pair.name.clone(),
                    symbol: pair.symbol.clone(),
                    category: pair.category.clone(),
                },
            );
        })
        .collect();
    

    // Parse the AST produced by typst
    let mut result: HashMap<String, Decoration> = HashMap::new();
    ast_dfs(
        &source,
        source.root(),
        &mut result,
        State {
            is_base: false,
            is_attachment: false,
        },
        &Options {
            rendering_mode,
            render_outside_math,
            render_spaces,
            blacklisted_symbols,
            custom_symbols,
        },
    );

    // Convert the hasmap into an array
    result.into_values().collect()
}


/// Generate a custom symbol struct easily from JS
#[wasm_bindgen]
pub fn generate_custom_symbol(
    name: String,
    symbol: String,
    category: String,
) -> CustomSymbol {
    return CustomSymbol {
        name,
        symbol,
        category,
    };
}