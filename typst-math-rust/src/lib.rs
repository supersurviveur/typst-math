mod interface;
mod parser;
mod utils;

use std::collections::HashMap;

use crate::parser::parser::State;
use interface::{Decoration, Options};
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
) -> Vec<Decoration> {
    // Generate a fake source
    let source = typst_syntax::Source::detached(content.to_string());
    // println!("{:#?}", source.root());

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
        },
    );

    // Convert the hasmap into an array
    result.into_values().collect()
}
