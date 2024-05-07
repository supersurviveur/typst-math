mod interface;
mod parser;
mod utils;

use std::collections::HashMap;

use interface::Decoration;
use parser::ast_dfs;
use utils::hook::set_panic_hook;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn init_lib() {
    set_panic_hook();
}

#[wasm_bindgen]
pub fn parse_document(content: &str) -> Vec<Decoration> {
    // Generate a fake source
    let source = typst_syntax::Source::detached(content.to_string());
    println!("{:#?}", source.root());

    // Parse the AST produced by typst
    let mut result: HashMap<String, Decoration> = HashMap::new();
    ast_dfs(&source, source.root(), &mut result);

    // Convert the hasmap into an array
    result.values().cloned().collect()
}
