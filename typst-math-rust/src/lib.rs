mod interface;
mod parser;
mod utils;

use std::collections::HashMap;

use interface::Decoration;
// use js::{logger, LogOutputChannel};
use parser::ast_dfs;
use crate::parser::State;
use utils::hook::set_panic_hook;
use wasm_bindgen::prelude::*;
mod js;

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
    ast_dfs(&source, source.root(), &mut result, State { is_base: false, is_attachment: false });

    // Convert the hasmap into an array
    result.values().cloned().collect()
}
