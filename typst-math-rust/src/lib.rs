mod utils;

use typst_syntax::{ast::MathIdent, SyntaxNode};
use utils::set_panic_hook;
use wasm_bindgen::prelude::*;
use typst_syntax::ast::Expr::MathIdent;

#[wasm_bindgen]
pub fn init_lib() {
    set_panic_hook();
}

pub fn parcours(node: &SyntaxNode) -> String {
    for c in node.children() {
        match c {
            MathIdent(x) => {

            }
        }
        if c.is::<MathIdent>() {
            format!("{c:#?}")
        }
        parcours(c);
    }
    "No".to_string()
}

#[wasm_bindgen]
pub fn test(content: &str) -> String {
    let result = typst_syntax::parse(content);
    let result = parcours(&result);
    result
}
