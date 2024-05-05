mod utils;

use typst_syntax::ast::Expr::MathIdent;
use typst_syntax::{ast::Expr, SyntaxNode};
use utils::hook::set_panic_hook;
use wasm_bindgen::prelude::*;

use crate::utils::symbols::SYMBOLS;

#[wasm_bindgen]
pub fn init_lib() {
    set_panic_hook();
}

/// Use a recursive DFS to traverse the entire AST
pub fn ast_dfs(node: &SyntaxNode) -> String {
    for child in node.children() {
        if let Some(expr) = child.cast::<Expr>() {
            match expr {
                MathIdent(x) => {
                    if let Some(entry) = SYMBOLS.get_entry(x.as_str()) {
                        println!("{x:#?} => {:#?}", entry.1);
                    } else {
                        println!("{x:#?}");
                    }
                }
                _ => {}
            }
        }
        ast_dfs(child);
    }
    "No".to_string()
}

#[wasm_bindgen]
pub fn test(content: &str) -> String {
    let result = typst_syntax::parse(content);
    println!("{result:#?}");
    let result = ast_dfs(&result);
    result
}
