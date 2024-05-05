mod interface;
mod utils;

use std::collections::HashMap;
use std::hash::Hash;

use interface::{Decoration, Position};
use typst_syntax::ast::FieldAccess;
use typst_syntax::{ast::Expr, SyntaxNode};
use utils::hook::set_panic_hook;
use wasm_bindgen::prelude::*;

use crate::utils::symbols::SYMBOLS;

#[wasm_bindgen]
pub fn init_lib() {
    set_panic_hook();
}

/// Use a recursive DFS to traverse the entire AST
pub fn ast_dfs(
    source: &typst_syntax::Source,
    node: &SyntaxNode,
    result: &mut HashMap<String, Decoration>,
) {
    for child in node.children() {
        if let Some(expr) = child.cast::<Expr>() {
            match expr {
                // Math identifier, check if it is in the symbols list
                Expr::MathIdent(ident) => {
                    if let Some(entry) = SYMBOLS.get_entry(ident.as_str()) {
                        let range = source.range(child.span()).expect("TODO source range error");
                        if let Some(map) = result.get_mut(&ident.to_string()) {
                            map.positions.push(Position {
                                start: range.start,
                                end: range.end,
                            });
                        } else {
                            result.insert(
                                ident.to_string(),
                                Decoration {
                                    content: ident.to_string(),
                                    symbol: entry.1.clone(),
                                    positions: vec![Position {
                                        start: range.start,
                                        end: range.end
                                    }],
                                },
                            );
                        }
                    }
                }
                // Field Access, create a string containing all fields sparated with a dot (alpha.alt), and check if it is in symbols list
                Expr::FieldAccess(access) => {
                    if let Some(content) = field_access_recursive(access) {
                        if let Some(entry) = SYMBOLS.get_entry(content.as_str()) {
                            let range =
                                source.range(child.span()).expect("TODO source range error");
                            if let Some(map) = result.get_mut(&content) {
                                map.positions.push(Position {
                                    start: range.start,
                                    end: range.end,
                                });
                            } else {
                                result.insert(
                                    content.clone(),
                                    Decoration {
                                        content: content,
                                        symbol: entry.1.clone(),
                                        positions: vec![Position {
                                            start: range.start,
                                            end: range.end
                                        }],
                                    },
                                );
                            }
                        }
                    }
                }
                _ => {
                    ast_dfs(source, child, result); // Propagate the function
                }
            }
        }
    }
}

fn field_access_recursive(access: FieldAccess) -> Option<String> {
    // Check if the target is a math identifier or another field access
    match access.target() {
        Expr::FieldAccess(subaccess) => {
            if let Some(start) = field_access_recursive(subaccess) {
                return Some(format!("{}.{}", start, access.field().to_string()));
            }
        }
        Expr::MathIdent(ident) => {
            return Some(format!(
                "{}.{}",
                ident.to_string(),
                access.field().to_string()
            ));
        }
        _ => {}
    }
    None
}

#[wasm_bindgen]
pub fn test(content: &str) -> Vec<Decoration> {
    let source = typst_syntax::Source::detached(content.to_string());
    println!("{:#?}", source.root());
    let mut result = HashMap::new();
    ast_dfs(&source, source.root(), &mut result);

    // Convert the hasmap into an array
    result.values().cloned().collect()
}
