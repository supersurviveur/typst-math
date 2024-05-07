use crate::interface::{Decoration, Position};
use crate::utils::styles::SYMBOLS_STYLES;
use std::collections::HashMap;
use typst_syntax::ast::{AstNode, FieldAccess};
use typst_syntax::Span;
use typst_syntax::{ast::Expr, SyntaxNode};

use crate::utils::symbols::{Category, Color, Symbol, SYMBOLS};

/// Helper function to insert a new symbol in the symbols hashmap
pub fn insert_result(
    source: &typst_syntax::Source,
    span: Span,
    uuid: String,
    symbol: &Symbol,
    color: Color,
    text_decoration: String,
    result: &mut HashMap<String, Decoration>,
    offset: (usize, usize)
) {
    let range = source.range(span).expect("TODO source range error");

    let position = Position {
        start: source.byte_to_utf16(range.start).unwrap() - offset.0,
        end: source.byte_to_utf16(range.end).unwrap() + offset.1,
    };
    // If the decoration already exists, simply add a new range
    if let Some(map) = result.get_mut(&uuid) {
        map.positions.push(position);
    } else {
        // If not, create the decoration and add this range
        result.insert(
            uuid.clone(),
            Decoration {
                uuid,
                symbol: symbol.clone(),
                color,
                text_decoration,
                positions: vec![position],
            },
        );
    }
}
/// Helper function to insert a new symbol in the symbols hashmap, with a symbol directly from the typst sym module
pub fn insert_result_symbol(
    source: &typst_syntax::Source,
    span: Span,
    content: String,
    uuid: String,
    result: &mut HashMap<String, Decoration>,
    added_text_decoration: &str,
    offset: (usize, usize)
) {
    if let Some(entry) = SYMBOLS.get_entry(&content.as_str()) {
        let (color, text_decoration) = get_style_from_category(entry.1.category);
        insert_result(
            source,
            span,
            uuid,
            entry.1,
            color,
            format!("{text_decoration} {added_text_decoration}"),
            result,
            offset
        );
    }
}

/// Get color and text_decoration from a symbol category
fn get_style_from_category(category: Category) -> (Color, std::string::String) {
    let mut color = Color::NUMBER;
    let mut text_decoration = "".to_string();
    if let Some(style) = SYMBOLS_STYLES.get(category as usize) {
        color = style.0;
        text_decoration = style.1.to_string();
    }
    println!("{:#?}", color);
    return (color, text_decoration);
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
                    insert_result_symbol(source,child.span(), ident.to_string(), ident.to_string(), result, "", (0,0));
                }
                // Field Access, create a string containing all fields sparated with a dot (alpha.alt), and check if it is in symbols list
                Expr::FieldAccess(access) => {
                    if let Some(content) = field_access_recursive(access) {
                        insert_result_symbol(source, child.span(),content.clone(), content, result, "", (0,0));
                    }
                }
                // Math attachement, ppower, subscript, superscript
                Expr::MathAttach(attachement) => {
                    if let Some(top) = attachement.top() {
                        match top {
                            Expr::MathIdent(ident) => {
                                insert_result_symbol(
                                    source, 
                                    top.span(),
                                    ident.to_string(),
                                    format!("top-{}", ident.to_string()),
                                    result,
                                    "font-size: 0.8em; transform: translateY(-30%); display: inline-block;", // Add style for superscript
                                    (1,0)
                                );
                            }
                            _ => {}
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
