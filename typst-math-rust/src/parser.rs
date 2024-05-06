use crate::interface::{Decoration, Position};
use crate::utils::styles::SYMBOLS_STYLES;
use std::collections::HashMap;
use typst_syntax::ast::FieldAccess;
use typst_syntax::Span;
use typst_syntax::{ast::Expr, SyntaxNode};

use crate::utils::symbols::{Color, Symbol, SYMBOLS};

/// Helper function to insert a new symbol in the symbols hashmap
pub fn insert_result(
    source: &typst_syntax::Source,
    span: Span,
    content: String,
    symbol: &Symbol,
    color: Color,
    text_decoration: String,
    result: &mut HashMap<String, Decoration>,
) {
    let range = source.range(span).expect("TODO source range error");

    let position = Position {
        start: source.byte_to_utf16(range.start).unwrap(),
        end: source.byte_to_utf16(range.end).unwrap(),
    };
    // If the decoration already exists, simply add a new range
    if let Some(map) = result.get_mut(&content) {
        map.positions.push(position);
    } else {
        // If not, create the decoration and add this range
        result.insert(
            content.clone(),
            Decoration {
                content,
                symbol: symbol.clone(),
                color,
                text_decoration,
                positions: vec![position],
            },
        );
    }
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
                        let mut color = Color::NUMBER;
                        let mut text_decoration = "".to_string();
                        if let Some(style) = SYMBOLS_STYLES.get_entry(entry.1.category.to_string().as_str()) {
                            color = style.1.0;
                            text_decoration = style.1.1.to_string();
                        }
                        insert_result(
                            source,
                            child.span(),
                            ident.to_string(),
                            entry.1,
                            color,
                            text_decoration,
                            result,
                        );
                    }
                }
                // Field Access, create a string containing all fields sparated with a dot (alpha.alt), and check if it is in symbols list
                Expr::FieldAccess(access) => {
                    if let Some(content) = field_access_recursive(access) {
                        if let Some(entry) = SYMBOLS.get_entry(content.as_str()) {
                            insert_result(
                                source,
                                child.span(),
                                content,
                                entry.1,
                                Color::NUMBER,
                                "".to_string(),
                                result,
                            );
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
