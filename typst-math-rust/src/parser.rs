use crate::interface::{Decoration, Position};
use crate::utils::styles::SYMBOLS_STYLES;
use std::collections::HashMap;
use typst_syntax::ast::{AstNode, FieldAccess};
use typst_syntax::{ast::Expr, SyntaxNode};
use typst_syntax::{LinkedNode, Span, SyntaxKind};

use crate::utils::symbols::{Category, Color, SYMBOLS};

/// Helper function to insert a new symbol in the symbols hashmap
pub fn insert_result(
    source: &typst_syntax::Source,
    span: Span,
    uuid: String,
    symbol: String,
    color: Color,
    text_decoration: String,
    result: &mut HashMap<String, Decoration>,
    offset: (usize, usize),
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
                symbol: symbol,
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
    offset: (usize, usize),
    additional_content: (&str, &str),
) {
    if let Some(entry) = SYMBOLS.get_entry(&content.as_str()) {
        let (color, text_decoration) = get_style_from_category(entry.1.category);
        insert_result(
            source,
            span,
            uuid,
            format!(
                "{}{}{}",
                additional_content.0,
                entry.1.symbol.to_string(),
                additional_content.1,
            ),
            color,
            format!("{text_decoration} {added_text_decoration}"),
            result,
            offset,
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
    return (color, text_decoration);
}

fn inner_ast_dfs(
    source: &typst_syntax::Source,
    expr: Expr,
    child: Option<&SyntaxNode>,
    result: &mut HashMap<String, Decoration>,
    is_attachment: bool,
    uuid: &str,
    added_text_decoration: &str,
    offset: (usize, usize),
) {
    match expr {
        // Math identifier, check if it is in the symbols list
        Expr::MathIdent(ident) => {
            insert_result_symbol(
                source,
                expr.span(),
                ident.to_string(),
                format!("{uuid}{}", ident.to_string()),
                result,
                added_text_decoration,
                offset,
                ("", "")
            );
        }
        // Field Access, create a string containing all fields sparated with a dot (alpha.alt), and check if it is in symbols list
        Expr::FieldAccess(access) => {
            if let Some(content) = field_access_recursive(access) {
                insert_result_symbol(
                    source,
                    expr.span(),
                    content.clone(),
                    format!("{uuid}{}", content),
                    result,
                    added_text_decoration,
                    offset,
                    ("", "")
                );
            }
        }
        // Math attachement, power, subscript, superscript
        Expr::MathAttach(attachement) => {
            if !is_attachment {
                inner_ast_dfs(
                    source,
                    attachement.base(),
                    None,
                    result,
                    true,
                    uuid,
                    added_text_decoration,
                    offset
                );
            }
            if let Some(top) = attachement.top() {
                inner_ast_dfs(
                    source,
                    top,
                    None,
                    result,
                    true,
                    "top-",
                    "font-size: 0.8em; transform: translateY(-30%); display: inline-block;",
                    (1, 0),
                )
            }
            if let Some(bottom) = attachement.bottom() {
                inner_ast_dfs(
                    source,
                    bottom,
                    None,
                    result,
                    true,
                    "bottom-",
                    "font-size: 0.8em; transform: translateY(20%); display: inline-block;",
                    (1, 0),
                )
            }
        }
        Expr::Math(math) => {
            if let Some(child) = source.find(math.span()) {
                let children: Vec<LinkedNode> = child.children().collect();
                // If we are in an attachment, check if the current math block is just paren around a symbol
                if children.len() == 3
                    && is_attachment
                    && children[0].kind() == SyntaxKind::LeftParen
                    && children[1].kind() == SyntaxKind::Math
                    && children[2].kind() == SyntaxKind::RightParen
                {
                    let children: Vec<LinkedNode> = children[1].children().collect();

                    // Check if it's just a symbol
                    if children.len() == 1 && children[0].kind() == SyntaxKind::MathIdent {
                        match children[0].cast::<Expr>().unwrap() {
                            Expr::MathIdent(ident) => {
                                insert_result_symbol(
                                    source,
                                    ident.span(),
                                    ident.to_string(),
                                    format!("{uuid}{}", ident.to_string()),
                                    result,
                                    added_text_decoration, // Add style for superscript
                                    (1 + offset.0, 1 + offset.1),
                                    ("", "")
                                );
                                return; // return to avoid propagation
                            }
                            _ => {}
                        }
                    }
                    // Check if it's a symbol with a minus sign
                    else if children.len() == 2
                        && children[0].kind() == SyntaxKind::Shorthand
                        && children[1].kind() == SyntaxKind::MathIdent
                    {
                        let short = children[0].cast::<Expr>().unwrap();
                        match (short, children[1].cast::<Expr>().unwrap()) {
                            (Expr::Shorthand(short), Expr::MathIdent(ident)) => {
                                if short.get() == '\u{2212}' {
                                    insert_result_symbol(
                                        source,
                                        ident.span(),
                                        ident.to_string(),
                                        format!("{uuid}-minus-{}", ident.to_string()),
                                        result,
                                        added_text_decoration, // Add style for superscript
                                        (2 + offset.0, 1 + offset.1),
                                        ("-", "")
                                    );
                                    return; // return to avoid propagation
                                }
                            }
                            _ => {}
                        }
                    }
                }
                ast_dfs(source, &child, result, is_attachment); // Propagate the function
            }
        }
        _ => {
            if let Some(child) = child {
                ast_dfs(source, child, result, is_attachment); // Propagate the function
            }
        }
    }
}

/// Use a recursive DFS to traverse the entire AST
pub fn ast_dfs(
    source: &typst_syntax::Source,
    node: &SyntaxNode,
    result: &mut HashMap<String, Decoration>,
    is_attachment: bool,
) {
    for child in node.children() {
        if let Some(expr) = child.cast::<Expr>() {
            inner_ast_dfs(
                source,
                expr,
                Some(child),
                result,
                is_attachment,
                "",
                "",
                (0, 0),
            )
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
