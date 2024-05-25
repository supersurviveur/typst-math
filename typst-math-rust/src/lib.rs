mod interface;
mod parser;
mod utils;

// use std::time::Instant;
use std::{collections::HashMap, ops::Range};

use crate::parser::parser::State;
use interface::{CustomSymbol, Decoration, Options, Parsed};
use parser::{
    parser::{ast_dfs, inner_ast_dfs},
    utils::InnerParser,
};
use typst_syntax::{ast::Expr, LinkedNode, SyntaxNode};
use utils::hook::set_panic_hook;
use wasm_bindgen::prelude::*;

/// Initialize the WASM library
#[wasm_bindgen]
pub fn init_lib() {
    set_panic_hook();
}

/// Retrieve all nodes in a given range
pub fn find_node(range: Range<usize>, current: LinkedNode, nodes: &mut Vec<SyntaxNode>) {
    if current.range().start >= range.start && current.range().end <= range.end {
        nodes.push(current.get().clone())
    } else {
        for child in current.children() {
            find_node(range.clone(), child, nodes);
        }
    }
}

/// Parse a document and return the decorations to apply
#[wasm_bindgen]
pub fn parse_document(
    content: &str,
    edited_line_start: i32,
    edited_line_end: i32,
    rendering_mode: u8,
    render_outside_math: bool,
    render_spaces: bool,
    blacklisted_symbols: Vec<String>,
    custom_symbols: Vec<CustomSymbol>,
) -> Parsed {
    // Generate a fake source
    let mut source = typst_syntax::Source::detached(content.to_string());
    // println!("{:#?}", source.root());

    // These variable contains the range of the document that was parsed incrementally and will be returned to the extension
    let mut edit_start_line = 0;
    let mut edit_end_line = 0;
    let mut edit_start_column = 0;
    let mut edit_end_column = 0;
    // List of nodes to parse again
    let mut nodes = vec![];
    if edited_line_start >= 0 {
        // if edited_line_start is -1, we render the complete text
        let edited_range = source
            .line_to_range(edited_line_start as usize)
            .unwrap_or(
                source
                    .line_to_range(0)
                    .expect("No lines in the current source"),
            )
            .start
            ..source
                .line_to_range(edited_line_end as usize)
                .unwrap_or(
                    source
                        .line_to_range(source.len_lines() - 1)
                        .expect("Unreachable"),
                )
                .end;

        // Create a "fake" edit of the document (We don't change the content) to get the part which was reparsed
        let txt = source
            .get(edited_range.clone())
            .expect("Edited range outside source")
            .to_string();
        let range = source.edit(edited_range, txt.as_str());

        // Find all nodes in this range
        find_node(
            range.clone(),
            source.find(source.root().span()).unwrap(),
            &mut nodes,
        );

        if nodes.is_empty() {
            // If no nodes were found, parse the entire document
            nodes.push(source.root().clone());
        } else {
            // Get the range of part which will be reparsed
            let first = source.find(nodes.first().unwrap().span()).unwrap().range();
            let last = source.find(nodes.last().unwrap().span()).unwrap().range();
            edit_start_line = source.byte_to_line(first.start).unwrap();
            edit_end_line = source.byte_to_line(last.end).unwrap();
            edit_start_column = source.byte_to_column(first.start).unwrap();
            edit_end_column = source.byte_to_column(last.end).unwrap();
        }
    } else {
        // Parse the entire document
        nodes.push(source.root().clone());
    }

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

    let mut result: HashMap<String, Decoration> = HashMap::new();
    let options = Options {
        rendering_mode,
        render_outside_math,
        render_spaces,
        blacklisted_symbols,
        custom_symbols,
    };
    let mut state = State {
        is_base: false,
        is_attachment: false,
    };
    // Parse the AST produced by typst over nodes
    for node in nodes {
        let mut parser = InnerParser::new(&source, &node, &mut result, &mut state, &options);
        if let Some(expr) = node.cast::<Expr>() {
            inner_ast_dfs(&mut parser, expr, "", "", (0, 0))
        } else {
            ast_dfs(&mut parser, &node, "", "");
        }
    }

    // Convert the hasmap into an array
    Parsed {
        decorations: result.into_values().collect(),
        edit_start_line,
        edit_end_line,
        edit_start_column,
        edit_end_column,
        erroneous: source.root().erroneous(),
    }
}

/// Generate a custom symbol struct easily from JS
#[wasm_bindgen]
pub fn generate_custom_symbol(name: String, symbol: String, category: String) -> CustomSymbol {
    return CustomSymbol {
        name,
        symbol,
        category,
    };
}
