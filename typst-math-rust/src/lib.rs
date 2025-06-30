mod interface;
mod parser;
mod utils;

use std::{collections::HashMap, ops::Range};

use crate::parser::parser::State;
use interface::{CustomSymbol, Decoration, Options, Parsed};
use parser::{parser::ast_dfs, utils::InnerParser};
use typst_syntax::LinkedNode;
use utils::hook::set_panic_hook;
#[cfg(not(feature = "coverage"))]
use wasm_bindgen::prelude::*;

/// Initialize the WASM library
#[cfg_attr(not(feature = "coverage"), wasm_bindgen)]
pub fn init_lib() {
    set_panic_hook();
}

/// Retrieve all nodes in a given range
pub fn find_node<'a>(
    range: Range<usize>,
    current: LinkedNode<'a>,
    nodes: &mut Vec<LinkedNode<'a>>,
) {
    if current.range().start >= range.start && current.range().end <= range.end {
        nodes.push(current.clone())
    } else {
        for child in current.children() {
            find_node(range.clone(), child, nodes);
        }
    }
}

/// Parse a document and return the decorations to apply
#[cfg_attr(not(feature = "coverage"), wasm_bindgen)]
pub fn parse_document(
    content: &str,
    edited_line_start: i32,
    edited_line_end: i32,
    rendering_mode: u8,
    render_outside_math: bool,
    render_spaces: bool,
    hide_unnecessary_delimiters: bool,
    hide_leading_and_trailing_quotes: bool,
    blacklisted_symbols: Vec<String>,
    custom_symbols: Vec<CustomSymbol>,
) -> Parsed {
    // Generate a fake source
    let mut source = typst_syntax::Source::detached(content.to_string());
    println!("{:#?}", source.root());

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

        let root = source.find(source.root().span()).unwrap();
        // Find all nodes in this range
        find_node(range.clone(), root.clone(), &mut nodes);

        // Get the range of part which will be reparsed
        let first = source.find(nodes.first().unwrap().span()).unwrap().range();
        let last = source.find(nodes.last().unwrap().span()).unwrap().range();
        edit_start_line = source.byte_to_line(first.start).unwrap();
        edit_end_line = source.byte_to_line(last.end).unwrap();
        edit_start_column = source.byte_to_column(first.start).unwrap();
        edit_end_column = source.byte_to_column(last.end).unwrap();
    } else {
        // Parse the entire document
        let root = source.find(source.root().span()).unwrap();
        nodes.push(root);
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
        hide_unnecessary_delimiters,
        hide_leading_and_trailing_quotes,
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
        ast_dfs(&mut parser, &node, "", "", (0, 0));
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
#[cfg_attr(not(feature = "coverage"), wasm_bindgen)]
pub fn generate_custom_symbol(name: String, symbol: String, category: String) -> CustomSymbol {
    return CustomSymbol {
        name,
        symbol,
        category,
    };
}

#[cfg(test)]
mod tests {
    use crate::{generate_custom_symbol, init_lib, parse_document};

    #[test]
    fn test_initialization() {
        init_lib();
    }

    #[test]
    fn test_custom_symbols() {
        let parsed = parse_document(
            "$alpha symbol$",
            -1,
            -1,
            3,
            true,
            true,
            false,
            false,
            vec![],
            vec![generate_custom_symbol(
                "symbol".to_string(),
                "symbol".to_string(),
                "operator".to_string(),
            )],
        );
        assert_eq!(parsed.decorations.len(), 2);
    }
}
