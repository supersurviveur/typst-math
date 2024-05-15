mod interface;
mod parser;
mod utils;
extern crate web_sys;
use web_sys::console;

// use std::time::Instant;
use std::{collections::HashMap, ops::Range};

use crate::parser::parser::State;
use interface::{CustomSymbol, Decoration, Options, Parsed};
use parser::parser::{ast_dfs, inner_ast_dfs};
use typst_syntax::{ast::Expr, LinkedNode, SyntaxNode};
use utils::hook::set_panic_hook;
use wasm_bindgen::prelude::*;

/// Initialize the WASM library
#[wasm_bindgen]
pub fn init_lib() {
    set_panic_hook();
}

pub fn test(range: Range<usize>, current: LinkedNode, result: &mut Vec<SyntaxNode>) {
    if current.range().start >= range.start && current.range().end <= range.end {
        result.push(current.get().clone())
    } else {
        for child in current.children() {
            test(range.clone(), child, result);
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

    // let now = Instant::now();
    // let elapsed_time = now.elapsed();
    // println!("Running slow_function() took {} miliseconds.", elapsed_time.as_millis());
    // logger::info(format!("Hello from Rust! {}ms", elapsed_time.as_millis()).as_str());
    console::time_with_label("rust");
    let mut edit_start = 0;
    let mut edit_end = 0;
    let mut edit_start2 = 0;
    let mut edit_end2 = 0;
    // let mut root = source.root();
    let mut nodes = vec![];
    if edited_line_start >= 0 {
        let edited_range = source
            .line_to_range(edited_line_start as usize)
            .unwrap()
            .start
            ..source
                .line_to_range(edited_line_end as usize)
                .unwrap()
                .end;
        let txt = source.get(edited_range.clone()).unwrap().to_string();
        // console::log_1(&format!("{}", txt).as_str().into());
        let range = source.edit(edited_range, txt.as_str());
        test(
            range.clone(),
            source.find(source.root().span()).unwrap(),
            &mut nodes,
        );
        // root = node.get();
        // console::log_1(&format!("{:#?}", nodes).as_str().into());
        edit_start = source
            .byte_to_line(
                source
                    .find(nodes.first().unwrap().span())
                    .unwrap()
                    .range()
                    .start,
            )
            .unwrap();
        edit_end = source
            .byte_to_line(
                source
                    .find(nodes.last().unwrap().span())
                    .unwrap()
                    .range()
                    .end,
            )
            .unwrap();
        edit_start2 = source
            .byte_to_column(
                source
                    .find(nodes.first().unwrap().span())
                    .unwrap()
                    .range()
                    .start,
            )
            .unwrap();
        edit_end2 = source
            .byte_to_column(
                source
                    .find(nodes.last().unwrap().span())
                    .unwrap()
                    .range()
                    .end,
            )
            .unwrap();
    } else {
        nodes.push(source.root().clone());
    }
    console::time_end_with_label("rust");

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

    // Parse the AST produced by typst
    let mut result: HashMap<String, Decoration> = HashMap::new();
    let options = Options {
        rendering_mode,
        render_outside_math,
        render_spaces,
        blacklisted_symbols,
        custom_symbols,
    };
    for node in nodes {
        if let Some(expr) = node.cast::<Expr>() {
            inner_ast_dfs(
                &source,
                expr,
                Some(&node),
                &mut result,
                State {
                    is_base: false,
                    is_attachment: false,
                },
                "",
                "",
                (0, 0),
                &options,
            )
        } else {
            ast_dfs(
                &source,
                &node,
                &mut result,
                State {
                    is_base: false,
                    is_attachment: false,
                },
                &options,
            );
        }
        // ast_dfs(
        //     &source,
        //     &node,
        //     &mut result,
        //     State {
        //         is_base: false,
        //         is_attachment: false,
        //     },
        //     &options,
        // );
    }

    // Convert the hasmap into an array
    Parsed {
        decorations: result.into_values().collect(),
        edit_start,
        edit_end,
        edit_start2,
        edit_end2,
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
