//! Utility functions for the parser

use std::collections::HashMap;

use typst_syntax::Span;

use crate::{
    interface::{Decoration, Options, Position},
    utils::{
        styles::SYMBOLS_STYLES,
        symbols::{Category, Color, PHYSICA_SYMBOLS, SYMBOLS},
    },
};

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
    options: &Options,
) {
    let range = source.range(span).expect("Span out of range");

    // Convert position to UTF-16, because VSCode uses UTF-16 for positions
    let position = Position {
        start: source.byte_to_utf16(range.start).unwrap() - offset.0,
        end: source.byte_to_utf16(range.end).unwrap() + offset.1,
    };

    // Check if the symbol is blacklisted
    if options
        .blacklisted_symbols
        .contains(&source.get(range).unwrap_or("UNREACHABLE").to_string())
    {
        return;
    }

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
/// Helper function to insert a new invisible symbol in the symbols hashmap to hide a span
pub fn insert_void(
    source: &typst_syntax::Source,
    span: Span,
    result: &mut HashMap<String, Decoration>,
    offset: (usize, usize),
    options: &Options,
) {
    insert_result(
        source,
        span,
        "void".to_string(),
        "".to_string(),
        Color::Number,
        "".to_string(),
        result,
        offset,
        options,
    )
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
    options: &Options,
) {
    let mut category = None;
    let mut symbol = None;
    if options.is_physica {
        if let Some(entry) = PHYSICA_SYMBOLS.get_entry(&content.as_str()) {
            category = Some(entry.1 .1);
            symbol = Some(entry.1 .0.to_string());
        }
    }
    // Check if the symbol is in the symbols list
    else if let Some(entry) = SYMBOLS.get_entry(&content.as_str()) {
        category = Some(entry.1.category);
        symbol = Some(format!("{}", entry.1.symbol));
    }
    if category.is_some() {
        let category = category.unwrap();
        let symbol = symbol.unwrap();
        // If we are in a space and we don't want to render them, return
        if !options.render_spaces && category == Category::Space {
            return;
        }
        let (color, text_decoration) = get_style_from_category(category);
        insert_result(
            source,
            span,
            uuid,
            format!(
                "{}{}{}",
                additional_content.0,
                symbol.to_string(),
                additional_content.1,
            ),
            color,
            format!("{text_decoration} {added_text_decoration}"),
            result,
            offset,
            options,
        );
    }
}

/// Get color and text_decoration css style from a symbol category
fn get_style_from_category(category: Category) -> (Color, std::string::String) {
    // Default values
    let mut color = Color::Number;
    let mut text_decoration = "".to_string();

    if let Some(style) = SYMBOLS_STYLES.get(category as usize) {
        color = style.0;
        text_decoration = style.1.to_string();
    }
    return (color, text_decoration);
}
