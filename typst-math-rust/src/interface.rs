use std::collections::HashMap;

use wasm_bindgen::prelude::*;

use crate::utils::symbols::Color;

/// Represents a content which will be replaced in VSCode, with a specific style, position and color
/// uuid is used to identify decorations :
/// - rust side: in the decoraions hasmap
/// - js side: in the decorations array, to avoid generating the same decoration multiple times (Expensive)
#[derive(Debug, Clone)]
#[cfg_attr(not(feature = "coverage"), wasm_bindgen(getter_with_clone))]
pub struct Decoration {
    pub uuid: String,
    pub symbol: String,
    pub color: Color,
    pub text_decoration: String,
    pub positions: Vec<Position>,
}

/// Represents a symbol position in the document
#[derive(Debug, Clone)]
#[cfg_attr(not(feature = "coverage"), wasm_bindgen)]
pub struct Position {
    pub start: usize,
    pub end: usize,
}

/// Represents the options for the rendering, set in the user settings
pub struct Options {
    pub rendering_mode: u8,
    pub render_outside_math: bool,
    pub render_spaces: bool,
    pub hide_unnecessary_delimiters: bool,
    pub hide_leading_and_trailing_quotes: bool,
    pub blacklisted_symbols: Vec<String>,
    pub custom_symbols: HashMap<String, CustomSymbol>,
}

impl Default for Options {
    fn default() -> Self {
        Options {
            rendering_mode: 3,
            render_outside_math: true,
            render_spaces: false,
            hide_unnecessary_delimiters: false,
            hide_leading_and_trailing_quotes: false,
            blacklisted_symbols: vec![],
            custom_symbols: HashMap::new(),
        }
    }
}

/// Represents a user defined symbol that can be used trough WASM
#[derive(Debug)]
#[cfg_attr(not(feature = "coverage"), wasm_bindgen(getter_with_clone))]
pub struct CustomSymbol {
    pub name: String,
    pub symbol: String,
    pub category: String,
}

/// Represents the result of the parsing function
#[cfg_attr(not(feature = "coverage"), wasm_bindgen(getter_with_clone))]
pub struct Parsed {
    pub decorations: Vec<Decoration>,
    pub edit_start_line: usize,
    pub edit_end_line: usize,
    pub edit_start_column: usize,
    pub edit_end_column: usize,
    pub erroneous: bool,
}
