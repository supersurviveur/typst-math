use wasm_bindgen::prelude::*;

use crate::utils::symbols::Color;

/// Represents a content which will be replaced in VSCode, with a specific style, position and color
/// uuid is used to identify decorations :
/// - rust side: in the decoraions hasmap
/// - js side: in the decorations array, to avoid generating the same decoration multiple times (Expensive)
#[derive(Debug, Clone)]
#[wasm_bindgen(getter_with_clone)]
pub struct Decoration {
    pub uuid: String,
    pub symbol: String,
    pub color: Color,
    pub text_decoration: String,
    pub positions: Vec<Position>,
}

/// Represents a symbol position in the document
#[derive(Debug, Clone)]
#[wasm_bindgen]
pub struct Position {
    pub start: usize,
    pub end: usize,
}

/// Represents the options for the rendering, set in the user settings
pub struct Options {
    pub rendering_mode: u8,
    pub render_outside_math: bool,
    pub render_spaces: bool,
}
