use wasm_bindgen::prelude::*;

use crate::utils::symbols::{Color, Symbol};

/// A structure which represents a content which will be replaced in VSCode
#[derive(Debug, Clone)]
#[wasm_bindgen(getter_with_clone)]
pub struct Decoration {
    pub uuid: String,
    pub symbol: Symbol,
    pub color: Color,
    pub text_decoration: String,
    pub positions: Vec<Position>,
}

#[derive(Debug, Clone)]
#[wasm_bindgen]
pub struct Position {
    pub start: usize,
    pub end: usize,
}

