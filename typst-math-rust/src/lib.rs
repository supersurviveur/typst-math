mod utils;

use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    fn alert(s: &str);
}

#[wasm_bindgen]
pub fn testRS(content: &str) -> String {
    let result = typst_syntax::parse(content);
    let result = format!("{result:#?}");
    result
}
