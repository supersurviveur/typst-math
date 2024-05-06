/// Styles for symbols rendering
use phf::phf_map;

use super::symbols::Color;

pub const SYMBOLS_STYLES: phf::Map<&str, (Color, &str)> = phf_map! {
    "KEYWORD" => (Color::KEYWORD, "")
};