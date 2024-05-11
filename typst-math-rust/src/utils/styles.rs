/// Styles for symbols rendering
use super::symbols::Color;

/// Styles for symbols rendering, ordered by category
pub const SYMBOLS_STYLES: [(Color, &str); 9] = [
    // KEYWORDS
    (
        Color::Keyword,
        "font-family: \"NewComputerModernMath\"; font-weight: bold;",
    ),
    // COMPARISON
    (
        Color::Comparison,
        "font-family: \"NewComputerModernMath\"; font-weight: bold;",
    ),
    // OPERATORS
    (Color::Operator, "font-family: \"Fira Math\";"),
    // NUMBERS
    (Color::Number, "font-family: \"NewComputerModernMath\";"),
    // GREEK LETTERS
    (Color::Letter, "font-family: \"JuliaMono\";"),
    // BIG GREEK LETTERS
    (Color::Letter, "font-family: \"NewComputerModernMath\";"),
    // SETS
    (Color::Set, "font-family: \"Fira Math\";"),
    // SPACES
    (
        Color::Number,
        "box-shadow: 0px 0px 0px 1px rgba(128, 128, 128, 0.5); background-color: #80808080",
    ),
    // DEFAULT
    (Color::Number, "font-family: \"NewComputerModernMath\";"),
];
