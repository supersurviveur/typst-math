/// Styles for symbols rendering

use super::symbols::Color;

pub const SYMBOLS_STYLES: [(Color, &str); 7] = [
    (
        Color::KEYWORD,
        "font-family: \"NewComputerModernMath\"; font-weight: bold;",
    ),
    (
        Color::COMPARISON,
        "font-family: \"NewComputerModernMath\"; font-weight: bold;",
    ),
    (
        Color::OPERATOR,
        "font-family: \"Fira Math\";",
    ),
    (
        Color::NUMBER,
        "font-family: \"NewComputerModernMath\";",
    ),
    (
        Color::LETTER,
        "font-family: \"JuliaMono\";",
    ),
    (
        Color::SET,
        "font-family: \"Fira Math\";",
    ),
    // DEFAULT
    (
        Color::NUMBER,
        "",
    ),
];
