//! Utility functions for the parser

use super::parser::State;
use crate::{
    interface::{Decoration, Options, Position},
    utils::{
        styles::SYMBOLS_STYLES,
        symbols::{get_category_by_name, Category, Color, SYMBOLS},
    },
};
use std::{collections::HashMap, ops::Range};
use typst_syntax::SyntaxNode;
use typst_syntax::{ast::AstNode, LinkedNode, Source};

/// Get symbol from it's name
pub fn get_symbol(content: String, options: &Options) -> Option<(Category, String)> {
    // Check if the symbol is defined by the user
    if let Some(entry) = options.custom_symbols.get(&content) {
        return Some((get_category_by_name(&entry.category), entry.symbol.clone()));
    }
    // Check if the symbol is in the symbols list
    else if let Some(entry) = SYMBOLS.get_entry(&content.as_str()) {
        return Some((entry.1.category, format!("{}", entry.1.symbol)));
    }
    return None;
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

/// Cast expr to the given AST type. No checks are done, will panick if the given expression is not of the given type.
pub fn unchecked_cast_expr<'a, T: AstNode<'a>>(expr: &'a SyntaxNode) -> T {
    T::from_untyped(expr).unwrap()
}

/// The number of code units this string would use if it was encoded in
/// UTF16. This runs in linear time.
fn len_utf16(string: &str) -> usize {
    string.chars().map(char::len_utf16).sum()
}

/// Return the index range of the UTF-16 code unit at the byte index range. \
/// Faster than calling `byte_to_utf16` over start and end.
fn byte_range_to_utf16(source: &Source, range: &Range<usize>) -> Option<Range<usize>> {
    let start = source.byte_to_utf16(range.start)?;

    let head = source.get(range.start..range.end)?;
    let end = start + len_utf16(head);

    return Some(start..end);
}

/// Store the current data of the parsing
pub struct InnerParser<'a> {
    /// Source of the document
    pub source: &'a typst_syntax::Source,
    /// Vector containing decorations to render
    pub result: &'a mut HashMap<String, Decoration>,
    /// Current state of the parser
    pub state: &'a mut State,
    /// User settings
    pub options: &'a Options,

    /// Current expression
    pub expr: &'a LinkedNode<'a>,
    /// Current uuid applied to sub-decorations
    pub uuid: &'a str,
    /// Current css style applied to sub-decorations
    pub added_text_decoration: &'a str,
    /// Current offset applied to sub-decorations ranges'
    pub offset: (usize, usize),
}

impl<'a> InnerParser<'a> {
    /// Create a new parser
    pub fn new(
        source: &'a typst_syntax::Source,
        expr: &'a LinkedNode<'a>,
        result: &'a mut HashMap<String, Decoration>,
        state: &'a mut State,
        options: &'a Options,
    ) -> InnerParser<'a> {
        InnerParser {
            source,
            expr,
            result,
            state,
            uuid: "",
            added_text_decoration: "",
            offset: (0, 0),
            options,
        }
    }
    /// Create a new parser from another
    pub fn from(
        parser: &'a mut InnerParser,
        expr: &'a LinkedNode<'a>,
        uuid: &'a str,
        added_text_decoration: &'a str,
        offset: (usize, usize),
    ) -> InnerParser<'a> {
        InnerParser {
            source: parser.source,
            expr,
            result: parser.result,
            state: parser.state,
            uuid,
            added_text_decoration,
            offset,
            options: parser.options,
        }
    }
    /// Helper function to insert a new symbol in the symbols hashmap, with a symbol directly from the typst sym module
    pub fn insert_result_symbol(
        &mut self,
        range: Range<usize>,
        content: String,
        uuid: String,
        added_text_decoration: &str,
        offset: (usize, usize),
        additional_content: (&str, &str),
    ) {
        if let Some((category, symbol)) = get_symbol(content, self.options) {
            // If we are in a space and we don't want to render them, return
            if !self.options.render_spaces && category == Category::Space {
                return;
            }
            let (color, text_decoration) = get_style_from_category(category);
            self.insert_result(
                range,
                uuid,
                format!(
                    "{}{}{}",
                    additional_content.0,
                    symbol.to_string(),
                    additional_content.1,
                ),
                color,
                format!("{text_decoration} {added_text_decoration}"),
                offset,
            );
        }
    }
    /// Helper function to insert a new symbol in the symbols hashmap
    pub fn insert_result(
        &mut self,
        range: Range<usize>,
        uuid: String,
        symbol: String,
        color: Color,
        text_decoration: String,
        offset: (usize, usize),
    ) {
        // Convert position to UTF-16, because VSCode uses UTF-16 for positions
        let utf16_range = byte_range_to_utf16(self.source, &range).unwrap();
        let position = Position {
            start: utf16_range.start - offset.0,
            end: utf16_range.end + offset.1,
        };

        // Check if the symbol is blacklisted
        if self
            .options
            .blacklisted_symbols
            .contains(&self.source.get(range).unwrap_or("UNREACHABLE").to_string())
        {
            return;
        }

        // If the decoration already exists, simply add a new range
        if let Some(map) = self.result.get_mut(&uuid) {
            map.positions.push(position);
        } else {
            // If not, create the decoration and add this range
            self.result.insert(
                uuid.clone(),
                Decoration {
                    uuid,
                    symbol,
                    color,
                    text_decoration,
                    positions: vec![position],
                },
            );
        }
    }
    /// Helper function to insert a new invisible symbol in the symbols hashmap to hide a span
    pub fn insert_void(&mut self, range: Range<usize>, offset: (usize, usize)) {
        self.insert_result(
            range,
            "void".to_string(),
            "".to_string(),
            Color::Number,
            "".to_string(),
            offset,
        )
    }
}

#[cfg(test)]
mod tests {
    use typst_syntax::SyntaxNode;

    use crate::{interface::Options, parser::parser::State};

    #[test]
    fn test_inner_parser() {
        let source = typst_syntax::Source::detached("α");
        let mut result = std::collections::HashMap::new();
        let mut state = State::default();
        let options = Options::default();
        let node = SyntaxNode::leaf(typst_syntax::SyntaxKind::Ident, "alpha");
        let expr = typst_syntax::LinkedNode::new(&node);
        let mut parser = super::InnerParser::new(&source, &expr, &mut result, &mut state, &options);
        parser.insert_result_symbol(
            0..2,
            "alpha".to_string(),
            "alpha".to_string(),
            "",
            (0, 0),
            ("", ""),
        );

        let mut parser = super::InnerParser::from(&mut parser, &expr, "alpha", "", (0, 0));
        parser.insert_result_symbol(
            0..2,
            "alpha".to_string(),
            "alpha".to_string(),
            "",
            (0, 0),
            ("", ""),
        );
        assert_eq!(parser.result.len(), 1);
        assert_eq!(parser.result.get("alpha").unwrap().symbol, "α");
    }

    #[test]
    fn test_inner_parser_spaces() {
        let source = typst_syntax::Source::detached("zwnj");
        let mut result = std::collections::HashMap::new();
        let mut state = State::default();
        let mut options = Options::default();
        let node = SyntaxNode::leaf(typst_syntax::SyntaxKind::MathIdent, "zwnj");
        let expr = typst_syntax::LinkedNode::new(&node);
        let mut parser = super::InnerParser::new(&source, &expr, &mut result, &mut state, &options);
        parser.insert_result_symbol(
            0..4,
            "zwnj".to_string(),
            "zwnj".to_string(),
            "",
            (0, 0),
            ("", ""),
        );
        assert_eq!(parser.result.len(), 0);

        options.render_spaces = true;
        let mut parser = super::InnerParser::new(&source, &expr, &mut result, &mut state, &options);
        parser.insert_result_symbol(
            0..4,
            "zwnj".to_string(),
            "zwnj".to_string(),
            "",
            (0, 0),
            ("", ""),
        );
        assert_eq!(parser.result.len(), 1);
    }

    #[test]
    fn test_inner_parser_not_found() {
        let source = typst_syntax::Source::detached("");
        let mut result = std::collections::HashMap::new();
        let mut state = State::default();
        let options = Options::default();
        let node = SyntaxNode::leaf(typst_syntax::SyntaxKind::Ident, "alpha");
        let expr = typst_syntax::LinkedNode::new(&node);
        let mut parser = super::InnerParser::new(&source, &expr, &mut result, &mut state, &options);
        parser.insert_result_symbol(
            0..5,
            "doesn't exist".to_string(),
            "doesn't exist".to_string(),
            "",
            (0, 0),
            ("", ""),
        );
    }

    #[test]
    fn test_inner_parser_blacklist() {
        let source = typst_syntax::Source::detached("alpha");
        let mut result = std::collections::HashMap::new();
        let mut state = State::default();
        let mut options = Options::default();
        options.blacklisted_symbols.push("alpha".to_string());
        let node = SyntaxNode::leaf(typst_syntax::SyntaxKind::Ident, "alpha");
        let expr = typst_syntax::LinkedNode::new(&node);
        let mut parser = super::InnerParser::new(&source, &expr, &mut result, &mut state, &options);
        parser.insert_result_symbol(
            0..5,
            "alpha".to_string(),
            "alpha".to_string(),
            "",
            (0, 0),
            ("", ""),
        );
        assert_eq!(parser.result.len(), 0);
    }
}
