use typst_math_rust::parse_document;

/// Usefull to test the library in pure rust
fn main() {
    parse_document("$alpha^abs(alpha)$", -1, -1, 3, true, true, vec![], vec![]);
}
