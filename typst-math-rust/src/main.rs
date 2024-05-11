use typst_math_rust::parse_document;

/// Usefull to test the library in pure rust
fn main() {
    parse_document("$test$", 3, true, true);
}
