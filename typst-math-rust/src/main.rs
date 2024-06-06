use typst_math_rust::parse_document;

/// Usefull to test the library in pure rust
fn main() {
    let parsed = parse_document("$alpha^((2))$", -1, -1, 3, true, true, true, vec![], vec![]);

    println!("{:?}", parsed.decorations);
}
