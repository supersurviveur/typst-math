use typst_math_rust::parse_document;

fn main() {
    parse_document("$alpha^beta_(-alpha)$");
}
