use typst_math_rust::parse_document;

fn main() {
    println!(
        "{:#?}",
        parse_document("*Hello* $<=>_e^(2) wc alpha tilde(beta) beta^angle.l$")
    );
}
