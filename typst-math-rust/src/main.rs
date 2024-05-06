use typst_math_rust::parse_document;

fn main() {
    println!(
        "{:#?}",
        parse_document("é£ééééééééééééé $RR$ et soit $a in RR$, adhérent à $I$. Soit $n in NN$.")
    );
    println!(
        "{:#?}",
        parse_document("é *Hello* $wc alpha tilde(beta) beta^angle.l$")
    );
}
