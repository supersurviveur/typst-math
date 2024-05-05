use typst_math_rust::test;


fn main() {
    println!("{:#?}", test("é£ééééééééééééé $RR$ et soit $a in RR$, adhérent à $I$. Soit $n in NN$."));
    println!("{:#?}", test("é *Hello* $wc alpha tilde(beta) beta^angle.l$"));
}
