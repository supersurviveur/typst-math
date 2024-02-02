import * as vscode from 'vscode';
import { DynamicGenerator } from './dynamicGenerator';
import { getAllDecorations } from './helpers';
import { getColors } from './utils';
import { StaticGenerator, arrowLimitLow, startWordLimit, wordLimit } from './staticGenerator';



export function generateDecorations(): {
    decorationType: vscode.TextEditorDecorationType,
    getRanges: (document: vscode.TextEditor) => vscode.DecorationOptions[],
}[] {
    // Usefull variables
    const generator = new StaticGenerator();

    const signVariants: [RegExp, string][] = [
        [/_\+/g, "₊"],
        [/_\-/g, "₋"]
    ];

    function generateSignedVariants() {
        const result = [];

        for (let variant of signVariants) {
            // Match only signed
            result.push(generator.mathSetVariantsSymbol(
                variant[0],
                variant[1],
                ``,
                /\b([A-Z])\1/g,
                /(?!\^\*)/g // Don't match non-zero
            ));
            // Match non-zero then signed (for sign)
            result.push(generator.mathSetVariantsSymbol(
                variant[0],
                variant[1],
                `transform: translateX(-0.37em);
                display: inline-block;`,
                /\b([A-Z])\1\^\*/g,
            ));
            // Match signed then non-zero (for sign)
            result.push(generator.mathSetVariantsSymbol(
                variant[0],
                variant[1],
                ``,
                /\b([A-Z])\1/g,
                /\^\*/g
            ));
        }
        return result;
    }
    return [
        // Three regex are used everywhere:
        // - reg: The main regex, matching the symbol we want to decorate
        // - pre: The regex to match the text before the symbol
        // - post: The regex to match the text after the symbol
        // pre and post avoid matching multiple times the same text with different decorations

        // comparison symbols
        generator.comparisonSymbol(/=/g, '=', /[^:<>!=]/g, /[^:<>!=]/g), // TODO: avoid replacing char, just add style
        generator.comparisonSymbol(/</g, '<', arrowLimitLow, arrowLimitLow),
        generator.comparisonSymbol(/>/g, '>', arrowLimitLow, arrowLimitLow),
        generator.comparisonSymbol(/<</g, '≪', arrowLimitLow, arrowLimitLow),
        generator.comparisonSymbol(/>>/g, '≫', arrowLimitLow, arrowLimitLow),
        generator.comparisonSymbol(/<<</g, '⋘', arrowLimitLow, arrowLimitLow),
        generator.comparisonSymbol(/>>>/g, '⋙', arrowLimitLow, arrowLimitLow),

        generator.comparisonSymbol(/eq\.triple/g, '≡', wordLimit, wordLimit),
        generator.comparisonSymbol(/equiv/g, '≡', wordLimit, wordLimit),
        generator.comparisonSymbol(/equiv\.not/g, '≢', wordLimit, wordLimit),
        generator.comparisonSymbol(/eq\.quad/g, '≣', wordLimit, wordLimit),
        generator.comparisonSymbol(/approx/g, '≈', wordLimit, wordLimit),
        generator.comparisonSymbol(/approx\.not/g, '≉', wordLimit, wordLimit),
        generator.comparisonSymbol(/approx\.eq/g, '≊', wordLimit, wordLimit),
        generator.comparisonSymbol(/tilde\.op/g, '∼', wordLimit, wordLimit),

        generator.comparisonSymbol(/!=/g, '≠'),
        generator.comparisonSymbol(/:=/g, '≔', /[^:]/g),
        generator.comparisonSymbol(/::=/g, '⩴'),
        generator.comparisonSymbol(/=>/g, '⇒', /[^<=]/g),
        generator.comparisonSymbol(/==>/g, '⟹', /[^<]/g),
        generator.comparisonSymbol(/<=>/g, '⇔', /[^<]/g),
        generator.comparisonSymbol(/<==>/g, '⟺', /[^<]/g),
        generator.comparisonSymbol(/<==/g, '⟸', /[^<]/g, /[^>]/g),
        generator.comparisonSymbol(/<=/g, '≤', /[^<]/g, /[^>=]/g),
        generator.comparisonSymbol(/>=/g, '≥', /[^>]/g, /[^>=]/g),
        generator.comparisonSymbol(/->/g, '→', /[^-><\|]/g),
        generator.comparisonSymbol(/-->/g, '⟶', /[^-><\|]/g),
        generator.comparisonSymbol(/\|->/g, '↦'),
        generator.comparisonSymbol(/<-/g, '←', undefined, /[^-><\|]/g),
        generator.comparisonSymbol(/<--/g, '⟵', undefined, /[^-><\|]/g),
        generator.comparisonSymbol(/<->/g, '↔'),
        generator.comparisonSymbol(/<-->/g, '⟷'),

        generator.comparisonSymbol(/dots\.h/g, '…', wordLimit, wordLimit),
        generator.comparisonSymbol(/dots\.h\.c/g, '⋯', wordLimit, wordLimit),
        generator.comparisonSymbol(/dots\.v/g, '⋮', wordLimit, wordLimit),
        generator.comparisonSymbol(/dots\.up/g, '⋰', wordLimit, wordLimit),
        generator.comparisonSymbol(/dots\.down/g, '⋱', wordLimit, wordLimit),

        // Keywords
        generator.keywordSymbol(/forall\s?/g, '∀', startWordLimit, wordLimit),
        generator.keywordSymbol(/exists\s?/g, '∃', startWordLimit, wordLimit),
        generator.keywordSymbol(/\s?in\s?/g, '∈', startWordLimit, wordLimit),
        generator.keywordSymbol(/\s?in\.not\s?/g, '∉', startWordLimit, wordLimit),
        generator.keywordSymbol(/\s?in\.small\s?/g, '∊', startWordLimit, wordLimit),
        generator.keywordSymbol(/\s?subset\s?/g, '⊂', startWordLimit, wordLimit),
        generator.keywordSymbol(/\s?subset\.not\s?/g, '⊄', startWordLimit, wordLimit),
        generator.keywordSymbol(/\s?subset\.eq\s?/g, '⊆', startWordLimit, wordLimit),
        generator.keywordSymbol(/\s?subset\.eq\.not\s?/g, '⊈', startWordLimit, wordLimit),
        generator.keywordSymbol(/\s?union\s?/g, '∪', startWordLimit, wordLimit),
        generator.keywordSymbol(/union\.big\s?/g, '⋃', startWordLimit, wordLimit),
        generator.keywordSymbol(/\s?sect\s?/g, '∩', startWordLimit, wordLimit),
        generator.keywordSymbol(/sect\.big\s?/g, '⋂', startWordLimit, wordLimit),
        generator.keywordSymbol(/complement\s?/g, '∁', startWordLimit, wordLimit),


        // Greek letters
        ...generator.letterSymbolWithVariants(/alpha/g, 'α'),
        ...generator.letterSymbolWithVariants(/Alpha/g, 'Α'),
        ...generator.letterSymbolWithVariants(/beta/g, 'β'),
        ...generator.letterSymbolWithVariants(/Beta/g, 'Β'),
        ...generator.letterSymbolWithVariants(/beta\.alt/g, 'ϐ'),
        ...generator.letterSymbolWithVariants(/gamma/g, 'γ'),
        ...generator.letterSymbolWithVariants(/Gamma/g, 'Γ'),
        ...generator.letterSymbolWithVariants(/delta/g, 'δ'),
        ...generator.letterSymbolWithVariants(/Delta/g, 'Δ'),
        ...generator.letterSymbolWithVariants(/epsilon/g, 'ε'),
        ...generator.letterSymbolWithVariants(/epsilon\.alt/g, 'ϵ'),
        ...generator.letterSymbolWithVariants(/Epsilon/g, 'Ε'),
        ...generator.letterSymbolWithVariants(/zeta/g, 'ζ'),
        ...generator.letterSymbolWithVariants(/Zeta/g, 'Ζ'),
        ...generator.letterSymbolWithVariants(/eta/g, 'η'),
        ...generator.letterSymbolWithVariants(/Eta/g, 'Η'),
        ...generator.letterSymbolWithVariants(/theta/g, 'θ'),
        ...generator.letterSymbolWithVariants(/Theta/g, 'Θ'),
        ...generator.letterSymbolWithVariants(/theta\.alt/g, 'ϑ'),
        ...generator.letterSymbolWithVariants(/iota/g, 'ι'),
        ...generator.letterSymbolWithVariants(/Iota/g, 'Ι'),
        ...generator.letterSymbolWithVariants(/kappa/g, 'κ'),
        ...generator.letterSymbolWithVariants(/Kappa/g, 'Κ'),
        ...generator.letterSymbolWithVariants(/kappa\.alt/g, 'ϰ'),
        ...generator.letterSymbolWithVariants(/lambda/g, 'λ'),
        ...generator.letterSymbolWithVariants(/Lambda/g, 'Λ'),
        ...generator.letterSymbolWithVariants(/mu/g, 'μ'),
        ...generator.letterSymbolWithVariants(/Mu/g, 'Μ'),
        ...generator.letterSymbolWithVariants(/nu/g, 'ν'),
        ...generator.letterSymbolWithVariants(/Nu/g, 'Ν'),
        ...generator.letterSymbolWithVariants(/xi/g, 'ξ'),
        ...generator.letterSymbolWithVariants(/Xi/g, 'Ξ'),
        ...generator.letterSymbolWithVariants(/omicron/g, 'ο'),
        ...generator.letterSymbolWithVariants(/Omicron/g, 'Ο'),
        ...generator.letterSymbolWithVariants(/pi/g, 'π'),
        ...generator.letterSymbolWithVariants(/Pi/g, 'Π'),
        ...generator.letterSymbolWithVariants(/pi\.alt/g, 'ϖ'),
        ...generator.letterSymbolWithVariants(/rho/g, 'ρ'),
        ...generator.letterSymbolWithVariants(/Rho/g, 'Ρ'),
        ...generator.letterSymbolWithVariants(/rho\.alt/g, 'ϱ'),
        ...generator.letterSymbolWithVariants(/sigma/g, 'σ'),
        ...generator.letterSymbolWithVariants(/Sigma/g, 'Σ'),
        ...generator.letterSymbolWithVariants(/sigma\.alt/g, 'ς'),
        ...generator.letterSymbolWithVariants(/tau/g, 'τ'),
        ...generator.letterSymbolWithVariants(/Tau/g, 'Τ'),
        ...generator.letterSymbolWithVariants(/upsilon/g, 'υ'),
        ...generator.letterSymbolWithVariants(/Upsilon/g, 'Υ'),
        ...generator.letterSymbolWithVariants(/phi/g, 'φ'), // phi and phi.alt char are inverted, because Juliafont invert them
        ...generator.letterSymbolWithVariants(/Phi/g, 'Φ'),
        ...generator.letterSymbolWithVariants(/phi\.alt/g, 'ϕ'),
        ...generator.letterSymbolWithVariants(/chi/g, 'χ'),
        ...generator.letterSymbolWithVariants(/Chi/g, 'Χ'),
        ...generator.letterSymbolWithVariants(/psi/g, 'ψ'),
        ...generator.letterSymbolWithVariants(/Psi/g, 'Ψ'),
        ...generator.letterSymbolWithVariants(/omega/g, 'ω'),
        ...generator.letterSymbolWithVariants(/Omega/g, 'Ω'),

        // Big letters
        generator.bigLetterSymbol(/sum/g, '∑'),
        generator.bigLetterSymbol(/product/g, '∏'),
        generator.bigLetterSymbol(/integral/g, '∫'),

        // Sets
        ...generator.mathSetSymbolWithVariants(/emptyset/g, '∅'),
        ...generator.mathSetSymbolWithVariants(/AA/g, '𝔸'),
        ...generator.mathSetSymbolWithVariants(/BB/g, '𝔹'),
        ...generator.mathSetSymbolWithVariants(/CC/g, 'ℂ'),
        ...generator.mathSetSymbolWithVariants(/DD/g, '𝔻'),
        ...generator.mathSetSymbolWithVariants(/EE/g, '𝔼'),
        ...generator.mathSetSymbolWithVariants(/FF/g, '𝔽'),
        ...generator.mathSetSymbolWithVariants(/GG/g, '𝔾'),
        ...generator.mathSetSymbolWithVariants(/HH/g, 'ℍ'),
        ...generator.mathSetSymbolWithVariants(/II/g, '𝕀'),
        ...generator.mathSetSymbolWithVariants(/JJ/g, '𝕁'),
        ...generator.mathSetSymbolWithVariants(/KK/g, '𝕂'),
        ...generator.mathSetSymbolWithVariants(/LL/g, '𝕃'),
        ...generator.mathSetSymbolWithVariants(/MM/g, '𝕄'),
        ...generator.mathSetSymbolWithVariants(/NN/g, 'ℕ'),
        ...generator.mathSetSymbolWithVariants(/OO/g, '𝕆'),
        ...generator.mathSetSymbolWithVariants(/PP/g, 'ℙ'),
        ...generator.mathSetSymbolWithVariants(/QQ/g, 'ℚ'),
        ...generator.mathSetSymbolWithVariants(/RR/g, 'ℝ'),
        ...generator.mathSetSymbolWithVariants(/SS/g, '𝕊'),
        ...generator.mathSetSymbolWithVariants(/TT/g, '𝕋'),
        ...generator.mathSetSymbolWithVariants(/UU/g, '𝕌'),
        ...generator.mathSetSymbolWithVariants(/VV/g, '𝕍'),
        ...generator.mathSetSymbolWithVariants(/WW/g, '𝕎'),
        ...generator.mathSetSymbolWithVariants(/XX/g, '𝕏'),
        ...generator.mathSetSymbolWithVariants(/YY/g, '𝕐'),
        ...generator.mathSetSymbolWithVariants(/ZZ/g, 'ℤ'),
        generator.mathExtendSetSymbol(/\[/g, '[', undefined, /[^|]/g),
        generator.mathExtendSetSymbol(/\]/g, ']', /[^|]/g),
        generator.mathExtendSetSymbol(/\[\|/g, '\u{27E6}'),
        generator.mathExtendSetSymbol(/\|\]/g, '\u{27E7}'),
        // Set variants
        // Match non-zero
        generator.mathSetVariantsSymbol(
            /\^\*/g,
            "*",
            `font-size: 0.6em;
            transform: translateY(-30%);
            display: inline-block;`,
            /\b([A-Z])\1/g,
            /[^_]/g // Don't match negative or positive
        ),
        // Match non-zero signed, using two different decorations, for better styling
        // To cover all cases, we need to do 4 regex
        // Only positive and negative set, and non-zero and signed (for sign)
        ...generateSignedVariants(),
        // 3. Match non-zero then signed (for non-zero)
        generator.mathSetVariantsSymbol(
            /\^\*/g,
            "*",
            `font-size: 0.6em;
            transform: translate(0.2em, -30%);
            display: inline-block;`,
            /\b([A-Z])\1/g,
            /_(\+|\-)/g
        ),
        // 4. Match signed then non-zero (for non-zero)
        generator.mathSetVariantsSymbol(
            /\^\*/g,
            "*",
            `font-size: 0.6em;
            transform: translate(-0.8em, -30%);
            display: inline-block;`,
            /\b([A-Z])\1_(\+|\-)/g
        ),

        // Operators
        generator.operatorSymbol(/plus/g, '+', startWordLimit, wordLimit),
        generator.operatorSymbol(/\+/g, '+', /[^_]/g),
        generator.operatorSymbol(/minus/g, '-', startWordLimit, wordLimit),
        generator.operatorSymbol(/\-/g, '-', /[^_<\-]/g),
        generator.operatorSymbol(/times/g, '×', startWordLimit, wordLimit),
        generator.operatorSymbol(/times\.big/g, '⨉', startWordLimit, wordLimit),
        generator.operatorSymbol(/\*/g, '\u{2217}', /[^\^]/g),
        generator.operatorSymbol(/div/g, '÷', startWordLimit, wordLimit),
        generator.operatorSymbol(/and/g, '∧', startWordLimit, wordLimit),
        generator.operatorSymbol(/and\.big/g, '⋀', startWordLimit, wordLimit),
        generator.operatorSymbol(/or/g, '∨', startWordLimit, wordLimit),
        generator.operatorSymbol(/or\.big/g, '⋁', startWordLimit, wordLimit),
        generator.operatorSymbol(/not/g, '¬', startWordLimit, wordLimit),
        generator.operatorSymbol(/divides/g, '∣', startWordLimit, wordLimit),
        generator.operatorSymbol(/divides\.not/g, '∤', startWordLimit, wordLimit),
        generator.operatorSymbol(/without/g, '∖', startWordLimit, wordLimit),

        generator.operatorSymbol(/plus\.minus/g, '±', startWordLimit, wordLimit),
        generator.operatorSymbol(/minus\.plus/g, '∓', startWordLimit, wordLimit),

        generator.operatorSymbol(/dot/g, '⋅', startWordLimit, /(?!\.)(_|\n|\r|\s|\^)/g),
        generator.operatorSymbol(/star/g, '⋆', startWordLimit, wordLimit),
        generator.operatorSymbol(/circle\.tiny/g, '∘', startWordLimit, wordLimit),
        generator.operatorSymbol(/circle\.stroked\.tiny/g, '∘', startWordLimit, wordLimit),
        generator.operatorSymbol(/circle\.small/g, '⚬', startWordLimit, wordLimit),
        generator.operatorSymbol(/circle/g, '○', startWordLimit, wordLimit),

        generator.numberSymbol(/oo/g, '∞', startWordLimit, wordLimit),
        generator.numberSymbol(/infinity/g, '∞', startWordLimit, wordLimit),
        generator.numberSymbol(/dif/g, 'd', startWordLimit, wordLimit),
        generator.numberSymbol(/diff/g, '∂', startWordLimit, wordLimit),
        generator.numberSymbol(/qed/g, '∎', startWordLimit, wordLimit),
        // Cal letters
        generator.numberSymbol(/cal\(A\)/g, '𝒜', startWordLimit),
        generator.numberSymbol(/cal\(B\)/g, 'ℬ', startWordLimit),
        generator.numberSymbol(/cal\(C\)/g, '𝒞', startWordLimit),
        generator.numberSymbol(/cal\(D\)/g, '𝒟', startWordLimit),
        generator.numberSymbol(/cal\(E\)/g, 'ℰ', startWordLimit),
        generator.numberSymbol(/cal\(F\)/g, 'ℱ', startWordLimit),
        generator.numberSymbol(/cal\(G\)/g, '𝒢', startWordLimit),
        generator.numberSymbol(/cal\(H\)/g, 'ℋ', startWordLimit),
        generator.numberSymbol(/cal\(I\)/g, 'ℐ', startWordLimit),
        generator.numberSymbol(/cal\(J\)/g, '𝒥', startWordLimit),
        generator.numberSymbol(/cal\(K\)/g, '𝒦', startWordLimit),
        generator.numberSymbol(/cal\(L\)/g, 'ℒ', startWordLimit),
        generator.numberSymbol(/cal\(M\)/g, 'ℳ', startWordLimit),
        generator.numberSymbol(/cal\(N\)/g, '𝒩', startWordLimit),
        generator.numberSymbol(/cal\(O\)/g, '𝒪', startWordLimit),
        generator.numberSymbol(/cal\(P\)/g, '𝒫', startWordLimit),
        generator.numberSymbol(/cal\(Q\)/g, '𝒬', startWordLimit),
        generator.numberSymbol(/cal\(R\)/g, 'ℛ', startWordLimit),
        generator.numberSymbol(/cal\(S\)/g, '𝒮', startWordLimit),
        generator.numberSymbol(/cal\(T\)/g, '𝒯', startWordLimit),
        generator.numberSymbol(/cal\(U\)/g, '𝒰', startWordLimit),
        generator.numberSymbol(/cal\(V\)/g, '𝒱', startWordLimit),
        generator.numberSymbol(/cal\(W\)/g, '𝒲', startWordLimit),
        generator.numberSymbol(/cal\(X\)/g, '𝒳', startWordLimit),
        generator.numberSymbol(/cal\(Y\)/g, '𝒴', startWordLimit),
        generator.numberSymbol(/cal\(Z\)/g, '𝒵', startWordLimit),
        generator.numberSymbol(/cal\(a\)/g, '𝒶', startWordLimit),
        generator.numberSymbol(/cal\(b\)/g, '𝒷', startWordLimit),
        generator.numberSymbol(/cal\(c\)/g, '𝒸', startWordLimit),
        generator.numberSymbol(/cal\(d\)/g, '𝒹', startWordLimit),
        generator.numberSymbol(/cal\(e\)/g, 'ℯ', startWordLimit),
        generator.numberSymbol(/cal\(f\)/g, '𝒻', startWordLimit),
        generator.numberSymbol(/cal\(g\)/g, 'ℊ', startWordLimit),
        generator.numberSymbol(/cal\(h\)/g, '𝒽', startWordLimit),
        generator.numberSymbol(/cal\(i\)/g, '𝒾', startWordLimit),
        generator.numberSymbol(/cal\(j\)/g, '𝒿', startWordLimit),
        generator.numberSymbol(/cal\(k\)/g, '𝓀', startWordLimit),
        generator.numberSymbol(/cal\(l\)/g, '𝓁', startWordLimit),
        generator.numberSymbol(/cal\(m\)/g, '𝓂', startWordLimit),
        generator.numberSymbol(/cal\(n\)/g, '𝓃', startWordLimit),
        generator.numberSymbol(/cal\(o\)/g, 'ℴ', startWordLimit),
        generator.numberSymbol(/cal\(p\)/g, '𝓅', startWordLimit),
        generator.numberSymbol(/cal\(q\)/g, '𝓆', startWordLimit),
        generator.numberSymbol(/cal\(r\)/g, '𝓇', startWordLimit),
        generator.numberSymbol(/cal\(s\)/g, '𝓈', startWordLimit),
        generator.numberSymbol(/cal\(t\)/g, '𝓉', startWordLimit),
        generator.numberSymbol(/cal\(u\)/g, '𝓊', startWordLimit),
        generator.numberSymbol(/cal\(v\)/g, '𝓋', startWordLimit),
        generator.numberSymbol(/cal\(w\)/g, '𝓌', startWordLimit),
        generator.numberSymbol(/cal\(x\)/g, '𝓍', startWordLimit),
        generator.numberSymbol(/cal\(y\)/g, '𝓎', startWordLimit),
        generator.numberSymbol(/cal\(z\)/g, '𝓏', startWordLimit),
        // Fraktur letters
        generator.numberSymbol(/frak\(A\)/g, '𝔄', startWordLimit),
        generator.numberSymbol(/frak\(B\)/g, '𝔅', startWordLimit),
        generator.numberSymbol(/frak\(C\)/g, 'ℭ', startWordLimit),
        generator.numberSymbol(/frak\(D\)/g, '𝔇', startWordLimit),
        generator.numberSymbol(/frak\(E\)/g, '𝔈', startWordLimit),
        generator.numberSymbol(/frak\(F\)/g, '𝔉', startWordLimit),
        generator.numberSymbol(/frak\(G\)/g, '𝔊', startWordLimit),
        generator.numberSymbol(/frak\(H\)/g, 'ℌ', startWordLimit),
        generator.numberSymbol(/frak\(I\)/g, 'ℑ', startWordLimit),
        generator.numberSymbol(/frak\(J\)/g, '𝔍', startWordLimit),
        generator.numberSymbol(/frak\(K\)/g, '𝔎', startWordLimit),
        generator.numberSymbol(/frak\(L\)/g, '𝔏', startWordLimit),
        generator.numberSymbol(/frak\(M\)/g, '𝔐', startWordLimit),
        generator.numberSymbol(/frak\(N\)/g, '𝔑', startWordLimit),
        generator.numberSymbol(/frak\(O\)/g, '𝔒', startWordLimit),
        generator.numberSymbol(/frak\(P\)/g, '𝔓', startWordLimit),
        generator.numberSymbol(/frak\(Q\)/g, '𝔔', startWordLimit),
        generator.numberSymbol(/frak\(R\)/g, 'ℜ', startWordLimit),
        generator.numberSymbol(/frak\(S\)/g, '𝔖', startWordLimit),
        generator.numberSymbol(/frak\(T\)/g, '𝔗', startWordLimit),
        generator.numberSymbol(/frak\(U\)/g, '𝔘', startWordLimit),
        generator.numberSymbol(/frak\(V\)/g, '𝔙', startWordLimit),
        generator.numberSymbol(/frak\(W\)/g, '𝔚', startWordLimit),
        generator.numberSymbol(/frak\(X\)/g, '𝔛', startWordLimit),
        generator.numberSymbol(/frak\(Y\)/g, '𝔜', startWordLimit),
        generator.numberSymbol(/frak\(Z\)/g, 'ℨ', startWordLimit),
        generator.numberSymbol(/frak\(a\)/g, '𝔞', startWordLimit),
        generator.numberSymbol(/frak\(b\)/g, '𝔟', startWordLimit),
        generator.numberSymbol(/frak\(c\)/g, '𝔠', startWordLimit),
        generator.numberSymbol(/frak\(d\)/g, '𝔡', startWordLimit),
        generator.numberSymbol(/frak\(e\)/g, '𝔢', startWordLimit),
        generator.numberSymbol(/frak\(f\)/g, '𝔣', startWordLimit),
        generator.numberSymbol(/frak\(g\)/g, '𝔤', startWordLimit),
        generator.numberSymbol(/frak\(h\)/g, '𝔥', startWordLimit),
        generator.numberSymbol(/frak\(i\)/g, '𝔦', startWordLimit),
        generator.numberSymbol(/frak\(j\)/g, '𝔧', startWordLimit),
        generator.numberSymbol(/frak\(k\)/g, '𝔨', startWordLimit),
        generator.numberSymbol(/frak\(l\)/g, '𝔩', startWordLimit),
        generator.numberSymbol(/frak\(m\)/g, '𝔪', startWordLimit),
        generator.numberSymbol(/frak\(n\)/g, '𝔫', startWordLimit),
        generator.numberSymbol(/frak\(o\)/g, '𝔬', startWordLimit),
        generator.numberSymbol(/frak\(p\)/g, '𝔭', startWordLimit),
        generator.numberSymbol(/frak\(q\)/g, '𝔮', startWordLimit),
        generator.numberSymbol(/frak\(r\)/g, '𝔯', startWordLimit),
        generator.numberSymbol(/frak\(s\)/g, '𝔰', startWordLimit),
        generator.numberSymbol(/frak\(t\)/g, '𝔱', startWordLimit),
        generator.numberSymbol(/frak\(u\)/g, '𝔲', startWordLimit),
        generator.numberSymbol(/frak\(v\)/g, '𝔳', startWordLimit),
        generator.numberSymbol(/frak\(w\)/g, '𝔴', startWordLimit),
        generator.numberSymbol(/frak\(x\)/g, '𝔵', startWordLimit),
        generator.numberSymbol(/frak\(y\)/g, '𝔶', startWordLimit),
        generator.numberSymbol(/frak\(z\)/g, '𝔷', startWordLimit),
        // blackboard bold letters
        generator.numberSymbol(/bb\(A\)/g, '𝔸', startWordLimit),
        generator.numberSymbol(/bb\(B\)/g, '𝔹', startWordLimit),
        generator.numberSymbol(/bb\(C\)/g, 'ℂ', startWordLimit),
        generator.numberSymbol(/bb\(D\)/g, '𝔻', startWordLimit),
        generator.numberSymbol(/bb\(E\)/g, '𝔼', startWordLimit),
        generator.numberSymbol(/bb\(F\)/g, '𝔽', startWordLimit),
        generator.numberSymbol(/bb\(G\)/g, '𝔾', startWordLimit),
        generator.numberSymbol(/bb\(H\)/g, 'ℍ', startWordLimit),
        generator.numberSymbol(/bb\(I\)/g, '𝕀', startWordLimit),
        generator.numberSymbol(/bb\(J\)/g, '𝕁', startWordLimit),
        generator.numberSymbol(/bb\(K\)/g, '𝕂', startWordLimit),
        generator.numberSymbol(/bb\(L\)/g, '𝕃', startWordLimit),
        generator.numberSymbol(/bb\(M\)/g, '𝕄', startWordLimit),
        generator.numberSymbol(/bb\(N\)/g, 'ℕ', startWordLimit),
        generator.numberSymbol(/bb\(O\)/g, '𝕆', startWordLimit),
        generator.numberSymbol(/bb\(P\)/g, 'ℙ', startWordLimit),
        generator.numberSymbol(/bb\(Q\)/g, 'ℚ', startWordLimit),
        generator.numberSymbol(/bb\(R\)/g, 'ℝ', startWordLimit),
        generator.numberSymbol(/bb\(S\)/g, '𝕊', startWordLimit),
        generator.numberSymbol(/bb\(T\)/g, '𝕋', startWordLimit),
        generator.numberSymbol(/bb\(U\)/g, '𝕌', startWordLimit),
        generator.numberSymbol(/bb\(V\)/g, '𝕍', startWordLimit),
        generator.numberSymbol(/bb\(W\)/g, '𝕎', startWordLimit),
        generator.numberSymbol(/bb\(X\)/g, '𝕏', startWordLimit),
        generator.numberSymbol(/bb\(Y\)/g, '𝕐', startWordLimit),
        generator.numberSymbol(/bb\(Z\)/g, 'ℤ', startWordLimit),
        generator.numberSymbol(/bb\(a\)/g, '𝕒', startWordLimit),
        generator.numberSymbol(/bb\(b\)/g, '𝕓', startWordLimit),
        generator.numberSymbol(/bb\(c\)/g, '𝕔', startWordLimit),
        generator.numberSymbol(/bb\(d\)/g, '𝕕', startWordLimit),
        generator.numberSymbol(/bb\(e\)/g, '𝕖', startWordLimit),
        generator.numberSymbol(/bb\(f\)/g, '𝕗', startWordLimit),
        generator.numberSymbol(/bb\(g\)/g, '𝕘', startWordLimit),
        generator.numberSymbol(/bb\(h\)/g, '𝕙', startWordLimit),
        generator.numberSymbol(/bb\(i\)/g, '𝕚', startWordLimit),
        generator.numberSymbol(/bb\(j\)/g, '𝕛', startWordLimit),
        generator.numberSymbol(/bb\(k\)/g, '𝕜', startWordLimit),
        generator.numberSymbol(/bb\(l\)/g, '𝕝', startWordLimit),
        generator.numberSymbol(/bb\(m\)/g, '𝕞', startWordLimit),
        generator.numberSymbol(/bb\(n\)/g, '𝕟', startWordLimit),
        generator.numberSymbol(/bb\(o\)/g, '𝕠', startWordLimit),
        generator.numberSymbol(/bb\(p\)/g, '𝕡', startWordLimit),
        generator.numberSymbol(/bb\(q\)/g, '𝕢', startWordLimit),
        generator.numberSymbol(/bb\(r\)/g, '𝕣', startWordLimit),
        generator.numberSymbol(/bb\(s\)/g, '𝕤', startWordLimit),
        generator.numberSymbol(/bb\(t\)/g, '𝕥', startWordLimit),
        generator.numberSymbol(/bb\(u\)/g, '𝕦', startWordLimit),
        generator.numberSymbol(/bb\(v\)/g, '𝕧', startWordLimit),
        generator.numberSymbol(/bb\(w\)/g, '𝕨', startWordLimit),
        generator.numberSymbol(/bb\(x\)/g, '𝕩', startWordLimit),
        generator.numberSymbol(/bb\(y\)/g, '𝕪', startWordLimit),
        generator.numberSymbol(/bb\(z\)/g, '𝕫', startWordLimit),
        generator.numberSymbol(/bb\(0\)/g, '𝟘', startWordLimit),
        generator.numberSymbol(/bb\(1\)/g, '𝟙', startWordLimit),
        generator.numberSymbol(/bb\(2\)/g, '𝟚', startWordLimit),
        generator.numberSymbol(/bb\(3\)/g, '𝟛', startWordLimit),
        generator.numberSymbol(/bb\(4\)/g, '𝟜', startWordLimit),
        generator.numberSymbol(/bb\(5\)/g, '𝟝', startWordLimit),
        generator.numberSymbol(/bb\(6\)/g, '𝟞', startWordLimit),
        generator.numberSymbol(/bb\(7\)/g, '𝟟', startWordLimit),
        generator.numberSymbol(/bb\(8\)/g, '𝟠', startWordLimit),
        generator.numberSymbol(/bb\(9\)/g, '𝟡', startWordLimit),


        ...generator.numberSymbolOnlyVariantsJulia(/a/g, 'a'),
        ...generator.numberSymbolOnlyVariantsJulia(/b/g, 'b'),
        ...generator.numberSymbolOnlyVariantsJulia(/c/g, 'c'),
        ...generator.numberSymbolOnlyVariantsJulia(/d/g, 'd'),
        ...generator.numberSymbolOnlyVariantsJulia(/e/g, 'e'),
        ...generator.numberSymbolOnlyVariantsJulia(/f/g, 'f'),
        ...generator.numberSymbolOnlyVariantsJulia(/g/g, 'g'),
        ...generator.numberSymbolOnlyVariantsJulia(/h/g, 'h'),
        ...generator.numberSymbolOnlyVariantsJulia(/i/g, 'i'),
        ...generator.numberSymbolOnlyVariantsJulia(/j/g, 'j'),
        ...generator.numberSymbolOnlyVariantsJulia(/k/g, 'k'),
        ...generator.numberSymbolOnlyVariantsJulia(/l/g, 'l'),
        ...generator.numberSymbolOnlyVariantsJulia(/m/g, 'm'),
        ...generator.numberSymbolOnlyVariantsJulia(/n/g, 'n'),
        ...generator.numberSymbolOnlyVariantsJulia(/o/g, 'o'),
        ...generator.numberSymbolOnlyVariantsJulia(/p/g, 'p'),
        ...generator.numberSymbolOnlyVariantsJulia(/q/g, 'q'),
        ...generator.numberSymbolOnlyVariantsJulia(/r/g, 'r'),
        ...generator.numberSymbolOnlyVariantsJulia(/s/g, 's'),
        ...generator.numberSymbolOnlyVariantsJulia(/t/g, 't'),
        ...generator.numberSymbolOnlyVariantsJulia(/u/g, 'u'),
        ...generator.numberSymbolOnlyVariantsJulia(/v/g, 'v'),
        ...generator.numberSymbolOnlyVariantsJulia(/w/g, 'w'),
        ...generator.numberSymbolOnlyVariantsJulia(/x/g, 'x'),
        ...generator.numberSymbolOnlyVariantsJulia(/y/g, 'y'),
        ...generator.numberSymbolOnlyVariantsJulia(/z/g, 'z'),
    ];
}


type dynamicDecorationType = {
    decorationType: vscode.TextEditorDecorationType,
    ranges: vscode.DecorationOptions[],
};

export function dynamicDecorations(activeEditor: vscode.TextEditor): dynamicDecorationType[] {
    const result: dynamicDecorationType[] = [];

    // Usefull variables
    const generator = new DynamicGenerator(activeEditor);

    // Reset decorations ranges
    // We don't reset entire decorations because we want to keep the decorationType
    for (let decorations in getAllDecorations()) {
        for (let decoration in getAllDecorations()[decorations]) {
            getAllDecorations()[decorations][decoration].ranges = [];
        }
    }

    // Per type decorations

    // Powers
    generator.simpleRegex(
        /\^(\d+\b|\(\d+\))/g,
        "powers",
        {
            color: getColors("number"),
            textDecoration: `none;
            font-family: JuliaMono;
            letter-spacing: -0.15em;
            transform: translateX(-0.15em);
            display: inline-block;`
        },
        (match) => {
            let number = match[0].slice(1);
            // Remove paren if there is one'
            if (number[0] === '(') {
                number = number.slice(1, -1);
            }
            const litNumbers = number.split('').map((n) => parseInt(n));
            return [
                number,
                litNumbers.map((n) => {
                    return ['⁰', '¹', '²', '³', '⁴', '⁵', '⁶', '⁷', '⁸', '⁹'][n];
                }).join('')
            ];
        }
    );

    // Negative powers
    generator.simpleRegex(
        /\^\(\-\d+\)/g,
        "powers",
        {
            color: getColors("number"),
            textDecoration: `none;
            font-family: JuliaMono;
            letter-spacing: -0.1em;
            transform: translateX(-0.15em);
            display: inline-block;`
        },
        (match) => {
            const number = match[0].slice(2);
            const litNumbers = number.split('').map((n) => parseInt(n));
            return [
                number,
                '⁻' + litNumbers.map((n) => {
                    return ['⁰', '¹', '²', '³', '⁴', '⁵', '⁶', '⁷', '⁸', '⁹'][n];
                }).join('')
            ];
        }
    );



    // Subscripts
    generator.simpleRegex(
        /_(\d+\b|\(\d+\))/g,
        "subscripts",
        {
            color: getColors("number"),
            textDecoration: `none;
            font-family: JuliaMono;
            letter-spacing: -0.15em;
            transform: translate(-0.05em, 0.2em);
            display: inline-block;
            padding-right: 0.1em;`,
        },
        (match) => {
            let number = match[0].slice(1);
            // Remove paren if there is one'
            if (number[0] === '(') {
                number = number.slice(1, -1);
            }
            const litNumbers = number.split('').map((n) => parseInt(n));
            return [
                number,
                litNumbers.map((n) => {
                    return ['₀', '₁', '₂', '₃', '₄', '₅', '₆', '₇', '₈', '₉'][n];
                }).join('')
            ];
        }
    );
    // Negative subscripts
    generator.simpleRegex(
        /_\(\-\d+\)/g,
        "subscripts",
        {
            color: getColors("number"),
            textDecoration: `none;
            font-family: JuliaMono;
            letter-spacing: -0.1em;
            transform: translate(-0.05em, 0.2em);
            display: inline-block;
            padding-right: 0.1em;`,
        },
        (match) => {
            const number = match[0].slice(3);
            const litNumbers = number.split('').map((n) => parseInt(n));
            return [
                number,
                '₋' + litNumbers.map((n) => {
                    return ['₀', '₁', '₂', '₃', '₄', '₅', '₆', '₇', '₈', '₉'][n];
                }).join('')
            ];
        }
    );

    // Third letters superscripts like k=0, n+1...
    generator.simpleRegex(
        /\^\([A-z](\+|\=|\-).\)/g,
        "powers",
        {
            color: getColors("number"),
            textDecoration: `none;
            font-family: JuliaMono;
            font-size: 0.8em;
            transform: translateY(-30%);
            display: inline-block;`,
        },
        (match) => {
            const content = match[0].slice(2, -1);
            return [
                content,
                content
            ];
        },
    );

    // Third letters subscripts like k=0, n+1...
    generator.simpleRegex(
        /_\([A-z](\+|\=|\-).\)/g,
        "subscripts",
        {
            color: getColors("number"),
            textDecoration: `none;
            font-family: JuliaMono;
            font-size: 0.8em;
            transform: translateY(20%);
            display: inline-block;`,
        },
        (match) => {
            const content = match[0].slice(2, -1);
            return [
                content,
                content
            ];
        },
    );

    // Arrow func on letters
    generator.simpleRegex(
        /arrow\([A-z0-9]/g,
        "combining",
        {
            color: getColors("number"),
            textDecoration: `none;`,
        },
        (match) => {
            const content = match[0].slice(6);
            return [
                content,
                content
            ];
        },
        startWordLimit,
        /\)/g
    );
    generator.simpleRegex(
        /\)/g,
        "combining",
        {
            color: getColors("number"),
            textDecoration: `none;
            font-family: "NewComputerModernMath";
            transform: translate(-0.84em, -0.9em);
            font-size: 0.8em;
            display: inline-block;`,
        },
        (match) => {
            const content = '→';
            return [
                content,
                content
            ];
        },
        /arrow\([A-z0-9]/g
    );
    
    // Tilde func on letters
    generator.simpleRegex(
        /tilde\([A-z0-9]/g,
        "combining",
        {
            color: getColors("number"),
            textDecoration: `none;`,
        },
        (match) => {
            const content = match[0].slice(6);
            return [
                content,
                content
            ];
        },
        startWordLimit,
        /\)/g
    );
    generator.simpleRegex(
        /\)/g,
        "combining",
        {
            color: getColors("number"),
            textDecoration: `none;
            transform: translate(-0.59em, -0.7em);
            font-size: 0.9em;
            display: inline-block;`,
        },
        (match) => {
            const content = '~';
            return [
                content,
                content
            ];
        },
        /tilde\([A-z0-9]/g,
    );

    // Hat func on letters
    generator.simpleRegex(
        /hat\([A-z0-9]/g,
        "combining",
        {
            color: getColors("number"),
            textDecoration: `none;`,
        },
        (match) => {
            const content = match[0].slice(4);
            return [
                content,
                content
            ];
        },
        startWordLimit,
        /\)/g
    );
    generator.simpleRegex(
        /\)/g,
        "combining",
        {
            color: getColors("number"),
            textDecoration: `none;
            transform: translate(-0.6em, -0.5em);
            font-size: 0.9em;
            display: inline-block;`,
        },
        (match) => {
            const content = '^';
            return [
                content,
                content
            ];
        },
        /hat\([A-z0-9]/g
    );

    // Dot func on letters
    generator.simpleRegex(
        /dot\([A-z0-9]/g,
        "combining",
        {
            color: getColors("number"),
            textDecoration: `none;`,
        },
        (match) => {
            const content = match[0].slice(4);
            return [
                content,
                content
            ];
        },
        startWordLimit,
        /\)/g
    );
    generator.simpleRegex(
        /\)/g,
        "combining",
        {
            color: getColors("number"),
            textDecoration: `none;
            transform: translate(-0.43em, -0.52em);
            display: inline-block;`,
        },
        (match) => {
            const content = '⋅';
            return [
                content,
                content
            ];
        },
        /dot\([A-z0-9]/g
    );

    // Double dot func on letters
    generator.simpleRegex(
        /dot\.double\([A-z0-9]/g,
        "combining",
        {
            color: getColors("number"),
            textDecoration: `none;`,
        },
        (match) => {
            const content = match[0].slice(11);
            return [
                content,
                content
            ];
        },
        startWordLimit,
        /\)/g
    );
    generator.simpleRegex(
        /\)/g,
        "combining",
        {
            color: getColors("number"),
            textDecoration: `none;
            transform: translate(-0.55em, -0.25em);
            display: inline-block;`,
        },
        (match) => {
            const content = '¨';
            return [
                content,
                content
            ];
        },
        /dot\.double\([A-z0-9]/g
    );

    // Triple dot func on letters
    generator.simpleRegex(
        /dot\.triple\([A-z0-9]/g,
        "combining",
        {
            color: getColors("number"),
            textDecoration: `none;`,
        },
        (match) => {
            const content = match[0].slice(11);
            return [
                content,
                content
            ];
        },
        startWordLimit,
        /\)/g
    );
    generator.simpleRegex(
        /\)/g,
        "combining",
        {
            color: getColors("number"),
            textDecoration: `none;
            font-family: JuliaMono;
            font-size: 1.4em;
            transform: translate(-0.5em);
            display: inline-block;`,
        },
        (match) => {
            const content = '\u20DB';
            return [
                content,
                content
            ];
        },
        /dot\.triple\([A-z0-9]/g
    );

    // Quad dot func on letters
    generator.simpleRegex(
        /dot\.quad\([A-z0-9]/g,
        "combining",
        {
            color: getColors("number"),
            textDecoration: `none;`,
        },
        (match) => {
            const content = match[0].slice(9);
            return [
                content,
                content
            ];
        },
        startWordLimit,
        /\)/g
    );
    generator.simpleRegex(
        /\)/g,
        "combining",
        {
            color: getColors("number"),
            textDecoration: `none;
            font-family: JuliaMono;
            font-size: 1.4em;
            transform: translate(-0.52em);
            display: inline-block;`,
        },
        (match) => {
            const content = '\u20DC';
            return [
                content,
                content
            ];
        },
        /dot\.quad\([A-z0-9]/g
    );

    // Overline func on letters
    generator.simpleRegex(
        /overline\([A-z0-9]/g,
        "combining",
        {
            color: getColors("number"),
            textDecoration: `none;`,
        },
        (match) => {
            const content = match[0].slice(9);
            return [
                content,
                content
            ];
        },
        startWordLimit,
        /\)/g
    );
    generator.simpleRegex(
        /\)/g,
        "combining",
        {
            color: getColors("number"),
            textDecoration: `none;
            transform: translate(-0.57em, -0.2em);
            display: inline-block;`,
        },
        (match) => {
            const content = '\u0305';
            return [
                content,
                content
            ];
        },
        /overline\([A-z0-9]/g
    );

    // Abs func on letters
    generator.simpleRegex(
        /a/g,
        "combining",
        {
            color: getColors("number"),
            textDecoration: `none;`,
        },
        (match) => {
            const content = '|';
            return [
                content,
                content
            ];
        },
        startWordLimit,
        /bs\([A-z0-9]\)/g
    );
    generator.simpleRegex(
        /bs\([A-z0-9]/g,
        "combining",
        {
            color: getColors("number"),
            textDecoration: `none;`,
        },
        (match) => {
            const content = match[0].slice(3);
            return [
                content,
                content
            ];
        },
        /a/g,
        /\)/g
    );
    generator.simpleRegex(
        /\)/g,
        "combining",
        {
            color: getColors("number"),
            textDecoration: `none;
            display: inline-block;`,
        },
        (match) => {
            const content = '|';
            return [
                content,
                content
            ];
        },
        /abs\([A-z0-9]/g
    );

    // Flatten allDecorations into result
    for (const key in getAllDecorations()) {
        for (const subKey in getAllDecorations()[key]) {
            result.push(getAllDecorations()[key][subKey]);
        }
    }

    return result;
}