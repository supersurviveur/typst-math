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
        generator.mathSetSymbol(/emptyset/g, '∅'),
        generator.mathSetSymbol(/AA/g, '𝔸'),
        generator.mathSetSymbol(/BB/g, '𝔹'),
        generator.mathSetSymbol(/CC/g, 'ℂ'),
        generator.mathSetSymbol(/DD/g, '𝔻'),
        generator.mathSetSymbol(/EE/g, '𝔼'),
        generator.mathSetSymbol(/FF/g, '𝔽'),
        generator.mathSetSymbol(/GG/g, '𝔾'),
        generator.mathSetSymbol(/HH/g, 'ℍ'),
        generator.mathSetSymbol(/II/g, '𝕀'),
        generator.mathSetSymbol(/JJ/g, '𝕁'),
        generator.mathSetSymbol(/KK/g, '𝕂'),
        generator.mathSetSymbol(/LL/g, '𝕃'),
        generator.mathSetSymbol(/MM/g, '𝕄'),
        generator.mathSetSymbol(/NN/g, 'ℕ'),
        generator.mathSetSymbol(/OO/g, '𝕆'),
        generator.mathSetSymbol(/PP/g, 'ℙ'),
        generator.mathSetSymbol(/QQ/g, 'ℚ'),
        generator.mathSetSymbol(/RR/g, 'ℝ'),
        generator.mathSetSymbol(/SS/g, '𝕊'),
        generator.mathSetSymbol(/TT/g, '𝕋'),
        generator.mathSetSymbol(/UU/g, '𝕌'),
        generator.mathSetSymbol(/VV/g, '𝕍'),
        generator.mathSetSymbol(/WW/g, '𝕎'),
        generator.mathSetSymbol(/XX/g, '𝕏'),
        generator.mathSetSymbol(/YY/g, '𝕐'),
        generator.mathSetSymbol(/ZZ/g, 'ℤ'),
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

        generator.operatorSymbol(/plus\.minus/g, '±', startWordLimit, wordLimit),
        generator.operatorSymbol(/minus\.plus/g, '∓', startWordLimit, wordLimit),

        generator.operatorSymbol(/dot/g, '⋅', startWordLimit, wordLimit),
        generator.operatorSymbol(/star/g, '⋆', startWordLimit, wordLimit),
        generator.operatorSymbol(/circle\.tiny/g, '∘', startWordLimit, wordLimit),
        generator.operatorSymbol(/circle\.small/g, '⚬', startWordLimit, wordLimit),
        generator.operatorSymbol(/circle/g, '○', startWordLimit, wordLimit),

        generator.numberSymbol(/oo/g, '∞', startWordLimit, wordLimit),
        generator.numberSymbol(/infinity/g, '∞', startWordLimit, wordLimit),
        generator.numberSymbol(/dif/g, 'd', startWordLimit, wordLimit),
        generator.numberSymbol(/diff/g, '∂', startWordLimit, wordLimit),
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

    // literal powers
    generator.simpleRegex(
        /\^([A-z]\b|\([A-z]\))/g,
        "powers",
        {
            color: getColors("number"),
            textDecoration: `none;
            font-family: JuliaMono;
            font-size: 0.8em;
            transform: translateY(-30%);
            display: inline-block;`
        },
        (match) => {
            let letter = match[0].slice(1);
            // Remove paren if there is one'
            if (letter[0] === '(') {
                letter = letter.slice(1, -1);
            }
            return [
                letter,
                letter
            ];
        },
    );
    // literal negative powers
    generator.simpleRegex(
        /\^\(\-[A-z]\)/g,
        "powers",
        {
            color: getColors("number"),
            textDecoration: `none;
            font-family: JuliaMono;
            font-size: 0.8em;
            transform: translateY(-30%);
            display: inline-block;`
        },
        (match) => {
            const letter = match[0].slice(2, -1);
            return [
                letter,
                letter
            ];
        },
        undefined,
        undefined
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

    // literal subscripts
    generator.simpleRegex(
        /_([A-z]\b|\([A-z]\))/g, // match without or with paren
        "subscripts",
        {
            color: getColors("number"),
            textDecoration: `none;
            font-family: JuliaMono;
            font-size: 0.8em;
            letter-spacing: -0.15em;
            transform: translateY(20%);
            display: inline-block;
            padding-right: 0.1em;`,
        },
        (match) => {
            let letter = match[0].slice(1);
            // Remove paren if there is one'
            if (letter[0] === '(') {
                letter = letter.slice(1, -1);
            }
            return [
                letter,
                letter
            ];
        },
    );
    // literal negative subscripts
    generator.simpleRegex(
        /_\(\-[A-z]\)/g,
        "subscripts",
        {
            color: getColors("number"),
            textDecoration: `none;
            font-family: JuliaMono;
            font-size: 0.8em;
            letter-spacing: -0.15em;
            transform: translateY(20%);
            display: inline-block;
            padding-right: 0.1em;`,
        },
        (match) => {
            const letter = match[0].slice(2, -1);
            return [
                letter,
                letter
            ];
        },
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

    // Flatten allDecorations into result
    for (const key in getAllDecorations()) {
        for (const subKey in getAllDecorations()[key]) {
            result.push(getAllDecorations()[key][subKey]);
        }
    }

    return result;
}