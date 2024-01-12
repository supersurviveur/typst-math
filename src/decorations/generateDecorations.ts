import * as vscode from 'vscode';
import { DynamicGenerator } from './dynamicGenerator';
import { getAllDecorations } from './helpers';
import { getColors } from './utils';
import { StaticGenerator } from './staticGenerator';

// Usefull regex
const wordLimit = /(?!\.)(\b|_|\n|\r)/g;
const startWordLimit = /[^\w\d\.]/g;
const arrowLimitLow = /[^=\-<>]/g;


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
        generator.comparisonSymbol(/eq\.triple/g, '≡', wordLimit, wordLimit),
        generator.comparisonSymbol(/!=/g, '≠'),
        generator.comparisonSymbol(/:=/g, '≔'),
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
        generator.keywordSymbol(/forall\b\s?/g, '∀', /\b/g),
        generator.keywordSymbol(/exists\b\s?/g, '∃', /\b/g),
        generator.keywordSymbol(/in\b\s?/g, '∈', /\b/g),
        generator.keywordSymbol(/in\.not\b\s?/g, '∉', /\b/g),

        // Greek letters
        generator.letterSymbol(/alpha/g, 'α'),
        generator.letterSymbol(/Alpha/g, 'Α'),
        generator.letterSymbol(/beta/g, 'β'),
        generator.letterSymbol(/Beta/g, 'Β'),
        generator.letterSymbol(/beta\.alt/g, 'ϐ'),
        generator.letterSymbol(/gamma/g, 'γ'),
        generator.letterSymbol(/Gamma/g, 'Γ'),
        generator.letterSymbol(/delta/g, 'δ'),
        generator.letterSymbol(/Delta/g, 'Δ'),
        generator.letterSymbol(/epsilon/g, 'ε'),
        generator.letterSymbol(/epsilon\.alt/g, 'ϵ'),
        generator.letterSymbol(/Epsilon/g, 'Ε'),
        generator.letterSymbol(/zeta/g, 'ζ'),
        generator.letterSymbol(/Zeta/g, 'Ζ'),
        generator.letterSymbol(/eta/g, 'η'),
        generator.letterSymbol(/Eta/g, 'Η'),
        generator.letterSymbol(/theta/g, 'θ'),
        generator.letterSymbol(/Theta/g, 'Θ'),
        generator.letterSymbol(/theta\.alt/g, 'ϑ'),
        generator.letterSymbol(/iota/g, 'ι'),
        generator.letterSymbol(/Iota/g, 'Ι'),
        generator.letterSymbol(/kappa/g, 'κ'),
        generator.letterSymbol(/Kappa/g, 'Κ'),
        generator.letterSymbol(/kappa\.alt/g, 'ϰ'),
        generator.letterSymbol(/lambda/g, 'λ'),
        generator.letterSymbol(/Lambda/g, 'Λ'),
        generator.letterSymbol(/mu/g, 'μ'),
        generator.letterSymbol(/Mu/g, 'Μ'),
        generator.letterSymbol(/nu/g, 'ν'),
        generator.letterSymbol(/Nu/g, 'Ν'),
        generator.letterSymbol(/xi/g, 'ξ'),
        generator.letterSymbol(/Xi/g, 'Ξ'),
        generator.letterSymbol(/omicron/g, 'ο'),
        generator.letterSymbol(/Omicron/g, 'Ο'),
        generator.letterSymbol(/pi/g, 'π'),
        generator.letterSymbol(/Pi/g, 'Π'),
        generator.letterSymbol(/pi\.alt/g, 'ϖ'),
        generator.letterSymbol(/rho/g, 'ρ'),
        generator.letterSymbol(/Rho/g, 'Ρ'),
        generator.letterSymbol(/rho\.alt/g, 'ϱ'),
        generator.letterSymbol(/sigma/g, 'σ'),
        generator.letterSymbol(/Sigma/g, 'Σ'),
        generator.letterSymbol(/sigma\.alt/g, 'ς'),
        generator.letterSymbol(/tau/g, 'τ'),
        generator.letterSymbol(/Tau/g, 'Τ'),
        generator.letterSymbol(/upsilon/g, 'υ'),
        generator.letterSymbol(/Upsilon/g, 'Υ'),
        generator.letterSymbol(/phi/g, 'φ'), // phi and phi.alt char are inverted, because Juliafont invert them
        generator.letterSymbol(/Phi/g, 'Φ'),
        generator.letterSymbol(/phi\.alt/g, 'ϕ'),
        generator.letterSymbol(/chi/g, 'χ'),
        generator.letterSymbol(/Chi/g, 'Χ'),
        generator.letterSymbol(/psi/g, 'ψ'),
        generator.letterSymbol(/Psi/g, 'Ψ'),
        generator.letterSymbol(/omega/g, 'ω'),
        generator.letterSymbol(/Omega/g, 'Ω'),

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
        generator.operatorSymbol(/\*/g, '\u{2217}', /[^\^]/g),
        generator.operatorSymbol(/div/g, '÷', startWordLimit, wordLimit),

        generator.operatorSymbol(/dot/g, '⋅', startWordLimit, wordLimit),
        generator.operatorSymbol(/star/g, '⋆', startWordLimit, wordLimit),
        generator.operatorSymbol(/circle\.tiny/g, '∘', startWordLimit, wordLimit),
        generator.operatorSymbol(/circle\.small/g, '⚬', startWordLimit, wordLimit),
        generator.operatorSymbol(/circle/g, '○', startWordLimit, wordLimit),

        generator.numberSymbol(/oo/g, '∞', startWordLimit, wordLimit),
        generator.numberSymbol(/dif/g, 'd', startWordLimit, wordLimit),
        generator.numberSymbol(/diff/g, '∂', startWordLimit, wordLimit),
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
        /\^\d+\b/g,
        "powers",
        {
            color: getColors().number,
            textDecoration: `none;
            font-family: JuliaMono;
            letter-spacing: -0.15em;
            transform: translateX(-0.15em);
            display: inline-block;`
        },
        (match) => {
            const number = match[0].slice(1);
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
            color: getColors().number,
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
        /\^[A-z]/g,
        "powers",
        {
            color: getColors().number,
            textDecoration: `none;
            font-family: JuliaMono;
            font-size: 0.8em;
            transform: translateY(-30%);
            display: inline-block;`
        },
        (match) => {
            const letter = match[0].slice(1);
            return [
                letter,
                letter
            ];
        },
        undefined,
        /\b/g
    );
    // literal negative powers
    generator.simpleRegex(
        /\^\(\-[A-z]\)/g,
        "powers",
        {
            color: getColors().number,
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
        /_\d+\b/g,
        "subscripts",
        {
            color: getColors().number,
            textDecoration: `none;
            font-family: JuliaMono;
            letter-spacing: -0.15em;
            transform: translate(-0.05em, 0.2em);
            display: inline-block;
            padding-right: 0.1em;`,
        },
        (match) => {
            const number = match[0].slice(1);
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
            color: getColors().number,
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
        /_[A-z]/g,
        "subscripts",
        {
            color: getColors().number,
            textDecoration: `none;
            font-family: JuliaMono;
            font-size: 0.8em;
            letter-spacing: -0.15em;
            transform: translateY(20%);
            display: inline-block;
            padding-right: 0.1em;`,
        },
        (match) => {
            const letter = match[0].slice(1);
            return [
                letter,
                letter
            ];
        },
        undefined,
        /\b/g
    );
    // literal negative subscripts
    generator.simpleRegex(
        /_\(\-[A-z]\)/g,
        "subscripts",
        {
            color: getColors().number,
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
        undefined,
        undefined
    );

    // Flatten allDecorations into result
    for (const key in getAllDecorations()) {
        for (const subKey in getAllDecorations()[key]) {
            result.push(getAllDecorations()[key][subKey]);
        }
    }

    return result;
}