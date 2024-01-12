import * as vscode from 'vscode';
import { DynamicGenerator } from './dynamicGenerator';
import { createDecorationType, getAllDecorations, staticSimpleRegex } from './helpers';

// Usefull regex
const wordLimit = /(?!\.)(\b|_|\n|\r)/g;
const startWordLimit = /[^\w\d\.]/g;
const arrowLimitLow = /[^=\-<>]/g;

// Get colors from settings
function getColors() {
    const config = vscode.workspace.getConfiguration('typst-math');
    const colors = config.get<{
        comparison: string,
        keyword: string,
        letter: string,
        group: string,
        operator: string,
        number: string,
    }>('colors');
    if (!colors) {
        throw new Error("Invalid colors");
    }
    return colors;
}

export function generateDecorations(): {
    decorationType: vscode.TextEditorDecorationType,
    getRanges: (document: vscode.TextEditor) => vscode.DecorationOptions[],
}[] {
    function helperSymbol(reg: RegExp, symbol: string, options: {
        color: string,
        textDecoration: string,
    }, pre?: RegExp, post?: RegExp) {
        return {
            getRanges: (activeEditor: vscode.TextEditor) =>
                staticSimpleRegex(activeEditor, reg, pre, post),
            decorationType: createDecorationType({
                color: options.color,
                textDecoration: options.textDecoration,
                contentText: symbol
            })
        };
    }
    function comparisonSymbol(reg: RegExp, symbol: string, pre?: RegExp, post?: RegExp) {
        return helperSymbol(reg, symbol, {
            color: getColors().comparison,
            textDecoration: 'none; font-family: "NewComputerModernMath"; font-weight: bold;',
        }, pre, post);
    }
    function keywordSymbol(reg: RegExp, symbol: string, pre?: RegExp, post?: RegExp) {
        return helperSymbol(reg, symbol, {
            color: getColors().keyword,
            textDecoration: 'none; font-family: "NewComputerModernMath"; font-weight: bold;'
        }, pre, post);
    }
    function letterSymbol(reg: RegExp, symbol: string) {
        return helperSymbol(reg, symbol, {
            color: getColors().letter,
            textDecoration: 'none; font-family: "JuliaMono";',
        });
    }
    function bigLetterSymbol(reg: RegExp, symbol: string) {
        return helperSymbol(reg, symbol, {
            color: getColors().letter,
            textDecoration: 'none; font-family: "NewComputerModernMath";',
        });
    }

    function mathSetSymbol(reg: RegExp, symbol: string) {
        return helperSymbol(reg, symbol, {
            color: getColors().group,
            textDecoration: `none;
            font-family: "Fira Math";`,
        });
    }

    function mathSetVariantsSymbol(reg: RegExp, symbol: string, style: string, pre?: RegExp, post?: RegExp) {
        return helperSymbol(reg, symbol, {
            color: getColors().group,
            textDecoration: `none;
            font-family: "JuliaMono";
            ${style}`,
        }, pre, post);
    }

    function mathExtendSetSymbol(reg: RegExp, symbol: string, pre?: RegExp, post?: RegExp) {
        return helperSymbol(reg, symbol, {
            color: getColors().group,
            textDecoration: `none;
            font-family: "Fira Math";`,
        }, pre, post);
    }

    function operatorSymbol(reg: RegExp, symbol: string, pre?: RegExp, post?: RegExp) {
        return helperSymbol(reg, symbol, {
            color: getColors().operator,
            textDecoration: `none;
            font-family: "Fira Math";`,
        }, pre, post);
    }

    const signVariants: [RegExp, string][] = [
        [/_\+/g, "₊"],
        [/_\-/g, "₋"]
    ];

    function generateSignedVariants() {
        const result = [];

        for (let variant of signVariants) {
            // Match only signed
            result.push(mathSetVariantsSymbol(
                variant[0],
                variant[1],
                ``,
                /\b([A-Z])\1/g,
                /(?!\^\*)/g // Don't match non-zero
            ));
            // Match non-zero then signed (for sign)
            result.push(mathSetVariantsSymbol(
                variant[0],
                variant[1],
                `transform: translateX(-0.37em);
                display: inline-block;`,
                /\b([A-Z])\1\^\*/g,
            ));
            // Match signed then non-zero (for sign)
            result.push(mathSetVariantsSymbol(
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
        comparisonSymbol(/=/g, '=', /[^:<>!=]/g, /[^:<>!=]/g), // TODO: avoid replacing char, just add style
        comparisonSymbol(/</g, '<', arrowLimitLow, arrowLimitLow),
        comparisonSymbol(/>/g, '>', arrowLimitLow, arrowLimitLow),
        comparisonSymbol(/eq\.triple/g, '≡', wordLimit, wordLimit),
        comparisonSymbol(/!=/g, '≠'),
        comparisonSymbol(/:=/g, '≔'),
        comparisonSymbol(/=>/g, '⇒', /[^<=]/g),
        comparisonSymbol(/==>/g, '⟹', /[^<]/g),
        comparisonSymbol(/<=>/g, '⇔', /[^<]/g),
        comparisonSymbol(/<==>/g, '⟺', /[^<]/g),
        comparisonSymbol(/<==/g, '⟸', /[^<]/g, /[^>]/g),
        comparisonSymbol(/<=/g, '≤', /[^<]/g, /[^>=]/g),
        comparisonSymbol(/>=/g, '≥', /[^>]/g, /[^>=]/g),
        comparisonSymbol(/->/g, '→', /[^-><\|]/g),
        comparisonSymbol(/-->/g, '⟶', /[^-><\|]/g),
        comparisonSymbol(/\|->/g, '↦'),
        comparisonSymbol(/<-/g, '←', undefined, /[^-><\|]/g),
        comparisonSymbol(/<--/g, '⟵', undefined, /[^-><\|]/g),
        comparisonSymbol(/<->/g, '↔'),
        comparisonSymbol(/<-->/g, '⟷'),

        comparisonSymbol(/dots\.h/g, '…', wordLimit, wordLimit),
        comparisonSymbol(/dots\.h\.c/g, '⋯', wordLimit, wordLimit),
        comparisonSymbol(/dots\.v/g, '⋮', wordLimit, wordLimit),
        comparisonSymbol(/dots\.up/g, '⋰', wordLimit, wordLimit),
        comparisonSymbol(/dots\.down/g, '⋱', wordLimit, wordLimit),

        // Keywords
        keywordSymbol(/forall\b\s?/g, '∀', /\b/g),
        keywordSymbol(/exists\b\s?/g, '∃', /\b/g),
        keywordSymbol(/in\b\s?/g, '∈', /\b/g),
        keywordSymbol(/in\.not\b\s?/g, '∉', /\b/g),

        // Greek letters
        letterSymbol(/alpha/g, 'α'),
        letterSymbol(/Alpha/g, 'Α'),
        letterSymbol(/beta/g, 'β'),
        letterSymbol(/Beta/g, 'Β'),
        letterSymbol(/beta\.alt/g, 'ϐ'),
        letterSymbol(/gamma/g, 'γ'),
        letterSymbol(/Gamma/g, 'Γ'),
        letterSymbol(/delta/g, 'δ'),
        letterSymbol(/Delta/g, 'Δ'),
        letterSymbol(/epsilon/g, 'ε'),
        letterSymbol(/epsilon\.alt/g, 'ϵ'),
        letterSymbol(/Epsilon/g, 'Ε'),
        letterSymbol(/zeta/g, 'ζ'),
        letterSymbol(/Zeta/g, 'Ζ'),
        letterSymbol(/eta/g, 'η'),
        letterSymbol(/Eta/g, 'Η'),
        letterSymbol(/theta/g, 'θ'),
        letterSymbol(/Theta/g, 'Θ'),
        letterSymbol(/theta\.alt/g, 'ϑ'),
        letterSymbol(/iota/g, 'ι'),
        letterSymbol(/Iota/g, 'Ι'),
        letterSymbol(/kappa/g, 'κ'),
        letterSymbol(/Kappa/g, 'Κ'),
        letterSymbol(/kappa\.alt/g, 'ϰ'),
        letterSymbol(/lambda/g, 'λ'),
        letterSymbol(/Lambda/g, 'Λ'),
        letterSymbol(/mu/g, 'μ'),
        letterSymbol(/Mu/g, 'Μ'),
        letterSymbol(/nu/g, 'ν'),
        letterSymbol(/Nu/g, 'Ν'),
        letterSymbol(/xi/g, 'ξ'),
        letterSymbol(/Xi/g, 'Ξ'),
        letterSymbol(/omicron/g, 'ο'),
        letterSymbol(/Omicron/g, 'Ο'),
        letterSymbol(/pi/g, 'π'),
        letterSymbol(/Pi/g, 'Π'),
        letterSymbol(/pi\.alt/g, 'ϖ'),
        letterSymbol(/rho/g, 'ρ'),
        letterSymbol(/Rho/g, 'Ρ'),
        letterSymbol(/rho\.alt/g, 'ϱ'),
        letterSymbol(/sigma/g, 'σ'),
        letterSymbol(/Sigma/g, 'Σ'),
        letterSymbol(/sigma\.alt/g, 'ς'),
        letterSymbol(/tau/g, 'τ'),
        letterSymbol(/Tau/g, 'Τ'),
        letterSymbol(/upsilon/g, 'υ'),
        letterSymbol(/Upsilon/g, 'Υ'),
        letterSymbol(/phi/g, 'φ'), // phi and phi.alt char are inverted, because Juliafont invert them
        letterSymbol(/Phi/g, 'Φ'),
        letterSymbol(/phi\.alt/g, 'ϕ'),
        letterSymbol(/chi/g, 'χ'),
        letterSymbol(/Chi/g, 'Χ'),
        letterSymbol(/psi/g, 'ψ'),
        letterSymbol(/Psi/g, 'Ψ'),
        letterSymbol(/omega/g, 'ω'),
        letterSymbol(/Omega/g, 'Ω'),

        // Big letters
        bigLetterSymbol(/sum/g, '∑'),
        bigLetterSymbol(/product/g, '∏'),
        bigLetterSymbol(/integral/g, '∫'),

        // Sets
        mathSetSymbol(/emptyset/g, '∅'),
        mathSetSymbol(/AA/g, '𝔸'),
        mathSetSymbol(/BB/g, '𝔹'),
        mathSetSymbol(/CC/g, 'ℂ'),
        mathSetSymbol(/DD/g, '𝔻'),
        mathSetSymbol(/EE/g, '𝔼'),
        mathSetSymbol(/FF/g, '𝔽'),
        mathSetSymbol(/GG/g, '𝔾'),
        mathSetSymbol(/HH/g, 'ℍ'),
        mathSetSymbol(/II/g, '𝕀'),
        mathSetSymbol(/JJ/g, '𝕁'),
        mathSetSymbol(/KK/g, '𝕂'),
        mathSetSymbol(/LL/g, '𝕃'),
        mathSetSymbol(/MM/g, '𝕄'),
        mathSetSymbol(/NN/g, 'ℕ'),
        mathSetSymbol(/OO/g, '𝕆'),
        mathSetSymbol(/PP/g, 'ℙ'),
        mathSetSymbol(/QQ/g, 'ℚ'),
        mathSetSymbol(/RR/g, 'ℝ'),
        mathSetSymbol(/SS/g, '𝕊'),
        mathSetSymbol(/TT/g, '𝕋'),
        mathSetSymbol(/UU/g, '𝕌'),
        mathSetSymbol(/VV/g, '𝕍'),
        mathSetSymbol(/WW/g, '𝕎'),
        mathSetSymbol(/XX/g, '𝕏'),
        mathSetSymbol(/YY/g, '𝕐'),
        mathSetSymbol(/ZZ/g, 'ℤ'),
        mathExtendSetSymbol(/\[/g, '[', undefined, /[^|]/g),
        mathExtendSetSymbol(/\]/g, ']', /[^|]/g),
        mathExtendSetSymbol(/\[\|/g, '\u{27E6}'),
        mathExtendSetSymbol(/\|\]/g, '\u{27E7}'),
        // Set variants
        // Match non-zero
        mathSetVariantsSymbol(
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
        mathSetVariantsSymbol(
            /\^\*/g,
            "*",
            `font-size: 0.6em;
            transform: translate(0.2em, -30%);
            display: inline-block;`,
            /\b([A-Z])\1/g,
            /_(\+|\-)/g
        ),
        // 4. Match signed then non-zero (for non-zero)
        mathSetVariantsSymbol(
            /\^\*/g,
            "*",
            `font-size: 0.6em;
            transform: translate(-0.8em, -30%);
            display: inline-block;`,
            /\b([A-Z])\1_(\+|\-)/g
        ),

        // Operators
        operatorSymbol(/plus/g, '+', startWordLimit, wordLimit),
        operatorSymbol(/\+/g, '+', /[^_]/g),
        operatorSymbol(/minus/g, '-', startWordLimit, wordLimit),
        operatorSymbol(/\-/g, '-', /[^_<\-]/g),
        operatorSymbol(/times/g, '×', startWordLimit, wordLimit),
        operatorSymbol(/\*/g, '\u{2217}', /[^\^]/g),
        operatorSymbol(/div/g, '÷', startWordLimit, wordLimit),

        operatorSymbol(/dot/g, '⋅', startWordLimit, wordLimit),
        operatorSymbol(/star/g, '⋆', startWordLimit, wordLimit),
        operatorSymbol(/circle\.tiny/g, '∘', startWordLimit, wordLimit),
        operatorSymbol(/circle\.small/g, '⚬', startWordLimit, wordLimit),
        operatorSymbol(/circle/g, '○', startWordLimit, wordLimit),
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