import * as vscode from 'vscode';
import { DynamicGenerator } from './dynamicGenerator';
import { getAllDecorations } from './helpers';
import { getColors, renderingMode } from './utils';
import { StaticGenerator, arrowLimitLow, resetDecorationMap, startWordLimit, wordLimit } from './staticGenerator';
import fs from 'fs/promises';
import path from 'path';

let first_generation = true;
let showSymbols = true;

export function toggleSymbols() {
    showSymbols = !showSymbols;
}

export function resetGeneration() {
    first_generation = true;
    resetDecorationMap();
}

function stringToRegex(str: string) {
    return new RegExp(str.replace(/[.*+?^${}()|[\]\\]/g, '\\$&'));
}

interface JsonData {
    comparison: { [x: string]: string },
    arrows: { [x: string]: string },
    operators: { [x: string]: string },
    basics: { [x: string]: string },
    bigLetters: { [x: string]: string },
    keywords: { [x: string]: string },
    sets: { [x: string]: string },
    setsVariants: { [x: string]: string },
    greekLetters: { [x: string]: string },
}

interface OtherJsonData {
    symbols: {
        letter: string,
        cal: string,
        frak: string,
        bb: string,
    }[]
}

export async function generateDecorations(activeEditor: vscode.TextEditor): Promise<{
    decorationType: vscode.TextEditorDecorationType,
    getRanges: (document: vscode.TextEditor) => vscode.DecorationOptions[],
}[]> {
    if (renderingMode() === 0 || !showSymbols) {
        return [];
    }
    // Usefull variables
    const generator = new StaticGenerator(activeEditor);

    // open the symbols file
    let file = await fs.readFile(path.join(__dirname, 'symbols.json'));
    let data: JsonData = JSON.parse(file.toString());
    file = await fs.readFile(path.join(__dirname, 'othersymbols.json'));
    let otherData: OtherJsonData = JSON.parse(file.toString());

    const signVariants: [RegExp, string][] = [
        [/_\+/g, "₊"],
        [/_\-/g, "₋"]
    ];

    async function generateSignedVariants() {
        const result = [];

        for (let variant of signVariants) {
            // Match only signed
            result.push(await generator.mathSetVariantsSymbol(
                variant[0],
                variant[1],
                ``,
                /\b([A-Z])\1/g,
                /(?!\^\*)/g // Don't match non-zero
            ));
            // Match non-zero then signed (for sign)
            result.push(await generator.mathSetVariantsSymbol(
                variant[0],
                variant[1],
                `transform: translateX(-0.37em);
                display: inline-block;`,
                /\b([A-Z])\1\^\*/g,
            ));
            // Match signed then non-zero (for sign)
            result.push(await generator.mathSetVariantsSymbol(
                variant[0],
                variant[1],
                ``,
                /\b([A-Z])\1/g,
                /\^\*/g
            ));
        }
        return result;
    }

    let result = [
        // Three regex are used everywhere:
        // - reg: The main regex, matching the symbol we want to decorate
        // - pre: The regex to match the text before the symbol
        // - post: The regex to match the text after the symbol
        // pre and post avoid matching multiple times the same text with different decorations

        // comparison symbols
        await generator.comparisonSymbol(/=/g, '=', /[^:<>!=]/g, /[^:<>!=]/g), // TODO: avoid replacing char, just add style
        await generator.comparisonSymbol(/</g, '<', arrowLimitLow, arrowLimitLow),
        await generator.comparisonSymbol(/>/g, '>', arrowLimitLow, arrowLimitLow),
        await generator.comparisonSymbol(/<</g, '≪', arrowLimitLow, arrowLimitLow),
        await generator.comparisonSymbol(/>>/g, '≫', arrowLimitLow, arrowLimitLow),
        await generator.comparisonSymbol(/<<</g, '⋘', arrowLimitLow, arrowLimitLow),
        await generator.comparisonSymbol(/>>>/g, '⋙', arrowLimitLow, arrowLimitLow),


        await generator.comparisonSymbol(/!=/g, '≠'),
        await generator.comparisonSymbol(/:=/g, '≔', /[^:]/g),
        await generator.comparisonSymbol(/::=/g, '⩴'),
        await generator.comparisonSymbol(/=:/g, '≕', /[^:]/g),
        await generator.comparisonSymbol(/=>/g, '⇒', /[^<=\|]/g),
        await generator.comparisonSymbol(/\|=>/g, '⤇', /[^<=]/g),
        await generator.comparisonSymbol(/==>/g, '⟹', /[^<]/g),
        await generator.comparisonSymbol(/<=>/g, '⇔', /[^<]/g),
        await generator.comparisonSymbol(/<==>/g, '⟺', /[^<]/g),
        await generator.comparisonSymbol(/<==/g, '⟸', /[^<]/g, /[^>]/g),
        await generator.comparisonSymbol(/<=/g, '≤', /[^<]/g, /[^>=]/g),
        await generator.comparisonSymbol(/>=/g, '≥', /[^>]/g, /[^>=]/g),
        await generator.comparisonSymbol(/->/g, '→', /[^-><\|]/g, /[^>]/g),
        await generator.comparisonSymbol(/-->/g, '⟶', /[^-><\|]/g),
        await generator.comparisonSymbol(/\|->/g, '↦'),
        await generator.comparisonSymbol(/->>/g, '↠'),
        await generator.comparisonSymbol(/~>/g, '⇝', /[^~]/g),
        await generator.comparisonSymbol(/~~>/g, '⟿'),
        await generator.comparisonSymbol(/>->/g, '↣', /[^>]/g),
        await generator.comparisonSymbol(/<-/g, '←', /[^<]/g, /[^-><\|]/g),
        await generator.comparisonSymbol(/<--/g, '⟵', undefined, /[^-><\|]/g),
        await generator.comparisonSymbol(/<~/g, '⇜', undefined, /[^~]/g),
        await generator.comparisonSymbol(/<-</g, '↢'),
        await generator.comparisonSymbol(/<<-/g, '↞'),
        await generator.comparisonSymbol(/<~~/g, '⟷'),
        await generator.comparisonSymbol(/<->/g, '↔'),
        await generator.comparisonSymbol(/<-->/g, '⟷'),
        await generator.comparisonSymbol(/\|\|/g, '∥'),

        // operators
        await generator.operatorSymbol(/\+/g, '+', /[^_]/g),
        await generator.operatorSymbol(/\-/g, '−', /[^_<\-]/g),
        await generator.operatorSymbol(/\*/g, '\u{2217}', /[^\^]/g),

        // Sets
        await generator.mathExtendSetSymbol(/\[/g, '[', undefined, /[^|]/g),
        await generator.mathExtendSetSymbol(/\]/g, ']', /[^|]/g),
        await generator.mathExtendSetSymbol(/\[\|/g, '\u{27E6}'),
        await generator.mathExtendSetSymbol(/\|\]/g, '\u{27E7}'),
    ];

    let compare = data["comparison"];
    for (let value in compare) {
        let reg = stringToRegex(value);
        result.push(
            await generator.comparisonSymbol(reg, compare[value], startWordLimit, wordLimit)
        );
    }

    let arrows = data["arrows"];
    for (let value in arrows) {
        let reg = stringToRegex(value);
        result.push(
            await generator.comparisonSymbol(reg, arrows[value], startWordLimit, wordLimit)
        );
    }
    let operators = data["operators"];
    for (let value in operators) {
        let reg = stringToRegex(value);
        result.push(
            await generator.operatorSymbol(reg, operators[value], startWordLimit, wordLimit)
        );
    }
    let basics = data["basics"];
    for (let value in basics) {
        let reg = stringToRegex(value);
        result.push(
            await generator.numberSymbol(reg, basics[value], startWordLimit, wordLimit)
        );
    }
    let bigLetters = data["bigLetters"];
    for (let value in bigLetters) {
        let reg = stringToRegex(value);
        result.push(
            await generator.bigLetterSymbol(reg, bigLetters[value], startWordLimit, wordLimit)
        );
    }
    let keywords = data["keywords"];
    for (let value in keywords) {
        let reg = stringToRegex(value);
        result.push(
            await generator.keywordSymbol(reg, keywords[value], startWordLimit, wordLimit)
        );
    }
    let sets = data["sets"];
    for (let value in sets) {
        let reg = stringToRegex(value);
        result.push(
            await generator.mathSetSymbol(reg, sets[value], startWordLimit, wordLimit)
        );
    }
    let setsVariants = data["setsVariants"];
    for (let value in setsVariants) {
        let reg = stringToRegex(value);
        result = result.concat(
            ...await generator.mathSetSymbolWithVariants(reg, setsVariants[value])
        );
    }
    let greekLetters = data["greekLetters"];
    for (let value in greekLetters) {
        let reg = stringToRegex(value);
        result = result.concat(
            ...await generator.letterSymbolWithVariants(reg, greekLetters[value])
        );
    }

    let otherSymbols = otherData["symbols"];
    for (let symbol of otherSymbols) {
        result = result.concat(
            ...await generator.numberSymbolOnlyVariantsJulia(stringToRegex(symbol.letter), symbol.letter),
            await generator.numberSymbol(stringToRegex(`cal\(${symbol.letter}\)`), symbol.cal, startWordLimit),
            await generator.numberSymbol(stringToRegex(`frak\(${symbol.letter}\)`), symbol.frak, startWordLimit),
            await generator.numberSymbol(stringToRegex(`bb\(${symbol.letter}\)`), symbol.bb, startWordLimit)
        );
    }

    result = result.concat([
        // Set variants
        // Match non-zero
        await generator.mathSetVariantsSymbol(
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
        ...await generateSignedVariants(),
        // 3. Match non-zero then signed (for non-zero)
        await generator.mathSetVariantsSymbol(
            /\^\*/g,
            "*",
            `font-size: 0.6em;
            transform: translate(0.2em, -30%);
            display: inline-block;`,
            /\b([A-Z])\1/g,
            /_(\+|\-)/g
        ),
        // 4. Match signed then non-zero (for non-zero)
        await generator.mathSetVariantsSymbol(
            /\^\*/g,
            "*",
            `font-size: 0.6em;
            transform: translate(-0.8em, -30%);
            display: inline-block;`,
            /\b([A-Z])\1_(\+|\-)/g
        ),


        await generator.numberSymbol(/bb\(0\)/g, '𝟘', startWordLimit),
        await generator.numberSymbol(/bb\(1\)/g, '𝟙', startWordLimit),
        await generator.numberSymbol(/bb\(2\)/g, '𝟚', startWordLimit),
        await generator.numberSymbol(/bb\(3\)/g, '𝟛', startWordLimit),
        await generator.numberSymbol(/bb\(4\)/g, '𝟜', startWordLimit),
        await generator.numberSymbol(/bb\(5\)/g, '𝟝', startWordLimit),
        await generator.numberSymbol(/bb\(6\)/g, '𝟞', startWordLimit),
        await generator.numberSymbol(/bb\(7\)/g, '𝟟', startWordLimit),
        await generator.numberSymbol(/bb\(8\)/g, '𝟠', startWordLimit),
        await generator.numberSymbol(/bb\(9\)/g, '𝟡', startWordLimit),

        ...await generator.numberSymbolOnlyVariantsJulia(/0/g, '0', undefined, undefined, true),
        ...await generator.numberSymbolOnlyVariantsJulia(/1/g, '1', undefined, undefined, true),
        ...await generator.numberSymbolOnlyVariantsJulia(/2/g, '2', undefined, undefined, true),
        ...await generator.numberSymbolOnlyVariantsJulia(/3/g, '3', undefined, undefined, true),
        ...await generator.numberSymbolOnlyVariantsJulia(/4/g, '4', undefined, undefined, true),
        ...await generator.numberSymbolOnlyVariantsJulia(/5/g, '5', undefined, undefined, true),
        ...await generator.numberSymbolOnlyVariantsJulia(/6/g, '6', undefined, undefined, true),
        ...await generator.numberSymbolOnlyVariantsJulia(/7/g, '7', undefined, undefined, true),
        ...await generator.numberSymbolOnlyVariantsJulia(/8/g, '8', undefined, undefined, true),
        ...await generator.numberSymbolOnlyVariantsJulia(/9/g, '9', undefined, undefined, true),

    ]);
    if (first_generation && renderingMode() === 3) {
        first_generation = false;
        result = result.concat(await generator.generateFunctionVariants());
    }
    // Return result without null
    return result.filter((x) => x !== null) as any;
}


type dynamicDecorationType = {
    decorationType: vscode.TextEditorDecorationType,
    ranges: vscode.DecorationOptions[],
};

export async function dynamicDecorations(activeEditor: vscode.TextEditor): Promise<dynamicDecorationType[]> {
    const result: dynamicDecorationType[] = [];
    if (renderingMode() === 0 || !showSymbols) {
        return result;
    }

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

    // Subscripts between quotes
    generator.simpleRegex(
        /_\".*?\"/g,
        "subscripts_quotes",
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

    // Superscripts between quotes
    generator.simpleRegex(
        /\^\".*?\"/g,
        "powers_quotes",
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


    // Abs function for numbers
    generator.simpleRegex(
        /abs\(/g,
        "abs",
        {
            color: getColors("operator"),
            textDecoration: `none;`,
        },
        (match) => {
            const number = match[0].slice(3);
            return [
                number,
                "|"
            ];
        },
        startWordLimit,
        /-?\d+\)/g
    );
    generator.simpleRegex(
        /\)/g,
        "abs",
        {
            color: getColors("operator"),
            textDecoration: `none;`,
        },
        (match) => {
            const number = match[0].slice(0, -1);
            return [
                number,
                "|"
            ];
        },
        /abs\(-?\d+/g
    );

    // Flatten allDecorations into result
    for (const key in getAllDecorations()) {
        for (const subKey in getAllDecorations()[key]) {
            result.push(getAllDecorations()[key][subKey]);
        }
    }

    return result;
}