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
        await generator.comparisonSymbol(/=>/g, '⇒', /[^<=]/g),
        await generator.comparisonSymbol(/==>/g, '⟹', /[^<]/g),
        await generator.comparisonSymbol(/<=>/g, '⇔', /[^<]/g),
        await generator.comparisonSymbol(/<==>/g, '⟺', /[^<]/g),
        await generator.comparisonSymbol(/<==/g, '⟸', /[^<]/g, /[^>]/g),
        await generator.comparisonSymbol(/<=/g, '≤', /[^<]/g, /[^>=]/g),
        await generator.comparisonSymbol(/>=/g, '≥', /[^>]/g, /[^>=]/g),
        await generator.comparisonSymbol(/->/g, '→', /[^-><\|]/g),
        await generator.comparisonSymbol(/-->/g, '⟶', /[^-><\|]/g),
        await generator.comparisonSymbol(/\|->/g, '↦'),
        await generator.comparisonSymbol(/<-/g, '←', undefined, /[^-><\|]/g),
        await generator.comparisonSymbol(/<--/g, '⟵', undefined, /[^-><\|]/g),
        await generator.comparisonSymbol(/<->/g, '↔'),
        await generator.comparisonSymbol(/<-->/g, '⟷'),
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

    result = result.concat([
        // Sets
        await generator.mathExtendSetSymbol(/\[/g, '[', undefined, /[^|]/g),
        await generator.mathExtendSetSymbol(/\]/g, ']', /[^|]/g),
        await generator.mathExtendSetSymbol(/\[\|/g, '\u{27E6}'),
        await generator.mathExtendSetSymbol(/\|\]/g, '\u{27E7}'),
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

        // await generator.numberSymbol(/oo/g, '∞', startWordLimit, wordLimit),
        // await generator.numberSymbol(/infinity/g, '∞', startWordLimit, wordLimit),
        // await generator.numberSymbol(/dif/g, 'd', startWordLimit, wordLimit),
        // await generator.numberSymbol(/diff/g, '∂', startWordLimit, wordLimit),
        // await generator.numberSymbol(/nabla/g, '∇', startWordLimit, wordLimit),
        // await generator.numberSymbol(/qed/g, '∎', startWordLimit, wordLimit),
        // Cal letters
        await generator.numberSymbol(/cal\(A\)/g, '𝒜', startWordLimit),
        await generator.numberSymbol(/cal\(B\)/g, 'ℬ', startWordLimit),
        await generator.numberSymbol(/cal\(C\)/g, '𝒞', startWordLimit),
        await generator.numberSymbol(/cal\(D\)/g, '𝒟', startWordLimit),
        await generator.numberSymbol(/cal\(E\)/g, 'ℰ', startWordLimit),
        await generator.numberSymbol(/cal\(F\)/g, 'ℱ', startWordLimit),
        await generator.numberSymbol(/cal\(G\)/g, '𝒢', startWordLimit),
        await generator.numberSymbol(/cal\(H\)/g, 'ℋ', startWordLimit),
        await generator.numberSymbol(/cal\(I\)/g, 'ℐ', startWordLimit),
        await generator.numberSymbol(/cal\(J\)/g, '𝒥', startWordLimit),
        await generator.numberSymbol(/cal\(K\)/g, '𝒦', startWordLimit),
        await generator.numberSymbol(/cal\(L\)/g, 'ℒ', startWordLimit),
        await generator.numberSymbol(/cal\(M\)/g, 'ℳ', startWordLimit),
        await generator.numberSymbol(/cal\(N\)/g, '𝒩', startWordLimit),
        await generator.numberSymbol(/cal\(O\)/g, '𝒪', startWordLimit),
        await generator.numberSymbol(/cal\(P\)/g, '𝒫', startWordLimit),
        await generator.numberSymbol(/cal\(Q\)/g, '𝒬', startWordLimit),
        await generator.numberSymbol(/cal\(R\)/g, 'ℛ', startWordLimit),
        await generator.numberSymbol(/cal\(S\)/g, '𝒮', startWordLimit),
        await generator.numberSymbol(/cal\(T\)/g, '𝒯', startWordLimit),
        await generator.numberSymbol(/cal\(U\)/g, '𝒰', startWordLimit),
        await generator.numberSymbol(/cal\(V\)/g, '𝒱', startWordLimit),
        await generator.numberSymbol(/cal\(W\)/g, '𝒲', startWordLimit),
        await generator.numberSymbol(/cal\(X\)/g, '𝒳', startWordLimit),
        await generator.numberSymbol(/cal\(Y\)/g, '𝒴', startWordLimit),
        await generator.numberSymbol(/cal\(Z\)/g, '𝒵', startWordLimit),
        await generator.numberSymbol(/cal\(a\)/g, '𝒶', startWordLimit),
        await generator.numberSymbol(/cal\(b\)/g, '𝒷', startWordLimit),
        await generator.numberSymbol(/cal\(c\)/g, '𝒸', startWordLimit),
        await generator.numberSymbol(/cal\(d\)/g, '𝒹', startWordLimit),
        await generator.numberSymbol(/cal\(e\)/g, 'ℯ', startWordLimit),
        await generator.numberSymbol(/cal\(f\)/g, '𝒻', startWordLimit),
        await generator.numberSymbol(/cal\(g\)/g, 'ℊ', startWordLimit),
        await generator.numberSymbol(/cal\(h\)/g, '𝒽', startWordLimit),
        await generator.numberSymbol(/cal\(i\)/g, '𝒾', startWordLimit),
        await generator.numberSymbol(/cal\(j\)/g, '𝒿', startWordLimit),
        await generator.numberSymbol(/cal\(k\)/g, '𝓀', startWordLimit),
        await generator.numberSymbol(/cal\(l\)/g, '𝓁', startWordLimit),
        await generator.numberSymbol(/cal\(m\)/g, '𝓂', startWordLimit),
        await generator.numberSymbol(/cal\(n\)/g, '𝓃', startWordLimit),
        await generator.numberSymbol(/cal\(o\)/g, 'ℴ', startWordLimit),
        await generator.numberSymbol(/cal\(p\)/g, '𝓅', startWordLimit),
        await generator.numberSymbol(/cal\(q\)/g, '𝓆', startWordLimit),
        await generator.numberSymbol(/cal\(r\)/g, '𝓇', startWordLimit),
        await generator.numberSymbol(/cal\(s\)/g, '𝓈', startWordLimit),
        await generator.numberSymbol(/cal\(t\)/g, '𝓉', startWordLimit),
        await generator.numberSymbol(/cal\(u\)/g, '𝓊', startWordLimit),
        await generator.numberSymbol(/cal\(v\)/g, '𝓋', startWordLimit),
        await generator.numberSymbol(/cal\(w\)/g, '𝓌', startWordLimit),
        await generator.numberSymbol(/cal\(x\)/g, '𝓍', startWordLimit),
        await generator.numberSymbol(/cal\(y\)/g, '𝓎', startWordLimit),
        await generator.numberSymbol(/cal\(z\)/g, '𝓏', startWordLimit),
        // Fraktur letters
        await generator.numberSymbol(/frak\(A\)/g, '𝔄', startWordLimit),
        await generator.numberSymbol(/frak\(B\)/g, '𝔅', startWordLimit),
        await generator.numberSymbol(/frak\(C\)/g, 'ℭ', startWordLimit),
        await generator.numberSymbol(/frak\(D\)/g, '𝔇', startWordLimit),
        await generator.numberSymbol(/frak\(E\)/g, '𝔈', startWordLimit),
        await generator.numberSymbol(/frak\(F\)/g, '𝔉', startWordLimit),
        await generator.numberSymbol(/frak\(G\)/g, '𝔊', startWordLimit),
        await generator.numberSymbol(/frak\(H\)/g, 'ℌ', startWordLimit),
        await generator.numberSymbol(/frak\(I\)/g, 'ℑ', startWordLimit),
        await generator.numberSymbol(/frak\(J\)/g, '𝔍', startWordLimit),
        await generator.numberSymbol(/frak\(K\)/g, '𝔎', startWordLimit),
        await generator.numberSymbol(/frak\(L\)/g, '𝔏', startWordLimit),
        await generator.numberSymbol(/frak\(M\)/g, '𝔐', startWordLimit),
        await generator.numberSymbol(/frak\(N\)/g, '𝔑', startWordLimit),
        await generator.numberSymbol(/frak\(O\)/g, '𝔒', startWordLimit),
        await generator.numberSymbol(/frak\(P\)/g, '𝔓', startWordLimit),
        await generator.numberSymbol(/frak\(Q\)/g, '𝔔', startWordLimit),
        await generator.numberSymbol(/frak\(R\)/g, 'ℜ', startWordLimit),
        await generator.numberSymbol(/frak\(S\)/g, '𝔖', startWordLimit),
        await generator.numberSymbol(/frak\(T\)/g, '𝔗', startWordLimit),
        await generator.numberSymbol(/frak\(U\)/g, '𝔘', startWordLimit),
        await generator.numberSymbol(/frak\(V\)/g, '𝔙', startWordLimit),
        await generator.numberSymbol(/frak\(W\)/g, '𝔚', startWordLimit),
        await generator.numberSymbol(/frak\(X\)/g, '𝔛', startWordLimit),
        await generator.numberSymbol(/frak\(Y\)/g, '𝔜', startWordLimit),
        await generator.numberSymbol(/frak\(Z\)/g, 'ℨ', startWordLimit),
        await generator.numberSymbol(/frak\(a\)/g, '𝔞', startWordLimit),
        await generator.numberSymbol(/frak\(b\)/g, '𝔟', startWordLimit),
        await generator.numberSymbol(/frak\(c\)/g, '𝔠', startWordLimit),
        await generator.numberSymbol(/frak\(d\)/g, '𝔡', startWordLimit),
        await generator.numberSymbol(/frak\(e\)/g, '𝔢', startWordLimit),
        await generator.numberSymbol(/frak\(f\)/g, '𝔣', startWordLimit),
        await generator.numberSymbol(/frak\(g\)/g, '𝔤', startWordLimit),
        await generator.numberSymbol(/frak\(h\)/g, '𝔥', startWordLimit),
        await generator.numberSymbol(/frak\(i\)/g, '𝔦', startWordLimit),
        await generator.numberSymbol(/frak\(j\)/g, '𝔧', startWordLimit),
        await generator.numberSymbol(/frak\(k\)/g, '𝔨', startWordLimit),
        await generator.numberSymbol(/frak\(l\)/g, '𝔩', startWordLimit),
        await generator.numberSymbol(/frak\(m\)/g, '𝔪', startWordLimit),
        await generator.numberSymbol(/frak\(n\)/g, '𝔫', startWordLimit),
        await generator.numberSymbol(/frak\(o\)/g, '𝔬', startWordLimit),
        await generator.numberSymbol(/frak\(p\)/g, '𝔭', startWordLimit),
        await generator.numberSymbol(/frak\(q\)/g, '𝔮', startWordLimit),
        await generator.numberSymbol(/frak\(r\)/g, '𝔯', startWordLimit),
        await generator.numberSymbol(/frak\(s\)/g, '𝔰', startWordLimit),
        await generator.numberSymbol(/frak\(t\)/g, '𝔱', startWordLimit),
        await generator.numberSymbol(/frak\(u\)/g, '𝔲', startWordLimit),
        await generator.numberSymbol(/frak\(v\)/g, '𝔳', startWordLimit),
        await generator.numberSymbol(/frak\(w\)/g, '𝔴', startWordLimit),
        await generator.numberSymbol(/frak\(x\)/g, '𝔵', startWordLimit),
        await generator.numberSymbol(/frak\(y\)/g, '𝔶', startWordLimit),
        await generator.numberSymbol(/frak\(z\)/g, '𝔷', startWordLimit),
        // blackboard bold letters
        await generator.numberSymbol(/bb\(A\)/g, '𝔸', startWordLimit),
        await generator.numberSymbol(/bb\(B\)/g, '𝔹', startWordLimit),
        await generator.numberSymbol(/bb\(C\)/g, 'ℂ', startWordLimit),
        await generator.numberSymbol(/bb\(D\)/g, '𝔻', startWordLimit),
        await generator.numberSymbol(/bb\(E\)/g, '𝔼', startWordLimit),
        await generator.numberSymbol(/bb\(F\)/g, '𝔽', startWordLimit),
        await generator.numberSymbol(/bb\(G\)/g, '𝔾', startWordLimit),
        await generator.numberSymbol(/bb\(H\)/g, 'ℍ', startWordLimit),
        await generator.numberSymbol(/bb\(I\)/g, '𝕀', startWordLimit),
        await generator.numberSymbol(/bb\(J\)/g, '𝕁', startWordLimit),
        await generator.numberSymbol(/bb\(K\)/g, '𝕂', startWordLimit),
        await generator.numberSymbol(/bb\(L\)/g, '𝕃', startWordLimit),
        await generator.numberSymbol(/bb\(M\)/g, '𝕄', startWordLimit),
        await generator.numberSymbol(/bb\(N\)/g, 'ℕ', startWordLimit),
        await generator.numberSymbol(/bb\(O\)/g, '𝕆', startWordLimit),
        await generator.numberSymbol(/bb\(P\)/g, 'ℙ', startWordLimit),
        await generator.numberSymbol(/bb\(Q\)/g, 'ℚ', startWordLimit),
        await generator.numberSymbol(/bb\(R\)/g, 'ℝ', startWordLimit),
        await generator.numberSymbol(/bb\(S\)/g, '𝕊', startWordLimit),
        await generator.numberSymbol(/bb\(T\)/g, '𝕋', startWordLimit),
        await generator.numberSymbol(/bb\(U\)/g, '𝕌', startWordLimit),
        await generator.numberSymbol(/bb\(V\)/g, '𝕍', startWordLimit),
        await generator.numberSymbol(/bb\(W\)/g, '𝕎', startWordLimit),
        await generator.numberSymbol(/bb\(X\)/g, '𝕏', startWordLimit),
        await generator.numberSymbol(/bb\(Y\)/g, '𝕐', startWordLimit),
        await generator.numberSymbol(/bb\(Z\)/g, 'ℤ', startWordLimit),
        await generator.numberSymbol(/bb\(a\)/g, '𝕒', startWordLimit),
        await generator.numberSymbol(/bb\(b\)/g, '𝕓', startWordLimit),
        await generator.numberSymbol(/bb\(c\)/g, '𝕔', startWordLimit),
        await generator.numberSymbol(/bb\(d\)/g, '𝕕', startWordLimit),
        await generator.numberSymbol(/bb\(e\)/g, '𝕖', startWordLimit),
        await generator.numberSymbol(/bb\(f\)/g, '𝕗', startWordLimit),
        await generator.numberSymbol(/bb\(g\)/g, '𝕘', startWordLimit),
        await generator.numberSymbol(/bb\(h\)/g, '𝕙', startWordLimit),
        await generator.numberSymbol(/bb\(i\)/g, '𝕚', startWordLimit),
        await generator.numberSymbol(/bb\(j\)/g, '𝕛', startWordLimit),
        await generator.numberSymbol(/bb\(k\)/g, '𝕜', startWordLimit),
        await generator.numberSymbol(/bb\(l\)/g, '𝕝', startWordLimit),
        await generator.numberSymbol(/bb\(m\)/g, '𝕞', startWordLimit),
        await generator.numberSymbol(/bb\(n\)/g, '𝕟', startWordLimit),
        await generator.numberSymbol(/bb\(o\)/g, '𝕠', startWordLimit),
        await generator.numberSymbol(/bb\(p\)/g, '𝕡', startWordLimit),
        await generator.numberSymbol(/bb\(q\)/g, '𝕢', startWordLimit),
        await generator.numberSymbol(/bb\(r\)/g, '𝕣', startWordLimit),
        await generator.numberSymbol(/bb\(s\)/g, '𝕤', startWordLimit),
        await generator.numberSymbol(/bb\(t\)/g, '𝕥', startWordLimit),
        await generator.numberSymbol(/bb\(u\)/g, '𝕦', startWordLimit),
        await generator.numberSymbol(/bb\(v\)/g, '𝕧', startWordLimit),
        await generator.numberSymbol(/bb\(w\)/g, '𝕨', startWordLimit),
        await generator.numberSymbol(/bb\(x\)/g, '𝕩', startWordLimit),
        await generator.numberSymbol(/bb\(y\)/g, '𝕪', startWordLimit),
        await generator.numberSymbol(/bb\(z\)/g, '𝕫', startWordLimit),
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


        ...await generator.numberSymbolOnlyVariantsJulia(/A/g, 'A'),
        ...await generator.numberSymbolOnlyVariantsJulia(/B/g, 'B'),
        ...await generator.numberSymbolOnlyVariantsJulia(/C/g, 'C'),
        ...await generator.numberSymbolOnlyVariantsJulia(/D/g, 'D'),
        ...await generator.numberSymbolOnlyVariantsJulia(/E/g, 'E'),
        ...await generator.numberSymbolOnlyVariantsJulia(/F/g, 'F'),
        ...await generator.numberSymbolOnlyVariantsJulia(/G/g, 'G'),
        ...await generator.numberSymbolOnlyVariantsJulia(/H/g, 'H'),
        ...await generator.numberSymbolOnlyVariantsJulia(/I/g, 'I'),
        ...await generator.numberSymbolOnlyVariantsJulia(/J/g, 'J'),
        ...await generator.numberSymbolOnlyVariantsJulia(/K/g, 'K'),
        ...await generator.numberSymbolOnlyVariantsJulia(/L/g, 'L'),
        ...await generator.numberSymbolOnlyVariantsJulia(/M/g, 'M'),
        ...await generator.numberSymbolOnlyVariantsJulia(/N/g, 'N'),
        ...await generator.numberSymbolOnlyVariantsJulia(/O/g, 'O'),
        ...await generator.numberSymbolOnlyVariantsJulia(/P/g, 'P'),
        ...await generator.numberSymbolOnlyVariantsJulia(/Q/g, 'Q'),
        ...await generator.numberSymbolOnlyVariantsJulia(/R/g, 'R'),
        ...await generator.numberSymbolOnlyVariantsJulia(/S/g, 'S'),
        ...await generator.numberSymbolOnlyVariantsJulia(/T/g, 'T'),
        ...await generator.numberSymbolOnlyVariantsJulia(/U/g, 'U'),
        ...await generator.numberSymbolOnlyVariantsJulia(/V/g, 'V'),
        ...await generator.numberSymbolOnlyVariantsJulia(/W/g, 'W'),
        ...await generator.numberSymbolOnlyVariantsJulia(/X/g, 'X'),
        ...await generator.numberSymbolOnlyVariantsJulia(/Y/g, 'Y'),
        ...await generator.numberSymbolOnlyVariantsJulia(/Z/g, 'Z'),
        ...await generator.numberSymbolOnlyVariantsJulia(/a/g, 'a'),
        ...await generator.numberSymbolOnlyVariantsJulia(/b/g, 'b'),
        ...await generator.numberSymbolOnlyVariantsJulia(/c/g, 'c'),
        ...await generator.numberSymbolOnlyVariantsJulia(/d/g, 'd'),
        ...await generator.numberSymbolOnlyVariantsJulia(/e/g, 'e'),
        ...await generator.numberSymbolOnlyVariantsJulia(/f/g, 'f'),
        ...await generator.numberSymbolOnlyVariantsJulia(/g/g, 'g'),
        ...await generator.numberSymbolOnlyVariantsJulia(/h/g, 'h'),
        ...await generator.numberSymbolOnlyVariantsJulia(/i/g, 'i'),
        ...await generator.numberSymbolOnlyVariantsJulia(/j/g, 'j'),
        ...await generator.numberSymbolOnlyVariantsJulia(/k/g, 'k'),
        ...await generator.numberSymbolOnlyVariantsJulia(/l/g, 'l'),
        ...await generator.numberSymbolOnlyVariantsJulia(/m/g, 'm'),
        ...await generator.numberSymbolOnlyVariantsJulia(/n/g, 'n'),
        ...await generator.numberSymbolOnlyVariantsJulia(/o/g, 'o'),
        ...await generator.numberSymbolOnlyVariantsJulia(/p/g, 'p'),
        ...await generator.numberSymbolOnlyVariantsJulia(/q/g, 'q'),
        ...await generator.numberSymbolOnlyVariantsJulia(/r/g, 'r'),
        ...await generator.numberSymbolOnlyVariantsJulia(/s/g, 's'),
        ...await generator.numberSymbolOnlyVariantsJulia(/t/g, 't'),
        ...await generator.numberSymbolOnlyVariantsJulia(/u/g, 'u'),
        ...await generator.numberSymbolOnlyVariantsJulia(/v/g, 'v'),
        ...await generator.numberSymbolOnlyVariantsJulia(/w/g, 'w'),
        ...await generator.numberSymbolOnlyVariantsJulia(/x/g, 'x'),
        ...await generator.numberSymbolOnlyVariantsJulia(/y/g, 'y'),
        ...await generator.numberSymbolOnlyVariantsJulia(/z/g, 'z'),
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
    await generator.simpleRegex(
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
    await generator.simpleRegex(
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
    await generator.simpleRegex(
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
    await generator.simpleRegex(
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
    await generator.simpleRegex(
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
    await generator.simpleRegex(
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