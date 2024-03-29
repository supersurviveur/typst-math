import * as vscode from 'vscode';
import { createDecorationType, helperSimpleRegex, resetAllDecorations } from './helpers';
import { blacklistedSymbols, getColors, renderingMode } from './utils';
import { STYLES } from './styles';

// Usefull regex
export const wordLimit = /(?!\.)(\b|_|\n|\r)/g; // Can probably be improved
export const startWordLimit = /(\s|(?!.[A-Za-z]|[A-Za-z]\.)(\n|..|\n.))/g; // Seems to work
export const startWordLimitNoVariants = /(\s|(?!.[A-Za-z]|[A-Za-z]\.|._|.\^)(\n|..|\n.))/g;
export const arrowLimitLow = /[^=\-<>~]/g;

// Map to store the already existing decorations, to avoid duplicates
let already_existing_map: Map<string, boolean> = new Map();
// This variable is used to keep all regex who support functions variants
let minimal_variant_list: string[] = [
    "[A-z0-9]_[A-z0-9]",
];
let variant_list: string[] = [
];

export function resetDecorationMap() {
    already_existing_map = new Map();
    minimal_variant_list = [
        "[A-z0-9]_[A-z0-9]",
    ];
    variant_list = [];
}

export class StaticGenerator {
    text: string;
    blacklist: string[];

    constructor(activeEditor: vscode.TextEditor) {
        this.text = activeEditor.document.getText();
        this.blacklist = blacklistedSymbols();
    }

    staticSimpleRegex(activeEditor: vscode.TextEditor, reg: RegExp, pre?: RegExp, post?: RegExp, rangeStartOffset = 0, rangeEndOffset = 0) {
        const result: vscode.DecorationOptions[] = [];
        helperSimpleRegex(activeEditor.document.getText(), activeEditor, reg, (match, range) => {
            result.push({ range: range, hoverMessage: match[0] });
        }, pre, post, rangeStartOffset, rangeEndOffset);
        return result;
    }
    helperSymbol(reg: RegExp, symbol: string, options: {
        color: string,
        textDecoration: string,
    }, pre?: RegExp, post?: RegExp, force_creation: boolean = false) {
        let uuid = symbol + (post ? post.source : "") + "-qz2qf-" + reg.source + (pre ? pre.source : "");
        if (already_existing_map.has(uuid) && !force_creation) {
            return null;
        }
        if (this.blacklist.includes(reg.source.replace("\\", ""))) {
            return null;
        }
        // Try the regex on the current text, return null if no match
        if (!reg.test(this.text) && !force_creation) {
            return null;
        }
        already_existing_map.set(uuid, true);
        return {
            getRanges: (activeEditor: vscode.TextEditor) =>
                this.staticSimpleRegex(activeEditor, reg, pre, post),
            decorationType: createDecorationType({
                color: options.color,
                textDecoration: options.textDecoration,
                contentText: symbol
            })
        };
    }
    public helperWithVariants(reg: RegExp, symbol: string, options: {
        color: string,
        textDecoration: string,
    }, pre?: RegExp, post?: RegExp) {
        return [
            // Basic version
            this.helperSymbol(reg, symbol, options, pre, post),
            // Variants
            ...(this.helperOnlyVariants(reg, symbol, options, pre, post))
        ];
    }
    public helperOnlyVariants(reg: RegExp, symbol: string, options: {
        color: string,
        textDecoration: string,
    }, pre?: RegExp, post?: RegExp, minimal: boolean = false) {
        minimal_variant_list.push(reg.source);
        if (!minimal) {
            variant_list.push(reg.source);
            return [

                // Superscript version
                this.helperSymbol(RegExp(`\\^(${reg.source}\\b|\\(${reg.source}\\))`), symbol,
                    {
                        ...options,
                        textDecoration: options.textDecoration + `font-size: 0.8em; transform: translateY(-30%); display: inline-block; `
                    }),
                // Subscript version
                this.helperSymbol(RegExp(`_(${reg.source}\\b|\\(${reg.source}\\))`), symbol, {
                    ...options,
                    textDecoration: options.textDecoration + `font-size: 0.8em; transform: translateY(20%); display: inline-block; `
                }),
                // Superscrit with minus
                this.helperSymbol(RegExp(`\\^\\(-${reg.source}\\)`), "-" + symbol, {
                    ...options,
                    textDecoration: options.textDecoration + `font-size: 0.8em; transform: translateY(-30%); display: inline-block; `
                }),
                // Subscript with minus
                this.helperSymbol(RegExp(`_\\(-${reg.source}\\)`), "-" + symbol, {
                    ...options,
                    textDecoration: options.textDecoration + `font-size: 0.8em; transform: translateY(20%); display: inline-block; `
                }),
            ];
        }
        return [];
    }
    public spaceSymbol(reg: RegExp, symbol: string, pre?: RegExp, post?: RegExp) {
        let color = getColors("keyword");
        return this.helperSymbol(reg, symbol, {
            color: "",
            textDecoration: `none; font-family: "JuliaMono"; background-color: ${color}; opacity: 0.3; box-shadow: 0 0 0 1px ${color};`,
        }, pre, post);
    }
    public comparisonSymbol(reg: RegExp, symbol: string, pre?: RegExp, post?: RegExp) {
        return this.helperSymbol(reg, symbol, {
            color: getColors("comparison"),
            textDecoration: 'none; font-family: "NewComputerModernMath"; font-weight: bold;',
        }, pre, post);
    }
    public keywordSymbol(reg: RegExp, symbol: string, pre?: RegExp, post?: RegExp) {
        if (renderingMode() < 2) { return null; }
        return this.helperSymbol(reg, symbol, {
            color: getColors("keyword"),
            textDecoration: 'none; font-family: "NewComputerModernMath"; font-weight: bold;'
        }, pre, post);
    }
    public letterSymbol(reg: RegExp, symbol: string) {
        return this.helperSymbol(reg, symbol, {
            color: getColors("letter"),
            textDecoration: 'none; font-family: "JuliaMono";',
        }, startWordLimit, wordLimit);
    }
    public letterSymbolWithVariants(reg: RegExp, symbol: string) {
        return this.helperWithVariants(reg, symbol,
            {
                color: getColors("letter"),
                textDecoration: 'none; font-family: "JuliaMono";',
            }, startWordLimitNoVariants, wordLimit);
    }

    public bigLetterSymbol(reg: RegExp, symbol: string, pre?: RegExp, post?: RegExp) {
        if (renderingMode() < 2) { return null; }
        return this.helperSymbol(reg, symbol, {
            color: getColors("letter"),
            textDecoration: 'none; font-family: "NewComputerModernMath";',
        }, pre, post);
    }

    public mathSetSymbol(reg: RegExp, symbol: string, pre?: RegExp, post?: RegExp) {
        if (renderingMode() < 2) { return null; }
        return this.helperSymbol(reg, symbol, {
            color: getColors("group"),
            textDecoration: `none;
        font-family: "Fira Math"; `,
        }, pre, post);
    }
    public mathSetSymbolWithVariants(reg: RegExp, symbol: string) {
        if (renderingMode() < 2) { return []; }
        return this.helperWithVariants(reg, symbol, {
            color: getColors("group"),
            textDecoration: `none;
        font-family: "Fira Math"; `,
        }, /(?!\^|[A-z])./g, wordLimit);
    }

    public mathSetVariantsSymbol(reg: RegExp, symbol: string, style: string, pre?: RegExp, post?: RegExp) {
        if (renderingMode() < 2) { return null; }
        return this.helperSymbol(reg, symbol, {
            color: getColors("group"),
            textDecoration: `none;
        font-family: "JuliaMono";
            ${style} `,
        }, pre, post);
    }

    public mathExtendSetSymbol(reg: RegExp, symbol: string, pre?: RegExp, post?: RegExp) {
        return this.helperSymbol(reg, symbol, {
            color: getColors("group"),
            textDecoration: `none;
        font-family: "Fira Math"; `,
        }, pre, post);
    }

    public operatorSymbol(reg: RegExp, symbol: string, pre?: RegExp, post?: RegExp) {
        return this.helperSymbol(reg, symbol, {
            color: getColors("operator"),
            textDecoration: `none;
        font-family: "Fira Math"; `,
        }, pre, post);
    }

    public numberSymbol(reg: RegExp, symbol: string, pre?: RegExp, post?: RegExp) {
        if (renderingMode() < 2) { return null; }
        return this.helperSymbol(reg, symbol, {
            color: getColors("number"),
            textDecoration: `none;
        font-family: "NewComputerModernMath"; `,
        }, pre, post);
    }
    public numberSymbolOnlyVariantsJulia(reg: RegExp, symbol: string, pre?: RegExp, post?: RegExp, minimal: boolean = false) {
        return this.helperOnlyVariants(reg, symbol, {
            color: getColors("number"),
            textDecoration: `none;
        letter-spacing: -0.1em;
        font-family: JuliaMono; `,
        }, pre, post, minimal);
    }
    public generateFunctionVariants() {
        // create a massive or to match all variants in the minimal_variant_list variable
        // Generate complex variants
        let subsuper = "(" + minimal_variant_list.join("|") + ")_?\\^?(" + minimal_variant_list.join("|") + ")";
        let sub = "(" + minimal_variant_list.join("|") + ")_(" + minimal_variant_list.join("|") + ")";
        let big_reg_sub = new RegExp(`(${[...minimal_variant_list, sub].join("|")})`);
        minimal_variant_list.push(subsuper);
        variant_list.push(subsuper);
        let big_reg = new RegExp(`(${minimal_variant_list.join("|")})`);
        let big_reg_notminimal = new RegExp(`(${variant_list.join("|")})`);
        let result = [
            // Arrow function
            this.helperSymbol(/arrow\(/g, STYLES.arrow_func_start.symbol,
                STYLES.arrow_func_start.options, startWordLimit, RegExp(`${big_reg.source}\\)`), true),
            this.helperSymbol(/\)/g, STYLES.arrow_func_end.symbol,
                STYLES.arrow_func_end.options, RegExp(`arrow\\(${big_reg.source}`), undefined, true),

            // Overline function
            this.helperSymbol(/overline\(/g, STYLES.overline_func_start.symbol,
                STYLES.overline_func_start.options, startWordLimit, RegExp(`${big_reg.source}\\)`), true),
            this.helperSymbol(/\)/g, STYLES.overline_func_end.symbol,
                STYLES.overline_func_end.options, RegExp(`overline\\(${big_reg.source}`), undefined, true),

            // Dot function
            this.helperSymbol(/dot\(/g, STYLES.dot_func_start.symbol,
                STYLES.dot_func_start.options, startWordLimit, RegExp(`${big_reg.source}\\)`), true),
            this.helperSymbol(/\)/g, STYLES.dot_func_end.symbol,
                STYLES.dot_func_end.options, RegExp(`dot\\(${big_reg.source}`), undefined, true),
            // Double dot function
            this.helperSymbol(/dot\.double\(/g, STYLES.double_dot_func_start.symbol,
                STYLES.double_dot_func_start.options, startWordLimit, RegExp(`${big_reg.source}\\)`), true),
            this.helperSymbol(/\)/g, STYLES.double_dot_func_end.symbol,
                STYLES.double_dot_func_end.options, RegExp(`dot\\.double\\(${big_reg.source}`), undefined, true),
            // Triple dot function
            this.helperSymbol(/dot\.triple\(/g, STYLES.triple_dot_func_start.symbol,
                STYLES.triple_dot_func_start.options, startWordLimit, RegExp(`${big_reg.source}\\)`), true),
            this.helperSymbol(/\)/g, STYLES.triple_dot_func_end.symbol,
                STYLES.triple_dot_func_end.options, RegExp(`dot\\.triple\\(${big_reg.source}`), undefined, true),
            // Quad dot function
            this.helperSymbol(/dot\.quad\(/g, STYLES.quad_dot_func_start.symbol,
                STYLES.quad_dot_func_start.options, startWordLimit, RegExp(`${big_reg.source}\\)`), true),
            this.helperSymbol(/\)/g, STYLES.quad_dot_func_end.symbol,
                STYLES.quad_dot_func_end.options, RegExp(`dot\\.quad\\(${big_reg.source}`), undefined, true),

            // Hat function
            this.helperSymbol(/hat\(/g, STYLES.hat_func_start.symbol,
                STYLES.hat_func_start.options, startWordLimit, RegExp(`${big_reg.source}\\)`), true),
            this.helperSymbol(/\)/g, STYLES.hat_func_end.symbol,
                STYLES.hat_func_end.options, RegExp(`hat\\(${big_reg.source}`), undefined, true),

            // Tilde function
            this.helperSymbol(/tilde\(/g, STYLES.tilde_func_start.symbol,
                STYLES.tilde_func_start.options, startWordLimit, RegExp(`${big_reg.source}\\)`), true),
            this.helperSymbol(/\)/g, STYLES.tilde_func_end.symbol,
                STYLES.tilde_func_end.options, RegExp(`tilde\\(${big_reg.source}`), undefined, true),

            // Square root function
            this.helperSymbol(/s/g, STYLES.sqrt_func_start.symbol,
                STYLES.sqrt_func_start.options, startWordLimit, RegExp(`qrt\\(${big_reg_sub.source}\\)`), true),
            this.helperSymbol(/qrt\(/g, STYLES.sqrt_func_second.symbol,
                STYLES.sqrt_func_second.options, RegExp(`s`), RegExp(`${big_reg_sub.source}\\)`), true),
            this.helperSymbol(/\)/g, STYLES.sqrt_func_end.symbol,
                STYLES.sqrt_func_end.options, RegExp(`sqrt\\(${big_reg_sub.source}`), undefined, true),
        ];

        result = result.concat([
            // Abs function
            this.helperSymbol(/abs\(/g, STYLES.abs_func.symbol,
                STYLES.abs_func.options, startWordLimit, RegExp(`${big_reg_notminimal.source}\\)`), true),
            this.helperSymbol(/\)/g, STYLES.abs_func.symbol,
                STYLES.abs_func.options, RegExp(`abs\\(${big_reg_notminimal.source}`), undefined, true),

            // Norm function
            this.helperSymbol(/norm\(/g, STYLES.norm_func.symbol,
                STYLES.norm_func.options, startWordLimit, RegExp(`${big_reg_notminimal.source}\\)`), true),
            this.helperSymbol(/\)/g, STYLES.norm_func.symbol,
                STYLES.norm_func.options, RegExp(`norm\\(${big_reg_notminimal.source}`), undefined, true),
        ]);

        return result;
    }
}