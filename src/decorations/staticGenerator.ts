import * as vscode from 'vscode';
import { createDecorationType, helperSimpleRegex, resetAllDecorations } from './helpers';
import { getColors, renderingMode } from './utils';
import { STYLES } from './styles';

// Usefull regex
export const wordLimit = /(?!\.)(\b|_|\n|\r)/g;
export const startWordLimit = /(?!\w|\d|\.)(\n|..|.)/g;
export const arrowLimitLow = /[^=\-<>]/g;

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

    constructor(activeEditor: vscode.TextEditor) {
        this.text = activeEditor.document.getText();
    }

    staticSimpleRegex(activeEditor: vscode.TextEditor, reg: RegExp, pre?: RegExp, post?: RegExp, rangeStartOffset = 0, rangeEndOffset = 0) {
        const result: vscode.DecorationOptions[] = [];
        helperSimpleRegex(activeEditor.document.getText(), activeEditor, reg, (match, range) => {
            result.push({ range: range, hoverMessage: match[0] });
        }, pre, post, rangeStartOffset, rangeEndOffset);
        return result;
    }
    async helperSymbol(reg: RegExp, symbol: string, options: {
        color: string,
        textDecoration: string,
    }, pre?: RegExp, post?: RegExp, force_creation: boolean = false) {
        let uuid = symbol + (post ? post.source : "") + "-qz2qf-" + reg.source + (pre ? pre.source : "");
        if (already_existing_map.has(uuid) && !force_creation) {
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
    public async helperWithVariants(reg: RegExp, symbol: string, options: {
        color: string,
        textDecoration: string,
    }, pre?: RegExp, post?: RegExp) {
        return [
            // Basic version
            await this.helperSymbol(reg, symbol, options, pre, post),
            // Variants
            ...(await this.helperOnlyVariants(reg, symbol, options, pre, post))
        ];
    }
    public async helperOnlyVariants(reg: RegExp, symbol: string, options: {
        color: string,
        textDecoration: string,
    }, pre?: RegExp, post?: RegExp, minimal: boolean = false) {
        minimal_variant_list.push(reg.source);
        if (!minimal) {
            variant_list.push(reg.source);
            return [

                // Superscript version
                await this.helperSymbol(RegExp(`\\^(${reg.source}\\b|\\(${reg.source}\\))`), symbol,
                    {
                        ...options,
                        textDecoration: options.textDecoration + `font-size: 0.8em; transform: translateY(-30%); display: inline-block; `
                    }),
                // Subscript version
                await this.helperSymbol(RegExp(`_(${reg.source}\\b|\\(${reg.source}\\))`), symbol, {
                    ...options,
                    textDecoration: options.textDecoration + `font-size: 0.8em; transform: translateY(20%); display: inline-block; `
                }),
                // Superscrit with minus
                await this.helperSymbol(RegExp(`\\^\\(-${reg.source}\\)`), "-" + symbol, {
                    ...options,
                    textDecoration: options.textDecoration + `font-size: 0.8em; transform: translateY(-30%); display: inline-block; `
                }),
                // Subscript with minus
                await this.helperSymbol(RegExp(`_\\(-${reg.source}\\)`), "-" + symbol, {
                    ...options,
                    textDecoration: options.textDecoration + `font-size: 0.8em; transform: translateY(20%); display: inline-block; `
                }),
            ];
        }
        return [];
    }
    public async comparisonSymbol(reg: RegExp, symbol: string, pre?: RegExp, post?: RegExp) {
        return await this.helperSymbol(reg, symbol, {
            color: getColors("comparison"),
            textDecoration: 'none; font-family: "NewComputerModernMath"; font-weight: bold;',
        }, pre, post);
    }
    public async keywordSymbol(reg: RegExp, symbol: string, pre?: RegExp, post?: RegExp) {
        if (renderingMode() < 2) { return null; }
        return await this.helperSymbol(reg, symbol, {
            color: getColors("keyword"),
            textDecoration: 'none; font-family: "NewComputerModernMath"; font-weight: bold;'
        }, pre, post);
    }
    public async letterSymbol(reg: RegExp, symbol: string) {
        return await this.helperSymbol(reg, symbol, {
            color: getColors("letter"),
            textDecoration: 'none; font-family: "JuliaMono";',
        }, wordLimit, wordLimit);
    }
    public async letterSymbolWithVariants(reg: RegExp, symbol: string) {
        return this.helperWithVariants(reg, symbol,
            {
                color: getColors("letter"),
                textDecoration: 'none; font-family: "JuliaMono";',
            }, /(?!\.|\^|[A-z])./g, wordLimit);
    }

    public async bigLetterSymbol(reg: RegExp, symbol: string) {
        if (renderingMode() < 2) { return null; }
        return await this.helperSymbol(reg, symbol, {
            color: getColors("letter"),
            textDecoration: 'none; font-family: "NewComputerModernMath";',
        }, startWordLimit, wordLimit);
    }

    public async mathSetSymbol(reg: RegExp, symbol: string) {
        if (renderingMode() < 2) { return null; }
        return await this.helperSymbol(reg, symbol, {
            color: getColors("group"),
            textDecoration: `none;
        font-family: "Fira Math"; `,
        });
    }
    public async mathSetSymbolWithVariants(reg: RegExp, symbol: string) {
        if (renderingMode() < 2) { return []; }
        return await this.helperWithVariants(reg, symbol, {
            color: getColors("group"),
            textDecoration: `none;
        font-family: "Fira Math"; `,
        }, /(?!\.|\^|[A-z])./g);
    }

    public async mathSetVariantsSymbol(reg: RegExp, symbol: string, style: string, pre?: RegExp, post?: RegExp) {
        if (renderingMode() < 2) { return null; }
        return await this.helperSymbol(reg, symbol, {
            color: getColors("group"),
            textDecoration: `none;
        font-family: "JuliaMono";
            ${style} `,
        }, pre, post);
    }

    public async mathExtendSetSymbol(reg: RegExp, symbol: string, pre?: RegExp, post?: RegExp) {
        return await this.helperSymbol(reg, symbol, {
            color: getColors("group"),
            textDecoration: `none;
        font-family: "Fira Math"; `,
        }, pre, post);
    }

    public async operatorSymbol(reg: RegExp, symbol: string, pre?: RegExp, post?: RegExp) {
        return await this.helperSymbol(reg, symbol, {
            color: getColors("operator"),
            textDecoration: `none;
        font-family: "Fira Math"; `,
        }, pre, post);
    }

    public async numberSymbol(reg: RegExp, symbol: string, pre?: RegExp, post?: RegExp) {
        if (renderingMode() < 2) { return null; }
        return await this.helperSymbol(reg, symbol, {
            color: getColors("number"),
            textDecoration: `none;
        font-family: "NewComputerModernMath"; `,
        }, pre, post);
    }
    public async numberSymbolOnlyVariantsJulia(reg: RegExp, symbol: string, pre?: RegExp, post?: RegExp, minimal: boolean = false) {
        return await this.helperOnlyVariants(reg, symbol, {
            color: getColors("number"),
            textDecoration: `none;
        letter-spacing: -0.1em;
        font-family: JuliaMono; `,
        }, pre, post, minimal);
    }
    public async generateFunctionVariants() {
        // create a massive or to match all variants in the minimal_variant_list variable
        // Generate complex variants
        let subsuper = "(" + minimal_variant_list.join("|") + ")_?\\^?(" + minimal_variant_list.join("|") + ")";
        minimal_variant_list.push(subsuper);
        variant_list.push(subsuper);
        let big_reg = new RegExp(`(${minimal_variant_list.join("|")})`);
        let big_reg_notminimal = new RegExp(`(${variant_list.join("|")})`);
        let result = [
            // Arrow function
            await this.helperSymbol(/arrow\(/g, STYLES.arrow_func_start.symbol,
                STYLES.arrow_func_start.options, startWordLimit, RegExp(`${big_reg.source}\\)`), true),
            await this.helperSymbol(/\)/g, STYLES.arrow_func_end.symbol,
                STYLES.arrow_func_end.options, RegExp(`arrow\\(${big_reg.source}`), undefined, true),

            // Overline function
            await this.helperSymbol(/overline\(/g, STYLES.overline_func_start.symbol,
                STYLES.overline_func_start.options, startWordLimit, RegExp(`${big_reg.source}\\)`), true),
            await this.helperSymbol(/\)/g, STYLES.overline_func_end.symbol,
                STYLES.overline_func_end.options, RegExp(`overline\\(${big_reg.source}`), undefined, true),

            // Dot function
            await this.helperSymbol(/dot\(/g, STYLES.dot_func_start.symbol,
                STYLES.dot_func_start.options, startWordLimit, RegExp(`${big_reg.source}\\)`), true),
            await this.helperSymbol(/\)/g, STYLES.dot_func_end.symbol,
                STYLES.dot_func_end.options, RegExp(`dot\\(${big_reg.source}`), undefined, true),
            // Double dot function
            await this.helperSymbol(/dot\.double\(/g, STYLES.double_dot_func_start.symbol,
                STYLES.double_dot_func_start.options, startWordLimit, RegExp(`${big_reg.source}\\)`), true),
            await this.helperSymbol(/\)/g, STYLES.double_dot_func_end.symbol,
                STYLES.double_dot_func_end.options, RegExp(`dot\\.double\\(${big_reg.source}`), undefined, true),
            // Triple dot function
            await this.helperSymbol(/dot\.triple\(/g, STYLES.triple_dot_func_start.symbol,
                STYLES.triple_dot_func_start.options, startWordLimit, RegExp(`${big_reg.source}\\)`), true),
            await this.helperSymbol(/\)/g, STYLES.triple_dot_func_end.symbol,
                STYLES.triple_dot_func_end.options, RegExp(`dot\\.triple\\(${big_reg.source}`), undefined, true),
            // Quad dot function
            await this.helperSymbol(/dot\.quad\(/g, STYLES.quad_dot_func_start.symbol,
                STYLES.quad_dot_func_start.options, startWordLimit, RegExp(`${big_reg.source}\\)`), true),
            await this.helperSymbol(/\)/g, STYLES.quad_dot_func_end.symbol,
                STYLES.quad_dot_func_end.options, RegExp(`dot\\.quad\\(${big_reg.source}`), undefined, true),

            // Hat function
            await this.helperSymbol(/hat\(/g, STYLES.hat_func_start.symbol,
                STYLES.hat_func_start.options, startWordLimit, RegExp(`${big_reg.source}\\)`), true),
            await this.helperSymbol(/\)/g, STYLES.hat_func_end.symbol,
                STYLES.hat_func_end.options, RegExp(`hat\\(${big_reg.source}`), undefined, true),

            // Tilde function
            await this.helperSymbol(/tilde\(/g, STYLES.tilde_func_start.symbol,
                STYLES.tilde_func_start.options, startWordLimit, RegExp(`${big_reg.source}\\)`), true),
            await this.helperSymbol(/\)/g, STYLES.tilde_func_end.symbol,
                STYLES.tilde_func_end.options, RegExp(`tilde\\(${big_reg.source}`), undefined, true),
        ];

        result = result.concat([
            // Abs function
            await this.helperSymbol(/abs\(/g, STYLES.abs_func.symbol,
                STYLES.abs_func.options, startWordLimit, RegExp(`${big_reg_notminimal.source}\\)`), true),
            await this.helperSymbol(/\)/g, STYLES.abs_func.symbol,
                STYLES.abs_func.options, RegExp(`abs\\(${big_reg_notminimal.source}`), undefined, true),

            // Norm function
            await this.helperSymbol(/norm\(/g, STYLES.norm_func.symbol,
                STYLES.norm_func.options, startWordLimit, RegExp(`${big_reg_notminimal.source}\\)`), true),
            await this.helperSymbol(/\)/g, STYLES.norm_func.symbol,
                STYLES.norm_func.options, RegExp(`norm\\(${big_reg_notminimal.source}`), undefined, true),
        ]);

        return result;
    }
}