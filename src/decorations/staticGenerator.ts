import * as vscode from 'vscode';
import { createDecorationType, helperSimpleRegex } from './helpers';
import { getColors } from './utils';

// Usefull regex
export const wordLimit = /(?!\.)(\b|_|\n|\r)/g;
export const startWordLimit = /[^\w\d\.]/g;
export const arrowLimitLow = /[^=\-<>]/g;

export class StaticGenerator {
    constructor() { }

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
    }, pre?: RegExp, post?: RegExp) {
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
    public comparisonSymbol(reg: RegExp, symbol: string, pre?: RegExp, post?: RegExp) {
        return this.helperSymbol(reg, symbol, {
            color: getColors("comparison"),
            textDecoration: 'none; font-family: "NewComputerModernMath"; font-weight: bold;',
        }, pre, post);
    }
    public keywordSymbol(reg: RegExp, symbol: string, pre?: RegExp, post?: RegExp) {
        return this.helperSymbol(reg, symbol, {
            color: getColors("keyword"),
            textDecoration: 'none; font-family: "NewComputerModernMath"; font-weight: bold;'
        }, pre, post);
    }
    public letterSymbol(reg: RegExp, symbol: string) {
        return this.helperSymbol(reg, symbol, {
            color: getColors("letter"),
            textDecoration: 'none; font-family: "JuliaMono";',
        }, wordLimit, wordLimit);
    }
    public letterSymbolWithVariants(reg: RegExp, symbol: string) {
        return [
            // Basic version
            this.helperSymbol(reg, symbol, {
                color: getColors("letter"),
                textDecoration: 'none; font-family: "JuliaMono";',
            }, /(?!\.|\^|[A-z])./g, wordLimit),
            // Superscript version
            this.helperSymbol(RegExp(`\\^(${reg.source}\\b|\\(${reg.source}\\))`), symbol, {
                color: getColors("letter"),
                textDecoration: `none; font-family: "JuliaMono";
                font-size: 0.8em;
                transform: translateY(-30%);
                display: inline-block;`,
            }),
            // Subscript version
            this.helperSymbol(RegExp(`_(${reg.source}\\b|\\(${reg.source}\\))`), symbol, {
                color: getColors("letter"),
                textDecoration: `none; font-family: "JuliaMono";
                font-size: 0.8em;
                transform: translateY(20%);
                display: inline-block;`,
            })
        ];
    }
    public bigLetterSymbol(reg: RegExp, symbol: string) {
        return this.helperSymbol(reg, symbol, {
            color: getColors("letter"),
            textDecoration: 'none; font-family: "NewComputerModernMath";',
        }, startWordLimit, wordLimit);
    }

    public mathSetSymbol(reg: RegExp, symbol: string) {
        return this.helperSymbol(reg, symbol, {
            color: getColors("group"),
            textDecoration: `none;
            font-family: "Fira Math";`,
        });
    }

    public mathSetVariantsSymbol(reg: RegExp, symbol: string, style: string, pre?: RegExp, post?: RegExp) {
        return this.helperSymbol(reg, symbol, {
            color: getColors("group"),
            textDecoration: `none;
            font-family: "JuliaMono";
            ${style}`,
        }, pre, post);
    }

    public mathExtendSetSymbol(reg: RegExp, symbol: string, pre?: RegExp, post?: RegExp) {
        return this.helperSymbol(reg, symbol, {
            color: getColors("group"),
            textDecoration: `none;
            font-family: "Fira Math";`,
        }, pre, post);
    }

    public operatorSymbol(reg: RegExp, symbol: string, pre?: RegExp, post?: RegExp) {
        return this.helperSymbol(reg, symbol, {
            color: getColors("operator"),
            textDecoration: `none;
            font-family: "Fira Math";`,
        }, pre, post);
    }

    public numberSymbol(reg: RegExp, symbol: string, pre?: RegExp, post?: RegExp) {
        return this.helperSymbol(reg, symbol, {
            color: getColors("number"),
            textDecoration: `none;
            font-family: "NewComputerModernMath";`,
        }, pre, post);
    }
}