import * as vscode from 'vscode';
import { Color } from "typst-math-rust";
import getWASM from './wasmHelper';

interface Colors {
    comparison: string,
    keyword: string,
    letter: string,
    group: string,
    operator: string,
    number: string,
}

// Default color themes
// Dark theme based on the Monokai theme
const darkTheme = {
    keyword: "#F92672",
    number: "#f8f8f2",
    comparison: "#AE81FF",
    letter: "#A6E22E",
    group: "#66D9EF",
    operator: "#FD971F"
};
// Light theme based on the Monokai theme
const lightTheme = {
    keyword: "#0000FF",
    number: "#000000",
    comparison: "#EE0000",
    letter: "#795E26",
    group: "#008000",
    operator: "#0070C1"
};


/**
 * Convert a WASM color to the name of the corresponding color in the settings
 * @param colorEnum 
 * @returns 
 */
function enumToColorName(colorEnum: Color): keyof Colors {
    switch (colorEnum) {
        case getWASM().Color.Keyword: return "keyword";
        case getWASM().Color.Comparison: return "comparison";
        case getWASM().Color.Operator: return "operator";
        case getWASM().Color.Letter: return "letter";
        case getWASM().Color.Set: return "group";
        case getWASM().Color.Number: return "number";
    }
}
// Get colors from settings
export function getColors(colorType: Color) {
    const config = vscode.workspace.getConfiguration('typst-math');
    const colors = config.get<Colors>('colors');
    if (!colors) {
        throw new Error("Invalid colors");
    }
    const color = enumToColorName(colorType);
    if (colors[color] === "") {
        // Get the theme kind (light or dark)
        const themeKind = vscode.window.activeColorTheme.kind;
        if (themeKind === vscode.ColorThemeKind.Dark) {
            return darkTheme[color];
        } else {
            return lightTheme[color];
        }
    } else {
        return colors[color];
    }
}

// Retreive the settings for decorations outside math mode
export function renderSymbolsOutsideMath() {
    const config = vscode.workspace.getConfiguration('typst-math');
    return config.get<boolean>('renderSymbolsOutsideMath') || false;
}
// Retreive the settings for space rendering
export function renderSpaces() {
    const config = vscode.workspace.getConfiguration('typst-math');
    return config.get<boolean>('renderSpaces') || false;
}
// Retreive blacklisted symbols
export function blacklistedSymbols() {
    const config = vscode.workspace.getConfiguration('typst-math');
    return config.get<string[]>('blacklist') || [];
}

// Get the rendering mode
export function getRenderingMode() {
    const config = vscode.workspace.getConfiguration('typst-math');
    let mode = config.get<string>('renderingMode');
    if (mode === "nothing") {
        return 0;
    } else if (mode === "basic") {
        return 1;
    } else if (mode === "medium") {
        return 2;
    } else if (mode === "complex") {
        return 3;
    } else {
        return 1;
    }
}