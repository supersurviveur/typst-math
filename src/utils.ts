import * as vscode from 'vscode';
import { Color } from "typst-math-rust";
import getWASM from './wasmHelper';

let config = vscode.workspace.getConfiguration('typst-math');

export function reloadConfiguration() {
    config = vscode.workspace.getConfiguration('typst-math');
}

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
    return config.get<boolean>('renderSymbolsOutsideMath') || false;
}
// Retreive the settings for space rendering
export function renderSpaces() {
    return config.get<boolean>('renderSpaces') || false;
}
// Retreive the settings for delimiter rendering
export function hideUnnecessaryDelimiters() {
    return config.get<boolean>('hideUnnecessaryDelimiters') || false;
}
// Retreive the settings for space rendering
export function revealOffset() {
    return config.get<number>('revealOffset') || 0;
}
// Retreive the settings for custom symbols
export function customSymbols(): {
    name: string,
    symbol: string
    category: string
}[] {
    let user = config.get<{
        name: string,
        symbol: string
        category: string
    }[]>('customSymbols') || [];

    // Check if the custom symbols are valid
    if (typeof user !== "object") {
        throw new Error("Invalid custom symbols");
    }
    for (let i = 0; i < user.length; i++) {
        if (typeof user[i] !== "object" || typeof user[i].name !== "string" || typeof user[i].symbol !== "string" || typeof user[i].category !== "string") {
            throw new Error(`Invalid custom symbols "${user[i]}" at index ${i}`);
        }
    }
    return user;
}
// Retreive blacklisted symbols
export function blacklistedSymbols() {
    return config.get<string[]>('blacklist') || [];
}

// Get the rendering mode
export function getRenderingMode() {
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