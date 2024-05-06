import * as vscode from 'vscode';
import { Color } from "typst-math-rust";
import getWASM from '../wasmHelper';

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

// Get colors from settings
export function getColors(colorType: keyof Colors) {
    const config = vscode.workspace.getConfiguration('typst-math');
    const colors = config.get<Colors>('colors');
    if (!colors) {
        throw new Error("Invalid colors");
    }
    if (colors[colorType] === "") {
        // Get the theme kind (light or dark)
        const themeKind = vscode.window.activeColorTheme.kind;
        if (themeKind === vscode.ColorThemeKind.Dark) {
            return darkTheme[colorType];
        } else {
            return lightTheme[colorType];
        }
    } else {
        return colors[colorType];
    }
}


function enumToColorName(colorEnum: Color) {
    switch (colorEnum) {
        case getWASM().Color.KEYWORD: return "keyword";
        case getWASM().Color.NUMBER: return "number";
    }
}
// Get colors from settings
export function getColors2(colorType: Color) {
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
    return config.get<boolean>('renderSymbolsOutsideMath');
}
// Retreive the settings for space rendering
export function renderSpace() {
    const config = vscode.workspace.getConfiguration('typst-math');
    return config.get<boolean>('renderSpace');
}
// Retreive blacklisted symbols
export function blacklistedSymbols() {
    const config = vscode.workspace.getConfiguration('typst-math');
    return config.get<string[]>('blacklist') || [];
}

// Get the rendering mode
export function renderingMode() {
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