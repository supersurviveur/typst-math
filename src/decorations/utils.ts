import * as vscode from 'vscode';

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