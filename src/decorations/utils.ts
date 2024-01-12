import * as vscode from 'vscode';

// Get colors from settings
export function getColors() {
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