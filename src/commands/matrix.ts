import * as vscode from 'vscode';

function generateMatrixSnippet(height: number, width: number) {
    let snippet = "mat(\n";
    for (let i = 0; i < height; i++) {
        for (let j = 0; j < width; j++) {
            snippet += `\${${i * width + j + 1}:0}`;
            if (j < width - 1) {
                snippet += ", ";
            }
        }
        if (i < height - 1) {
            snippet += ";\n";
        }
    }
    snippet += ")";

    return new vscode.SnippetString(snippet);
}

export const matrixCommand = vscode.commands.registerCommand('typst-math.matrix', async () => {
    // Get matrix height
    let value = await vscode.window.showInputBox({
        prompt: "Matrix height",
        placeHolder: "3",
        validateInput: (value: string) => {
            let height = parseInt(value);
            if (isNaN(height)) {
                return "Invalid height";
            }
            return undefined;
        },
    });
    if (value === undefined) { return; }
    let height = parseInt(value);

    // Get matrix width
    value = await vscode.window.showInputBox({
        prompt: "Matrix width",
        placeHolder: "3",
        validateInput: (value: string) => {
            let height = parseInt(value);
            if (isNaN(height)) {
                return "Invalid width";
            }
            return undefined;
        },
    });
    if (value === undefined) { return; }
    let width = parseInt(value);

    // Insert the snippet
    let editor = vscode.window.activeTextEditor;
    if (editor === undefined) { return; }
    editor.insertSnippet(generateMatrixSnippet(width, height), editor.selection);
});