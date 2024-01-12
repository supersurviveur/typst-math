import * as vscode from 'vscode';

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
        placeHolder: "3"
    });
    if (value === undefined) { return; }
    let width = parseInt(value);
    if (isNaN(width)) {
        vscode.window.showErrorMessage("Invalid width");
        return;
    }

    // Generate the matrix snippet
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

    // Insert the snippet
    let editor = vscode.window.activeTextEditor;
    if (editor === undefined) { return; }
    editor.insertSnippet(new vscode.SnippetString(snippet), editor.selection);
});