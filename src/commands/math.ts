import * as vscode from 'vscode';
import { showIntInputBox } from './utils';

function generateMatrixSnippet(height: number, width: number) {
    let snippet = "mat(\n";
    for (let i = 0; i < height; i++) {
        let temp = [];
        for (let j = 0; j < width; j++) {
            temp.push(`\${${i * width + j + 1}:0}`);
        }
        snippet += temp.join(", ");
        if (i < height - 1) {
            snippet += ";\n";
        }
    }
    snippet += ")";

    return new vscode.SnippetString(snippet);
}

export const mathCommand = vscode.commands.registerCommand('typst-math.math', async () => {
    let editor = vscode.window.activeTextEditor;
    if (editor === undefined) { return; }
    let snippet = new vscode.SnippetString("\$$TM_SELECTED_TEXT${1}\$${0}");
    editor.insertSnippet(snippet, editor.selection);
});
