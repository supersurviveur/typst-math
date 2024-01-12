import * as vscode from 'vscode';
import { showIntInputBox } from './utils';

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
    let height = await showIntInputBox("Matrix height", "3");
    if (height === undefined) { return; }

    // Get matrix width
    let width = await showIntInputBox("Matrix width", "3");
    if (width === undefined) { return; }

    // Insert the snippet
    let editor = vscode.window.activeTextEditor;
    if (editor === undefined) { return; }
    editor.insertSnippet(generateMatrixSnippet(height, width), editor.selection);
});

export const squareMatrixCommand = vscode.commands.registerCommand('typst-math.square-matrix', async () => {
    // Get matrix size
    let size = await showIntInputBox("Matrix size", "3");
    if (size === undefined) { return; }

    // Insert the snippet
    let editor = vscode.window.activeTextEditor;
    if (editor === undefined) { return; }
    editor.insertSnippet(generateMatrixSnippet(size, size), editor.selection);
});

export const matrix2Command = vscode.commands.registerCommand('typst-math.matrix2', async () => {
    // Insert the snippet
    let editor = vscode.window.activeTextEditor;
    if (editor === undefined) { return; }
    editor.insertSnippet(generateMatrixSnippet(2, 2), editor.selection);
});

export const matrix3Command = vscode.commands.registerCommand('typst-math.matrix3', async () => {
    // Insert the snippet
    let editor = vscode.window.activeTextEditor;
    if (editor === undefined) { return; }
    editor.insertSnippet(generateMatrixSnippet(3, 3), editor.selection);
});