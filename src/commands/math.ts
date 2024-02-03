import * as vscode from 'vscode';

export const mathCommand = vscode.commands.registerCommand('typst-math.math', async () => {
    let editor = vscode.window.activeTextEditor;
    if (editor === undefined) { return; }
    let snippet = new vscode.SnippetString("\$$TM_SELECTED_TEXT${1}\$${0}");
    editor.insertSnippet(snippet, editor.selection);
});
