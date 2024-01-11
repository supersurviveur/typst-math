import * as vscode from 'vscode';
import { dynamicDecorations, generateDecorations } from './decorations/generateDecorations';
import installfont from './installfontLib/installfont.js';

function installFontManually() {
    vscode.env.openExternal(vscode.Uri.parse("https://github.com/supersurviveur/typst-math/blob/main/fonts/")); // TODO
}

export function activate(context: vscode.ExtensionContext) {
    // Only on the first launch
    // context.globalState.update("firstLaunch", undefined);
    if (context.globalState.get("firstLaunch") === undefined) {
        vscode.window.showInformationMessage("Welcome to Typst Math! To correctly render math symbols, please install the fonts.", "Install Fonts with bundled program", "Install Fonts manually").then((value) => {
            if (value === "Install Fonts with bundled program") {
                // Get font directory from extension directory
                let fontDir = context.asAbsolutePath("fonts");
                // Install fonts
                try {
                    installfont(fontDir, function (err) {
                        if (err) {
                            throw err;
                        } else {
                            vscode.window.showInformationMessage("Fonts installed successfully");
                        }
                    });
                    // Editor needs to be completely restarted to load the fonts
                    vscode.window.showInformationMessage("Please completely restart the editor to load the fonts");
                } catch (err) {
                    vscode.window.showErrorMessage("Error while installing fonts, please install them manually.", "Install Fonts manually").then((value) => {
                        if (value === "Install Fonts manually") {
                            installFontManually();
                        }
                    });
                }
            } else if (value === "Install Fonts manually") {
                installFontManually();
            }
        });
        context.globalState.update("firstLaunch", false);
    }

    let decorations = generateDecorations();

    // If settings change, update the decorations
    vscode.workspace.onDidChangeConfiguration(() => {
        // Remove old decorations
        for (let decoration of decorations) {
            decoration.decorationType.dispose();
        }
        decorations = generateDecorations();
    });

    let activeEditor = vscode.window.activeTextEditor;

    function updateDecorations() {
        // if the current editor is noit a typst editor, return
        if (!activeEditor || activeEditor.document.languageId !== "typst") {
            return;
        }
        let updateDecorations = dynamicDecorations(activeEditor);
        for (let decoration of decorations) {
            activeEditor.setDecorations(decoration.decorationType, decoration.getRanges(activeEditor));
        }
        for (let decoration of updateDecorations) {
            activeEditor.setDecorations(decoration.decorationType, decoration.ranges);
        }
    }

    if (activeEditor) {
        updateDecorations();
    }

    vscode.window.onDidChangeActiveTextEditor(editor => {
        activeEditor = editor;
        if (editor) {
            updateDecorations();
        }
    }, null, context.subscriptions);

    vscode.workspace.onDidChangeTextDocument(event => {
        if (activeEditor && event.document === activeEditor.document) {
            updateDecorations();
        }
    }, null, context.subscriptions);

    vscode.window.onDidChangeTextEditorSelection(event => {
        if (activeEditor && event.textEditor === activeEditor) {
            updateDecorations();
        }
    }, null, context.subscriptions);


    let disposable = vscode.commands.registerCommand('typst-math.matrix', async () => {
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

    context.subscriptions.push(disposable);
}

export function deactivate() { }
