import * as vscode from 'vscode';
import { dynamicDecorations, generateDecorations } from './decorations/generateDecorations';
import installfont from './installfontLib/installfont.js';
import { matrixCommand } from './commands/matrix';

function installFontManually() {
    vscode.env.openExternal(vscode.Uri.parse("https://github.com/supersurviveur/typst-math/blob/main/fonts/"));
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
                            // Editor needs to be completely restarted to load the fonts
                            vscode.window.showInformationMessage("Fonts installed successfully, please completely restart the editor to load the fonts");
                        }
                    });
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

    // Register commands
    context.subscriptions.push(matrixCommand);
}

export function deactivate() { }
