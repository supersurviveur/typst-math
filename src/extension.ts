import * as vscode from 'vscode';
import { dynamicDecorations, generateDecorations } from './decorations/generateDecorations';
import { matrix2Command, matrix3Command, matrixCommand, squareMatrixCommand } from './commands/matrix';
import { askForFonts, installFontsCommandProvider } from './commands/installFonts';
import { resetAllDecorations } from './decorations/helpers';


export function activate(context: vscode.ExtensionContext) {
    // Only on the first launch
    // context.globalState.update("firstLaunch", undefined);
    if (context.globalState.get("firstLaunch") === undefined) {
        askForFonts(context);
        context.globalState.update("firstLaunch", false);
    }

    let decorations = generateDecorations();

    const regenerateDecorations = () => {
        // Remove old decorations
        for (let decoration of decorations) {
            decoration.decorationType.dispose();
        }
        resetAllDecorations();
        // Generate new decorations
        decorations = generateDecorations();

        updateDecorations();
    };

    // If settings or the current theme change, update the decorations
    vscode.workspace.onDidChangeConfiguration(regenerateDecorations);
    vscode.window.onDidChangeActiveColorTheme(regenerateDecorations);

    let activeEditor = vscode.window.activeTextEditor;

    function updateDecorations() {
        // if the current editor is not a typst editor, return
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
    context.subscriptions.push(squareMatrixCommand);
    context.subscriptions.push(matrix2Command);
    context.subscriptions.push(matrix3Command);

    context.subscriptions.push(installFontsCommandProvider(context));
}

export function deactivate() { }
