import * as vscode from 'vscode';
import { askForFonts, installFontsCommandProvider } from './commands/installFonts';
import { mathCommand } from './commands/math';
import { matrix2Command, matrix3Command, matrixCommand, squareMatrixCommand } from './commands/matrix';
import { dynamicDecorations, generateDecorations, resetGeneration } from './decorations/generateDecorations';
import { resetAllDecorations } from './decorations/helpers';
import { toggleSymbolsCommand } from './commands/toggleSymbols';

let decorations: {
    decorationType: vscode.TextEditorDecorationType;
    getRanges: (document: vscode.TextEditor) => vscode.DecorationOptions[];
}[] = [];

let activeEditor = vscode.window.activeTextEditor;


export const regenerateDecorations = async () => {
    // Remove old decorations
    for (let decoration of decorations) {
        decoration.decorationType.dispose();
    }
    resetAllDecorations();
    resetGeneration();
    decorations = [];

    // Generate new decorations
    if (activeEditor) {
        decorations = decorations.concat(await generateDecorations(activeEditor));
    }

    updateDecorations(true);
};

async function updateDecorations(needReload = false) {
    // if the current editor is not a typst editor, return
    if (!activeEditor || activeEditor.document.languageId !== "typst") {
        return;
    }
    let updateDecorations = await dynamicDecorations(activeEditor);
    if (needReload) {
        decorations = decorations.concat(await generateDecorations(activeEditor));
        console.log("Length", decorations.length + updateDecorations.length);
    }
    for (let decoration of decorations) {
        activeEditor.setDecorations(decoration.decorationType, decoration.getRanges(activeEditor));
    }
    for (let decoration of updateDecorations) {
        activeEditor.setDecorations(decoration.decorationType, decoration.ranges);
    }
}

export async function activate(context: vscode.ExtensionContext) {
    // Only on the first launch
    // context.globalState.update("firstLaunch", undefined);
    if (context.globalState.get("firstLaunch") === undefined) {
        askForFonts(context);
        context.globalState.update("firstLaunch", false);
    }

    // If settings or the current theme change, update the decorations
    vscode.workspace.onDidChangeConfiguration(regenerateDecorations);
    vscode.window.onDidChangeActiveColorTheme(regenerateDecorations);


    if (activeEditor) {
        updateDecorations(true);
    }

    vscode.window.onDidChangeActiveTextEditor(editor => {
        activeEditor = editor;
        if (editor) {
            updateDecorations(true);
        }
    }, null, context.subscriptions);

    vscode.workspace.onDidChangeTextDocument(event => {
        if (activeEditor && event.document === activeEditor.document) {
            updateDecorations(true);
        }
    }, null, context.subscriptions);

    let selection_timeout: NodeJS.Timeout | undefined = undefined;
    vscode.window.onDidChangeTextEditorSelection(event => {
        if (activeEditor && event.textEditor === activeEditor) {
            // If the selection changes, update the decorations after a short delay, to avoid updating the decorations too often
            if (selection_timeout) {
                clearTimeout(selection_timeout);
            }
            selection_timeout = setTimeout(() => {
                updateDecorations();
            }, 50);
        }
    }, null, context.subscriptions);

    // Register commands
    context.subscriptions.push(toggleSymbolsCommand);
    context.subscriptions.push(mathCommand);
    context.subscriptions.push(matrixCommand);
    context.subscriptions.push(squareMatrixCommand);
    context.subscriptions.push(matrix2Command);
    context.subscriptions.push(matrix3Command);

    context.subscriptions.push(installFontsCommandProvider(context));
}

export function deactivate() { }
