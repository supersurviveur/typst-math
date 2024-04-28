import * as vscode from 'vscode';
import { askForFonts, installFontsCommandProvider } from './commands/installFonts';
import { mathCommand } from './commands/math';
import { matrix2Command, matrix3Command, matrixCommand, squareMatrixCommand } from './commands/matrix';
import { dynamicDecorations, generateDecorations, resetGeneration } from './decorations/generateDecorations';
import { resetAllDecorations } from './decorations/helpers';
import { toggleSymbolsCommand } from './commands/toggleSymbols';
import { Logger } from './logger';

let decorations: {
    decorationType: vscode.TextEditorDecorationType;
    getRanges: (document: vscode.TextEditor) => vscode.DecorationOptions[];
}[] = [];

let activeEditor = vscode.window.activeTextEditor;


export const regenerateDecorations = async () => {
    Logger.debug("Regenerating decorations");
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
    Logger.debug("Updating decorations");
    // if the current editor is not a typst editor, return
    if (!activeEditor || activeEditor.document.languageId !== "typst") {
        return;
    }
    let updateDecorations = await dynamicDecorations(activeEditor);
    if (needReload) {
        decorations = decorations.concat(await generateDecorations(activeEditor));
        Logger.info("Loaded " + decorations.length + " decorations");
    }
    for (let decoration of decorations) {
        activeEditor.setDecorations(decoration.decorationType, decoration.getRanges(activeEditor));
    }
    for (let decoration of updateDecorations) {
        activeEditor.setDecorations(decoration.decorationType, decoration.ranges);
    }
}

export async function activate(context: vscode.ExtensionContext) {
    Logger.info("Activating extension");
    const wasm = await import("typst-math-rust");
    vscode.window.showInformationMessage(wasm.testRS(vscode.window.activeTextEditor?.document.getText() as string));

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

    let change_timeout: NodeJS.Timeout | undefined = undefined;
    let last_edited_line: undefined | number = undefined;
    vscode.workspace.onDidChangeTextDocument(event => {
        if (activeEditor && event.document === activeEditor.document) {
            last_edited_line = event.contentChanges[0].range.start.line;
            // if there is no carriage return, do not update the decorations
            if (event.contentChanges.length === 0) { return; }
            let flag = false;
            for (let change of event.contentChanges) {
                if (change.text.includes("\n") || change.text.includes("\r")) {
                    flag = true;
                    break;
                }
            }
            if (!flag) { return; }
            if (change_timeout) {
                clearTimeout(change_timeout);
            }
            change_timeout = setTimeout(() => {
                updateDecorations(true);
            }, 100);
        }
    }, null, context.subscriptions);

    let selection_timeout: NodeJS.Timeout | undefined = undefined;
    vscode.window.onDidChangeTextEditorSelection(event => {
        if (activeEditor && event.textEditor === activeEditor) {
            let temp_line = last_edited_line;
            
            // If the selection changes, update the decorations after a short delay, to avoid updating the decorations too often
            if (selection_timeout) {
                clearTimeout(selection_timeout);
            }
            selection_timeout = setTimeout(() => {
                updateDecorations(temp_line !== undefined && temp_line !== event.selections[0].start.line);
            }, 50);
        }
        last_edited_line = undefined;
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
