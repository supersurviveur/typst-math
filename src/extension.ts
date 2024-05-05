import * as vscode from 'vscode';
import { askForFonts, installFontsCommandProvider } from './commands/installFonts';
import { mathCommand } from './commands/math';
import { matrix2Command, matrix3Command, matrixCommand, squareMatrixCommand } from './commands/matrix';
import { dynamicDecorations, generateDecorations, resetGeneration } from './decorations/generateDecorations';
import { appendDecoration, createDecorationType, resetAllDecorations } from './decorations/helpers';
import { toggleSymbolsCommand } from './commands/toggleSymbols';
import { Logger } from './logger';

let wasm: typeof import("typst-math-rust");

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


let allDecorationsRust: {
    [key: string]: {
        decorationType: vscode.TextEditorDecorationType,
        ranges: vscode.DecorationOptions[],
    }
} = {
};
let currentRendering = 0;

function temp(currentId: number = 0) {
    if (activeEditor) {
        console.log(`Start temp`);
        console.time("test2");
        for (let t in allDecorationsRust) {
            allDecorationsRust[t].ranges = [];
        }
        let test = activeEditor;
        let decorations = wasm.test(activeEditor.document.getText() as string);
        for (let decoration of decorations) {
            if (!allDecorationsRust.hasOwnProperty(decoration.content)) {
                allDecorationsRust[decoration.content] = {
                    decorationType: createDecorationType({
                        contentText: decoration.symbol.symbol
                    }),
                    ranges: []
                };
            }
            let ranges = decoration.positions.map<vscode.DecorationOptions>((pos) => {
                return {
                    range: new vscode.Range(test.document.positionAt(pos.start), test.document.positionAt(pos.end)),
                };
            });
            allDecorationsRust[decoration.content].ranges = ranges;
        }
        console.log(`Hmm ${currentRendering} vs ${currentId}`);
        console.timeEnd("test2");
        console.time("test");
        for (let t in allDecorationsRust) {
            activeEditor?.setDecorations(
                allDecorationsRust[t].decorationType,
                allDecorationsRust[t].ranges
            );
        }
        console.timeEnd("test");
        Logger.info(`Loaded ${decorations.length} decorations`);
    }
}

export async function activate(context: vscode.ExtensionContext) {
    Logger.info("Activating extension");
    wasm = await import("typst-math-rust");
    wasm.init_lib();

    // Only on the first launch
    // context.globalState.update("firstLaunch", undefined);
    if (context.globalState.get("firstLaunch") === undefined) {
        askForFonts(context);
        context.globalState.update("firstLaunch", false);
    }

    // If settings or the current theme change, update the decorations
    // vscode.workspace.onDidChangeConfiguration(regenerateDecorations);
    // vscode.window.onDidChangeActiveColorTheme(regenerateDecorations);


    if (activeEditor) {
        temp();
        // updateDecorations(true);
    }

    vscode.window.onDidChangeActiveTextEditor(editor => {
        activeEditor = editor;
        if (editor) {
            temp();
            // updateDecorations(true);
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
                // temp();
                // updateDecorations(true);
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
            console.log("init timeout");
            currentRendering += 1;
            let tempId = currentRendering;
            selection_timeout = setTimeout(async () => {
                temp(tempId);
                // updateDecorations(temp_line !== undefined && temp_line !== event.selections[0].start.line);
            }, 200);
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
