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

function renderDecorations(hasSelection:  boolean = false) {
    console.time("renderDecorations");
    if (hasSelection && activeEditor?.selection) {
        let selection = activeEditor.selection;
        let reveal_selection = new vscode.Range(new vscode.Position(selection.start.line, 0), new vscode.Position(selection.end.line+1, 0));
        
        for (let t in allDecorationsRust) {
            activeEditor?.setDecorations(
                allDecorationsRust[t].decorationType,
                allDecorationsRust[t].ranges.filter(range => {
                    return range.range.intersection(reveal_selection) === undefined;
                })
            );
        }
    } else {
        for (let t in allDecorationsRust) {
            activeEditor?.setDecorations(
                allDecorationsRust[t].decorationType,
                allDecorationsRust[t].ranges
            );
        }
    }
    console.timeEnd("renderDecorations");
}
function reloadDecorations() {
    if (activeEditor) {
        console.time("reloadDecorations");
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
        renderDecorations();
        console.timeEnd("reloadDecorations");
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
    // vscode.workspace.onDidChangeConfiguration(reloadDecorations);
    // vscode.window.onDidChangeActiveColorTheme(reloadDecorations);


    if (activeEditor) {
        reloadDecorations();
        // updateDecorations(true);
    }

    vscode.window.onDidChangeActiveTextEditor(editor => {
        activeEditor = editor;
        if (editor) {
            reloadDecorations();
            // updateDecorations(true);
        }
    }, null, context.subscriptions);

    let editing = false;
    vscode.workspace.onDidChangeTextDocument(event => {
        editing = true;
    }, null, context.subscriptions);

    let selection_timeout: NodeJS.Timeout | undefined = undefined;
    let last_selection_line = {start: -1, end: -1};
    vscode.window.onDidChangeTextEditorSelection(event => {
        if (activeEditor && event.textEditor === activeEditor) {
            if (last_selection_line.start !== event.selections[0].start.line || last_selection_line.end !== event.selections[0].end.line) { // The cursor changes of line
                last_selection_line.start = event.selections[0].start.line;
                last_selection_line.end = event.selections[0].start.line;

                // If the selection changes, update the decorations after a short delay, to avoid updating the decorations too often
                if (selection_timeout) {
                    clearTimeout(selection_timeout);
                }

                if (editing) { // Text was typed, reload completely decorations
                    editing = false;
                    selection_timeout = setTimeout(async () => {
                        reloadDecorations();
                    }, 200);
                } else { // Only cursor was moved
                    selection_timeout = setTimeout(async () => {
                        renderDecorations(true);
                    }, 50); // 50ms to keep things fast
                }
            }
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
