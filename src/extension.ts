import * as vscode from 'vscode';
import { askForFonts, installFontsCommandProvider } from './commands/installFonts';
import { mathCommand } from './commands/math';
import { matrix2Command, matrix3Command, matrixCommand, squareMatrixCommand } from './commands/matrix';
import { toggleSymbolsCommand } from './commands/toggleSymbols';
import { Logger } from './logger';
import { Decorations } from './decorations/decorations';
import { initWASM } from './wasmHelper';
import { initStatusBar, updateStatusBarItem } from './statusbar';


export async function activate(context: vscode.ExtensionContext) {
    Logger.info("Activating extension");
    // Init decorations module and WASM
    await initWASM(); // Wait for initialisation
    const decorations = new Decorations();
    decorations.generateCustomSymbols();

    // Only on the first launch
    // context.globalState.update("firstLaunch", undefined);
    if (context.globalState.get("firstLaunch") === undefined) {
        askForFonts(context);
        context.globalState.update("firstLaunch", false);
    }

    // Status bar item
    const statusBarItem = initStatusBar();
    context.subscriptions.push(vscode.commands.registerCommand('typst-math.decorations-count', () => {
        decorations.toggleRendering();
    }));
    context.subscriptions.push(statusBarItem);

    // Load decorations if an editor is open
    if (vscode.window.activeTextEditor) {
        decorations.reloadDecorations();
    }

    // If settings or the current theme change, update the decorations
    vscode.workspace.onDidChangeConfiguration(decorations.onConfigChange.bind(decorations));
    vscode.window.onDidChangeActiveColorTheme(decorations.clearDecorations.bind(decorations));
    // Reloading symbols
    vscode.window.onDidChangeActiveTextEditor(decorations.onActiveTextEditorChange.bind(decorations));
    vscode.workspace.onDidChangeTextDocument(decorations.onTextDocumentChange.bind(decorations));
    vscode.window.onDidChangeTextEditorSelection(decorations.onSelectionChange.bind(decorations));

    // Register commands
    context.subscriptions.push(toggleSymbolsCommand(decorations));
    context.subscriptions.push(mathCommand);
    context.subscriptions.push(matrixCommand);
    context.subscriptions.push(squareMatrixCommand);
    context.subscriptions.push(matrix2Command);
    context.subscriptions.push(matrix3Command);

    context.subscriptions.push(installFontsCommandProvider(context));
}

export function deactivate() { }
