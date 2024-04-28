import * as vscode from 'vscode';
import { toggleSymbols } from '../decorations/generateDecorations';
import { regenerateDecorations } from '../extension';

export const toggleSymbolsCommand = vscode.commands.registerCommand('typst-math.toggle-symbols', async () => {
    toggleSymbols();
    regenerateDecorations();
});
