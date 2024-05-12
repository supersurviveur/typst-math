import * as vscode from 'vscode';
import { Decorations } from '../decorations/decorations';

export const toggleSymbolsCommand = (decorations: Decorations) => {
    return vscode.commands.registerCommand('typst-math.toggle-symbols', async () => {
        decorations.toggleRendering();
    });
};