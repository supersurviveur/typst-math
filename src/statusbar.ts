
import * as vscode from 'vscode';
import { Decorations } from './decorations/decorations';

let statusBarItem: vscode.StatusBarItem;

export function initStatusBar() {
    statusBarItem = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Right, 100);
    statusBarItem.command = 'typst-math.decorations-count';
    statusBarItem.tooltip = 'Toggle decorations rendering';

    return statusBarItem;
}

export function updateStatusBarItem(decorations: Decorations): void {
    if (vscode.window.activeTextEditor && vscode.window.activeTextEditor.document.languageId === 'typst') {
        if (decorations.rendering) {
            const n = Object.values(decorations.allDecorations).length;
            if (n > 0) {
                statusBarItem.text = `Decorations: ${n}`;
                statusBarItem.show();
            } else {
                statusBarItem.hide();
            }
        } else {
            statusBarItem.text = `Decorations: $(eye-closed)`;
            statusBarItem.show();
        }
    } else {
        statusBarItem.hide();
    }
}