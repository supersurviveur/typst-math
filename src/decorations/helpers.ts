import * as vscode from 'vscode';

export function createDecorationType(options: vscode.ThemableDecorationAttachmentRenderOptions) {
    return vscode.window.createTextEditorDecorationType({
        textDecoration: 'none; position: absolute; opacity: 0;',
        after: {
            ...options,
            textDecoration: `none;${options.textDecoration}`,
        }
    });
}