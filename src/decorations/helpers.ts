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

/**
 * Check if ranges intersect and if the intersection is empty.
 * @param a The first range
 * @param b The second range
 * @returns Return `false` if ranges don't intersect or if intersection is empty.
 */
export function strictIntersection(a: vscode.Range, b: vscode.Range): boolean {
    let inter = a.intersection(b);
    if (inter !== undefined) {
        return !inter.isEmpty;
    }
    return false;
};