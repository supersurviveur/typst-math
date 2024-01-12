import * as vscode from 'vscode';

export const showIntInputBox = async (prompt: string, placeHolder: string) => {
    let value = await vscode.window.showInputBox({
        prompt: prompt,
        placeHolder: placeHolder,
        validateInput: (value: string) => {
            let height = parseInt(value);
            if (isNaN(height)) {
                return `Invalid ${prompt.toLowerCase()}`;
            }
            return undefined;
        },
    });
    if (value === undefined) { return; }
    return parseInt(value);
};