import * as vscode from 'vscode';
import { appendDecoration, helperSimpleRegex } from './helpers';

export class DynamicGenerator {
    text: string;
    activeEditor: vscode.TextEditor;

    constructor(activeEditor: vscode.TextEditor) {
        this.text = activeEditor.document.getText();
        this.activeEditor = activeEditor;
    }

    /**
     * Helper function to decorate a basic regex match.
     * @param reg 
     * @param key 
     * @param decorationType 
     * @param getContent 
     */
    public simpleRegex(reg: RegExp, key: string, decorationType: vscode.ThemableDecorationAttachmentRenderOptions, getContent: (match: RegExpExecArray) => [string, string], pre?: RegExp, post?: RegExp, rangeStartOffset = 0, rangeEndOffset = 0) {
        helperSimpleRegex(this.text, this.activeEditor, reg, (match, range) => {
            const decoration = { range: range, hoverMessage: match[0] };

            const content = getContent(match);
            // Finally add the decoration
            appendDecoration(key, content[0], decoration, {
                ...decorationType,
                contentText: content[1]
            });
        }, pre, post, rangeStartOffset, rangeEndOffset);
    }
}