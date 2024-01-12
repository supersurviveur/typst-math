import * as vscode from 'vscode';

const allDecorations: {
    [key: string]: {
        [key: string]: {
            decorationType: vscode.TextEditorDecorationType,
            ranges: vscode.DecorationOptions[],
        }
    }
} = {
};

export function getAllDecorations() {
    return allDecorations;
}

export function helperSimpleRegex(text: string, activeEditor: vscode.TextEditor, reg: RegExp, onMatch: (match: RegExpExecArray, range: vscode.Range) => void, pre?: RegExp, post?: RegExp, rangeStartOffset = 0, rangeEndOffset = 0) {
    const completeReg = new RegExp((pre ? pre.source : '') + reg.source + (post ? post.source : ''), 'g');
    let match;
    while ((match = completeReg.exec(text))) {
        let newMatch;
        let startIndex = 0;
        let endIndex = 0;
        if (pre || post) {
            reg.lastIndex = 0; // Reset the regex
            newMatch = reg.exec(match[0]);
            if (!newMatch) { continue; }
            startIndex = match.index + newMatch.index;
            endIndex = match.index + newMatch.index + newMatch[0].length;
        } else {
            newMatch = match;
            startIndex = match.index;
            endIndex = match.index + match[0].length;
        }
        // Check if the match is between pre and post
        const startPos = activeEditor.document.positionAt(startIndex);
        const endPos = activeEditor.document.positionAt(endIndex);
        const range = new vscode.Range(startPos, endPos);


        // If the cursor is near the match, we don't decorate it

        // reveal 4 chars before and after the match
        // let revealStartPos = activeEditor.document.positionAt(startIndex + rangeStartOffset - 4);
        // if (revealStartPos.line !== startPos.line) {
        //     // Just reveal the current line
        //     revealStartPos = new vscode.Position(startPos.line, 0);
        // }
        // let revealEndPos = activeEditor.document.positionAt(endIndex + rangeEndOffset + 4);
        // if (revealEndPos.line !== endPos.line) {
        //     // Just reveal the current line
        //     revealEndPos = new vscode.Position(endPos.line, activeEditor.document.lineAt(endPos.line).text.length);
        // }
        // const revealRange = new vscode.Range(revealStartPos, revealEndPos);
        
        // Reveal the entire line
        const revealRange = new vscode.Range(new vscode.Position(startPos.line, 0), new vscode.Position(endPos.line, activeEditor.document.lineAt(endPos.line).text.length));
        if (revealRange.contains(activeEditor.selection)) { continue; }

        // Check if we are in math mode
        // Get the text before the match, and count if there is an even number of $
        const before = activeEditor.document.getText(new vscode.Range(new vscode.Position(0, 0), startPos.translate(0, 1)));
        let count = (before.match(/\$/g) || []).length; // Count all $
        count -= (before.match(/\\\$/g) || []).length; // Remove escaped $
        if (count % 2 === 0) { continue; }

        onMatch(newMatch, range);
    }
}

export function createDecorationType(options: vscode.ThemableDecorationAttachmentRenderOptions) {
    return vscode.window.createTextEditorDecorationType({
        textDecoration: 'none; position: absolute; opacity: 0;',
        after: options
    });
}

/**
 * Helper func to append a decoration to allDecorations.
 * If the decorationType doesn't exist, it is created.
 * Else, the decoration range is appended to the existing decorationType.
 * @param key 
 * @param subKey 
 * @param decoration 
 * @param decorationType 
 */
export function appendDecoration(key: string, subKey: string, decoration: vscode.DecorationOptions, decorationType: vscode.ThemableDecorationAttachmentRenderOptions) {
    if (!allDecorations.hasOwnProperty(key)) {
        allDecorations[key] = {};
    }
    if (!allDecorations[key].hasOwnProperty(subKey)) {
        allDecorations[key][subKey] = {
            decorationType: createDecorationType(decorationType),
            ranges: [decoration]
        };
    } else {
        allDecorations[key][subKey].ranges.push(decoration);
    }
}