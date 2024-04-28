import * as vscode from 'vscode';
import { renderSymbolsOutsideMath } from './utils';

let functions_list: string[] = [
    "arrow",
    "overline",
    "dot",
    "hat",
    "tilde",
    "sqrt",
    "abs",
    "norm",
    "dot.double",
    "dot.triple",
    "dot.quad",
    "diaer"
];

let allDecorations: {
    [key: string]: {
        [key: string]: {
            decorationType: vscode.TextEditorDecorationType,
            ranges: vscode.DecorationOptions[],
        }
    }
} = {
};

export function resetAllDecorations() {
    // Clear current decorations
    for (const key in allDecorations) {
        for (const subKey in allDecorations[key]) {
            // Reset decorations on all editors
            vscode.window.visibleTextEditors.forEach(editor => {
                editor.setDecorations(allDecorations[key][subKey].decorationType, []);
            });
        }
    }
    allDecorations = {};
}
export function getAllDecorations() {
    return allDecorations;
}

export function helperSimpleRegex(text: string, activeEditor: vscode.TextEditor, reg: RegExp, onMatch: (match: RegExpExecArray, range: vscode.Range) => void, pre?: RegExp, post?: RegExp, rangeStartOffset = 0, rangeEndOffset = 0) {
    // Add "#sym." to match also symbols outisde maths
    // Replace \1 with \2 because we create another group who replace the first one
    const completeReg = new RegExp(`(${(pre ? pre.source.replace("\\1", "\\2") : '')}|#sym\\.)` + reg.source + (post ? post.source : ''), 'g');
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
            if (post) {
                // If the match end with an (, don't render it if it is in function_list
                if (text[endIndex] === '(' && functions_list.includes(newMatch[0])) {
                    continue;
                } 
            }
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

        // Reveal the entire line
        const revealRange = new vscode.Range(new vscode.Position(startPos.line, 0), new vscode.Position(endPos.line, activeEditor.document.lineAt(endPos.line).text.length));
        if (activeEditor.selection.intersection(revealRange)) {
            continue;
        }

        let flag = false;
        // Check if we are in math mode
        // Get the text before the match, and count if there is an even number of $
        const before = activeEditor.document.getText(new vscode.Range(new vscode.Position(0, 0), startPos.translate(0, 1)));
        let count = (before.match(/\$/g) || []).length; // Count all $
        count -= (before.match(/\\\$/g) || []).length; // Remove escaped $
        if (count % 2 === 1) { flag = true; }

        // Check if there is the "#sym." prefix, if the user wants to match also symbols outside math mode
        if (renderSymbolsOutsideMath()) {
            try { // Try and catch to avoid errors if the match is at the beginning of the file (refactor needed)
                const before2 = activeEditor.document.getText(new vscode.Range(startPos.translate(0, -5), startPos));
                if (before2 === '#sym.') { flag = true; }
            } catch (e) { }
        }


        if (!flag) { continue; }
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