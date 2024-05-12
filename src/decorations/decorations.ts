import * as vscode from 'vscode';
import { createDecorationType } from './helpers';
import { Logger } from '../logger';
import { blacklistedSymbols, getColors, getRenderingMode, renderSpaces, renderSymbolsOutsideMath } from '../utils';
import getWASM from '../wasmHelper';
import { updateStatusBarItem } from '../statusbar';

export class Decorations {
    allDecorations: {
        [key: string]: {
            decorationType: vscode.TextEditorDecorationType,
            ranges: vscode.DecorationOptions[],
        }
    } = {};
    selection_timeout: NodeJS.Timeout | undefined = undefined;
    last_selection_line = { start: -1, end: -1 };
    editing = false;
    rendering = true;
    renderingMode = getRenderingMode();
    renderOutsideMath = renderSymbolsOutsideMath();
    renderSpaces = renderSpaces();
    blacklistedSymbols = blacklistedSymbols();
    activeEditor = vscode.window.activeTextEditor;

    // Render decorations, while revealing current line
    renderDecorations() {
        console.time("renderDecorations");
        if (this.activeEditor?.selection) {
            let selection = this.activeEditor.selection;
            let reveal_selection = new vscode.Range(
                new vscode.Position(selection.start.line, 0),
                new vscode.Position(selection.end.line, this.activeEditor.document.lineAt(selection.end.line).text.length));

            for (let t in this.allDecorations) {
                this.activeEditor?.setDecorations(
                    this.allDecorations[t].decorationType,
                    this.allDecorations[t].ranges.filter(range => {
                        return range.range.intersection(reveal_selection) === undefined;
                    })
                );
            }
        } else {
            for (let t in this.allDecorations) {
                this.activeEditor?.setDecorations(
                    this.allDecorations[t].decorationType,
                    this.allDecorations[t].ranges
                );
            }
        }
        console.timeEnd("renderDecorations");
    }
    toggleRendering() {
        this.rendering = !this.rendering;
        if (this.rendering) {
            this.reloadDecorations();
        } else {
            this.clearDecorations();
        }
        updateStatusBarItem(this);
    }
    clearDecorations() {
        // Reset decorations on all editors
        for (const key in this.allDecorations) {
            vscode.window.visibleTextEditors.forEach(editor => {
                editor.setDecorations(this.allDecorations[key].decorationType, []);
            });
        }
        this.allDecorations = {};
        this.reloadDecorations();
    }
    onConfigChange(event: vscode.ConfigurationChangeEvent) {
        if (event.affectsConfiguration("typst-math")) {
            this.renderingMode = getRenderingMode();
            this.renderOutsideMath = renderSymbolsOutsideMath();
            this.renderSpaces = renderSpaces();
            this.blacklistedSymbols = blacklistedSymbols();
            this.clearDecorations();
        }
    }
    // Pass the current doc to typst to get symbols, and then render them
    reloadDecorations() {
        if (this.activeEditor && this.activeEditor.document.languageId === "typst" && this.rendering && this.renderingMode > 0) {
            console.time("reloadDecorations");
            // Reset ranges
            for (let t in this.allDecorations) {
                this.allDecorations[t].ranges = [];
            }
            let editor = this.activeEditor; // Make typescript happy

            // Get symbols list
            let decorations = getWASM().parse_document(this.activeEditor.document.getText() as string, this.renderingMode, this.renderOutsideMath, this.renderSpaces, this.blacklistedSymbols);
            for (let decoration of decorations) {
                if (!this.allDecorations.hasOwnProperty(decoration.uuid)) {
                    this.allDecorations[decoration.uuid] = {
                        decorationType: createDecorationType({
                            contentText: decoration.symbol,
                            color: getColors(decoration.color),
                            textDecoration: decoration.text_decoration
                        }),
                        ranges: []
                    };
                }
                // Generate ranges with rust data
                let ranges = decoration.positions.map<vscode.DecorationOptions>((pos) => {
                    return {
                        range: new vscode.Range(editor.document.positionAt(pos.start), editor.document.positionAt(pos.end)),
                    };
                });
                this.allDecorations[decoration.uuid].ranges = ranges;
            }
            console.timeEnd("reloadDecorations");
            this.renderDecorations();
            updateStatusBarItem(this);
            Logger.info(`Loaded ${Object.keys(this.allDecorations).length} decorations`);
        }
    }

    // When the selection change, check if a reload and/or a render is needed
    onSelectionChange(event: vscode.TextEditorSelectionChangeEvent) {
        if (this.activeEditor && event.textEditor === this.activeEditor && this.activeEditor.document.languageId === "typst" && this.rendering && this.renderingMode > 0) {
            if (this.last_selection_line.start !== event.selections[0].start.line || this.last_selection_line.end !== event.selections[0].end.line) { // The cursor changes of line
                this.last_selection_line.start = event.selections[0].start.line;
                this.last_selection_line.end = event.selections[0].end.line;

                // If the selection changes, update the decorations after a short delay, to avoid updating the decorations too often
                if (this.selection_timeout) {
                    clearTimeout(this.selection_timeout);
                }

                if (this.editing) { // Text was typed, reload completely decorations
                    this.editing = false;
                    this.selection_timeout = setTimeout(async () => {
                        this.reloadDecorations();
                    }, 200);
                } else { // Only cursor was moved, just render decorations by revealing current line
                    this.selection_timeout = setTimeout(async () => {
                        this.renderDecorations();
                    }, 50); // 50ms to keep things fast, but not to quick to avoid rendering too often
                }
            }
        }
    }

    // This event is useful for finding out, when selection changes, if the last changes were made by typing or simply moving the cursor
    onTextDocumentChange(event: vscode.TextDocumentChangeEvent) {
        if (this.activeEditor && event.document === this.activeEditor.document) {
            if (event.contentChanges.length === 0) { return; }
            this.editing = true;
        }
    }

    // When the editor change, update activeEditor and reload decorations
    onActiveTextEditorChange(editor: vscode.TextEditor | undefined) {
        this.activeEditor = editor;
        if (this.activeEditor) {
            this.reloadDecorations();
        }
    }
}