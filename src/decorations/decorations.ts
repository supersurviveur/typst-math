import * as vscode from 'vscode';
import { createDecorationType, strictIntersection } from './helpers';
import { Logger } from '../logger';
import { blacklistedSymbols, getColors, getRenderingMode, customSymbols, renderSpaces, renderSymbolsOutsideMath } from '../utils';
import getWASM from '../wasmHelper';
import { updateStatusBarItem } from '../statusbar';
import { CustomSymbol } from 'typst-math-rust';

export class Decorations {
    allDecorations: {
        [key: string]: {
            decorationType: vscode.TextEditorDecorationType,
            ranges: vscode.DecorationOptions[],
        }
    } = {}; // Keep decorations and ranges in this variable
    selection_timeout: NodeJS.Timeout | undefined = undefined;
    last_selection_line = { start: -1, end: -1 }; // Keep the position of the last edited line
    editing = false; // Used to know if the document was edited
    edition_state: {
        reload_type: -2 | -1 | 0,
        edited_range: vscode.Range | undefined,
        selection_end: vscode.Position | undefined
    } = {
            reload_type: -1,
            edited_range: undefined,
            selection_end: undefined
        };
    offset = 0; // Line offset of the edition, used to translate symbols
    activeEditor = vscode.window.activeTextEditor;
    rendering = true;
    // VSCode settings
    renderingMode = getRenderingMode();
    renderOutsideMath = renderSymbolsOutsideMath();
    renderSpaces = renderSpaces();
    blacklistedSymbols = blacklistedSymbols();
    customSymbols: CustomSymbol[] = [];

    // generate a list of custom symbols
    generateCustomSymbols() {
        this.customSymbols = [];
        let custom = customSymbols();
        for (let value of custom) {
            this.customSymbols.push(getWASM().generate_custom_symbol(value.name, value.symbol, value.category));
        }
    }
    // Render decorations, while revealing current line
    renderDecorations() {
        console.time("renderDecorations");
        if (this.activeEditor?.selection) {
            // Generate ranges to reveal
            let reveal_selections: vscode.Range[] = [];
            for (let selection of this.activeEditor.selections) {
                reveal_selections.push(new vscode.Range(
                    new vscode.Position(selection.start.line, 0),
                    new vscode.Position(selection.end.line, this.activeEditor.document.lineAt(selection.end.line).text.length)));
            }

            for (let t in this.allDecorations) {
                this.activeEditor?.setDecorations(
                    this.allDecorations[t].decorationType,
                    this.allDecorations[t].ranges.filter(range => {
                        return reveal_selections.every(reveal_selection => range.range.intersection(reveal_selection) === undefined);
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
            this.allDecorations[key].decorationType.dispose();
        }
        this.allDecorations = {};
        this.edition_state = { // Force complete rendering next time
            reload_type: -2,
            edited_range: undefined,
            selection_end: undefined
        };
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
            let editor = this.activeEditor; // Make typescript happy

            // Get symbols list
            this.generateCustomSymbols();

            let parsed = getWASM().parse_document(this.activeEditor.document.getText() as string, this.edition_state.edited_range?.start.line || -1, this.edition_state.edited_range?.end.line || -1, this.renderingMode, this.renderOutsideMath, this.renderSpaces, this.blacklistedSymbols, this.customSymbols);

            // If edited lines aren't defined, we clear all ranges
            // If they are defined, remove symbols whiwh were rendered again, and trnaslate ones after the edition
            if (this.edition_state.reload_type < 0) {
                // Reset ranges
                for (let t in this.allDecorations) {
                    this.allDecorations[t].ranges = [];
                }
            } else {
                let reparsed_range = new vscode.Range(new vscode.Position(parsed.edit_start_line, parsed.edit_start_column), new vscode.Position(parsed.edit_end_line, parsed.edit_end_column));
                let edited_range = this.edition_state.edited_range as vscode.Range;
                let selection_end = this.edition_state.selection_end as vscode.Position;
                for (let t in this.allDecorations) {
                    // Remove ones that are in the edited range
                    this.allDecorations[t].ranges = this.allDecorations[t].ranges.filter(range => {
                        return !strictIntersection(new vscode.Range(edited_range.start, selection_end), range.range);
                    });
                    // Translate ones that are after the edition
                    this.allDecorations[t].ranges = this.allDecorations[t].ranges.map(range => {
                        if (range.range.start.isAfterOrEqual(selection_end)) {
                            let nstart = range.range.start.line + this.offset < 0 ? new vscode.Position(0, 0) : range.range.start.translate(this.offset);
                            let nend = range.range.end.line + this.offset < 0 ? new vscode.Position(0, 0) : range.range.end.translate(this.offset);
                            return {
                                range: new vscode.Range(nstart, nend),
                            };
                        } else {
                            return range;
                        }
                    });
                    // Remove ones that are in the reparsed range or outside the document
                    let document = this.activeEditor?.document;
                    this.allDecorations[t].ranges = this.allDecorations[t].ranges.filter(range => {
                        return !strictIntersection(reparsed_range, range.range) && document.lineAt(document.lineCount -1).range.end.isAfter(range.range.end);
                    });
                }
            }

            for (let decoration of parsed.decorations) {
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
                this.allDecorations[decoration.uuid].ranges.push(...ranges);
            }
            // Reset edited line
            // If there is an error in AST, force complete rendering next time, otherwise some symbols are never renderer after errors like missing $
            this.edition_state = {
                reload_type: parsed.erroneous ? -2 : -1,
                edited_range: undefined,
                selection_end: undefined
            };
            console.timeEnd("reloadDecorations");
            this.renderDecorations();
            updateStatusBarItem(this);
            Logger.info(`Loaded ${Object.keys(this.allDecorations).length} decorations`);
        }
    }

    // When the selection change, check if a reload and/or a render is needed
    onSelectionChange(event: vscode.TextEditorSelectionChangeEvent) {
        if (this.activeEditor && event.textEditor === this.activeEditor && this.activeEditor.document.languageId === "typst" && this.rendering && this.renderingMode > 0) {
            if (this.last_selection_line.start !== event.selections[0].start.line || this.last_selection_line.end !== event.selections[0].end.line || event.selections.length > 1) { // The cursor changes of line or ther is more than one selection
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

            // This part compute the edited range and update it for incremental rendering
            // negative values mean that next rendering will be complete, but -2 force it while -1 let the next edition change the edition range
            if (event.contentChanges.length > 1) { // too many changes, next rendering will be complete
                this.offset = 0;
                this.edition_state = {
                    reload_type: -2,
                    edited_range: undefined,
                    selection_end: undefined
                };
            } else if (this.edition_state.reload_type === -1) { // First change since last rendering
                this.offset = event.contentChanges[0].text.split("\n").length - 1 - (event.contentChanges[0].range.end.line - event.contentChanges[0].range.start.line);
                this.edition_state = {
                    reload_type: 0,
                    edited_range: new vscode.Range(event.contentChanges[0].range.start.line, 0, event.contentChanges[0].range.start.line + event.contentChanges[0].text.split("\n").length - 1, this.activeEditor.document.lineAt(event.contentChanges[0].range.start.line + event.contentChanges[0].text.split("\n").length - 1).range.end.character),
                    selection_end: event.contentChanges[0].range.end
                };
            } else if (this.edition_state.reload_type >= 0) { // Not the first change
                // Compute aditionnal offset
                this.offset += event.contentChanges[0].text.split("\n").length - 1 - (event.contentChanges[0].range.end.line - event.contentChanges[0].range.start.line);

                let current = this.edition_state.edited_range as vscode.Range;
                if (event.contentChanges[0].range.start.line >= current.start.line
                    && event.contentChanges[0].range.end.line >= current.end.line) { // Not the first change, but just extend at the end
                    this.edition_state.edited_range = new vscode.Range(this.edition_state.edited_range?.start.line as number, this.edition_state.edited_range?.start.character as number, event.contentChanges[0].range.start.line + event.contentChanges[0].text.split("\n").length - 1, this.activeEditor.document.lineAt(event.contentChanges[0].range.start.line + event.contentChanges[0].text.split("\n").length - 1).range.end.character);
                } else if (event.contentChanges[0].range.end.line <= current.end.line
                    && event.contentChanges[0].range.start.line <= current.start.line) { // Not the first change, but extend at the start
                    this.edition_state.edited_range = new vscode.Range(event.contentChanges[0].range.start.line, 0, this.edition_state.edited_range?.end.line as number, this.edition_state.edited_range?.end.character as number);
                } else { // Not the first change, next rendering will be complete 
                    this.offset = 0;
                    this.edition_state = {
                        reload_type: -2,
                        edited_range: undefined,
                        selection_end: undefined
                    };
                }
            }
        }
    }

    // When the editor change, update activeEditor and reload decorations
    onActiveTextEditorChange(editor: vscode.TextEditor | undefined) {
        this.activeEditor = editor;
        if (this.activeEditor) {
            this.clearDecorations(); // Clear decorations on the previous editor
            this.reloadDecorations();
        }
        updateStatusBarItem(this);
    }
}