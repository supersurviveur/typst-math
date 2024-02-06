import * as vscode from 'vscode';

const channel = vscode.window.createOutputChannel("Typst Math", { log: true });

export class Logger {
    static debug(message: string) {
        channel.debug(message);
    }

    static info(message: string) {
        channel.info(message);
    }

    static warn(message: string) {
        channel.warn(message);
    }

    static error(message: string) {
        channel.error(message);
    }
}
