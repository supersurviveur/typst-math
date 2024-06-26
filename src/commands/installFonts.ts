import * as vscode from "vscode";
import installfont from "../installfontLib/installfont";

function installFontManually() {
    vscode.env.openExternal(vscode.Uri.parse("https://github.com/supersurviveur/typst-math/blob/main/assets/fonts/README.md"));
}

export function askForFonts(context: vscode.ExtensionContext) {
    // show the automatic installion prompt only on windows
    let buttons = [];
    if (process.platform === "win32") {
        buttons.push("Install Fonts with bundled program");
    }
    buttons.push("Install Fonts manually");
    vscode.window.showInformationMessage("Welcome to Typst Math! To correctly render math symbols, please install the fonts.", ...buttons).then((value) => {
        if (value === "Install Fonts with bundled program") {
            // Get font directory from extension directory
            let fontDir = context.asAbsolutePath("fonts");
            // Install fonts
            try {
                installfont(fontDir, function (err) {
                    if (err) {
                        throw err;
                    } else {
                        // Editor needs to be completely restarted to load the fonts
                        vscode.window.showInformationMessage("Fonts installed successfully, please completely restart the editor to load the fonts");
                    }
                });
            } catch (err) {
                vscode.window.showErrorMessage("Error while installing fonts, please install them manually.", "Install Fonts manually").then((value) => {
                    if (value === "Install Fonts manually") {
                        installFontManually();
                    }
                });
            }
        } else if (value === "Install Fonts manually") {
            installFontManually();
        }
    });
}

export const installFontsCommandProvider = (context: vscode.ExtensionContext) => {
    return vscode.commands.registerCommand('typst-math.install-fonts', () => {
        askForFonts(context);
    });
};