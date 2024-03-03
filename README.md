# Typst math VS Code Extension

A VS Code extension to simplify math writing in [Typst](https://typst.app/home).

# Installation

The extension can be downloaded from the [Visual Studio Marketplace](https://marketplace.visualstudio.com/items?itemName=surv.typst-math).

To preview math symbols, some fonts are needed, you can install them [manually](./fonts/README.md) or let the extension install them automatically on first launch (Works only on Windows).

Unfortunatly, you also need to set your theme colors in the extension settings, because the extension can't access the theme colors directly. You can find the settings in `File > Preferences > Settings > Extensions > Typst Math`.
By default, the extension will use the monokai theme colors.

# Features

- Math snippets, commands and keywords to simplify math writing
- Math preview directly in the editor :
  - Render math symbols from : \
    ![Typst math without preview](./github/math-without-preview.png)
  - To : \
    ![Preview some math symbols directly](./github/math-preview.png) \
    When you edit a line containing math symbols, these symbols will be rendered as text (as in the first image) to allow you to edit them easily.

# Settings

- **Colors**: Select your theme colors.
- **RenderSymbolsOutsideMath**: If set to true, the extension will render symbols everywhere in the document, not only in math equations.
- **RenderPunctuation**: If set to true, the extension will render punctuation symbols like space, wj, space.quad...
- **RenderingMode**: Choose if you want to render just simple symbols or complex equations too.

# TODO

- Installing fonts seems to work only on windows, recreate a clean font installation system
- Add more features :
  - Snippets
  - Commands
  - Keywords
  - Symbols
