# Typst math VS Code Extension

A VS Code extension to simplify math writing in [Typst](https://typst.app/home).

# Installation

The extension can be downloaded from the [Visual Studio Marketplace](https://marketplace.visualstudio.com/items?itemName=surv.typst-math).

To preview math symbols, some fonts are required, which you can either install [manually](../assets/fonts/README.md) or let the extension install them automatically on first launch (works on Windows only).

Unfortunatly, you also need to set your theme colors in the extension settings, as the extension can't access theme colors directly. You can find the settings in `File > Preferences > Settings > Extensions > Typst Math`.
By default, the extension will use the monokai theme colors.

# Features

- Math snippets, commands and keywords to simplify math writing
- Math preview directly in the editor :
  - Render math symbols from : \
    ![Typst math without preview](../assets/math-without-preview.png)
  - To : \
    ![Preview some math symbols directly](../assets/math-preview.png) \
    When you edit a line containing math symbols, these symbols will be displayed as text (as in the first image) for easy editing.

# Settings

- **Colors**: Select your theme colors.
- **RenderSymbolsOutsideMath**: If set to true, the extension will render symbols everywhere in the document, not only in math equations.
- **RenderSpace**: If set to true, the extension will render space symbols like space, wj, space.quad...
- **RenderingMode**: Choose whether to render only simple symbols or also complex equations.

# TODO

- Installing fonts seems to work only on windows, recreate a clean font installation system
- Add more features :
  - Snippets
  - Commands
  - Keywords
  - Symbols
