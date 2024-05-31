# Contributing
This project contains three parts :
- The `typst-math-rust` folder contains the Rust code for the WebAssembly module. It parses a text document to an AST and compute the math symbols to render.
- The `typst-math-macros` folder contains helper macros for the Rust code.
- The main folder contains the TypeScript code for the VS Code extension.

## Build and run
```bash
git clone https://github.com/supersurviveur/typst-math.git
cd typst-math
```
To build the WebAssembly module, you need to install [wasm-pack](https://rustwasm.github.io/wasm-pack/installer/)
```bash
cd typst-math-rust
wasm-pack build
```
Then you can run the extension by opening the project in VS Code and pressing `F5` to start a debug session.

### Tests
You can run tests with `cargo test`, and see [test coverage](https://doc.rust-lang.org/rustc/instrument-coverage.html) by running `coverage.sh`.

### Rendering mode
A few rendering modes are available :
- `nothing` : No rendering
- `basic` : Only greek letters and shorthands are rendered
- `medium` : Attachments are rendered
- `complex` : Functions are rendered