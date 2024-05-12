// Helper module to handle wasm initialisation

let wasm: typeof import("typst-math-rust");
/// Initialize the WASM module
export async function initWASM() {
    wasm = await import("typst-math-rust");
    wasm.init_lib();
}

/// Get the WASM module
export default function getWASM() {
    if (!wasm) { throw Error("WASM is not yet initalized !!"); }
    return wasm;
}