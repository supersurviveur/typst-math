// Helper module to handle wasm initialisation

let wasm: typeof import("typst-math-rust");
export async function initWASM() {
    wasm = await import("typst-math-rust");
    wasm.init_lib();
}

export default function getWASM() {
    if (!wasm) { throw Error("WASM is not yet initalized !!"); }
    return wasm;
}