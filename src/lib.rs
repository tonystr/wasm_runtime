//! wasm_runtime is a self-contained webassembly runtime, capable 
//! of decoding WAT, and and executing WASM bytecode.
//!
//! This crate is based on the "Writing A Wasm Runtime In Rust" 
//! tutorial by Skanehira: <br />
//! <https://skanehira.github.io/writing-a-wasm-runtime-in-rust>

pub mod binary;
pub mod execution;
