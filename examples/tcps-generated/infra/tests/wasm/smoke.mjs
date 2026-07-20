import { readFile } from "node:fs/promises";

const path = process.argv[2];
if (!path) throw new Error("wasm path is required");
const bytes = await readFile(path);
const { instance } = await WebAssembly.instantiate(bytes, {});
const version = instance.exports.tcps_wasm_version_v1();
if (version !== 0x1A0713) throw new Error(`version mismatch: ${version}`);
const outcome = instance.exports.tcps_wasm_select_canonical_v1(1n, 3n, 999, 1000n);
const selected = (outcome >> 63n) & 1n;
const tool = (outcome >> 48n) & 0x7fffn;
if (selected !== 1n || tool !== 1n) throw new Error(`unexpected outcome: ${outcome}`);
console.log("wasm-smoke: accepted");

