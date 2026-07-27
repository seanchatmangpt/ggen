import "server-only";
import { blake3 } from "@noble/hashes/blake3.js";
import { canonicalJson } from "./canonical-json";

export function blake3Hex(value: unknown): string {
  return Buffer.from(blake3(new TextEncoder().encode(canonicalJson(value)))).toString("hex");
}
