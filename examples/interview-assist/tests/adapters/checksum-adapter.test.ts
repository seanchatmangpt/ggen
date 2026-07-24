import { describe, it, expect } from "vitest";
import { getChecksum } from "../../lib/adapters/checksum-adapter";

describe("checksum-adapter (real blake3 library calls, no mocks)", () => {
  it("hashes a known string to a stable, known digest", () => {
    const checksum = getChecksum();
    // Verified independently: `python3 -c "import blake3; print(blake3.blake3(b'hello world').hexdigest())"`
    // and Rust's `blake3::hash(b"hello world").to_hex()` both produce this
    // same digest -- BLAKE3 is a single, unkeyed, deterministic algorithm,
    // so all conformant implementations must agree on this constant.
    const expected = "d74981efa70a0c880b8d8c1985d075dbcbf679b99a5f9914e5aaf96b831a9e24";
    expect(checksum.hashHex("hello world")).toBe(expected);
  });

  it("is deterministic across repeated calls on the same input", () => {
    const checksum = getChecksum();
    const a = checksum.hashHex("interview-assist deterministic probe");
    const b = checksum.hashHex("interview-assist deterministic probe");
    expect(a).toBe(b);
    expect(a).toHaveLength(64); // 32 bytes hex-encoded
  });

  it("produces different digests for different inputs", () => {
    const checksum = getChecksum();
    expect(checksum.hashHex("a")).not.toBe(checksum.hashHex("b"));
  });

  it("accepts raw bytes as well as strings", () => {
    const checksum = getChecksum();
    const fromString = checksum.hashHex("abc");
    const fromBytes = checksum.hashHex(new TextEncoder().encode("abc"));
    expect(fromString).toBe(fromBytes);
  });
});
