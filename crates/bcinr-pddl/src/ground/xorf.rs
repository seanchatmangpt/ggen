//! XOR filter — an approximate-membership structure with **no false
//! negatives**, ported (design, not code) from `bytestar/bytecore/abi/tables.h`.
//!
//! That header declares `bs_xorf_maybe_has(t, h1, h2, h3)` over three read-only
//! spans `a, b, c`: a key's three seeded hashes index three fingerprint
//! regions, and their XOR is compared to the key's fingerprint. This is the
//! classic construction of Graf & Lemire, *"Xor Filters: Faster and Smaller
//! Than Bloom and Cuckoo Filters"* (2020). We reimplement it in safe Rust — no
//! dependency on bytestar, and no `unsafe`.
//!
//! ## Why a grounder wants this
//!
//! The lazy grounder asks, millions of times, "could predicate P over these
//! argument IDs possibly be a fact?" The authoritative answer is a binary
//! search in the sorted fact store ([`crate::facts`]). The XOR filter is the
//! cheap gate in front of it: one hash and three byte loads reject the
//! overwhelming majority of impossible atoms with zero allocation, so the
//! sorted store is only touched for atoms that *might* exist. No false
//! negatives means the gate is sound — it never hides a real fact — so
//! grounding stays correct while doing far less work.

/// A built XOR filter over a fixed key set.
///
/// Construction is fallible only in the sense that peeling may need a few seed
/// retries; [`XorFilter::build`] handles that internally and always returns a
/// filter for any finite key set.
#[derive(Debug, Clone)]
pub struct XorFilter {
    seed: u64,
    /// Size of each of the three segments (regions `a`, `b`, `c`).
    block_length: u32,
    /// `3 * block_length` fingerprints; region `k` starts at `k * block_length`.
    fingerprints: Vec<u8>,
    /// Number of keys the filter was built over. An empty filter reports
    /// membership for nothing (a zeroed fingerprint table would otherwise
    /// false-positive whenever a key's fingerprint hashes to 0).
    size: usize,
}

/// splitmix64 finalizer — a strong integer mix used for both the per-key hash
/// and fingerprint derivation.
#[inline]
fn mix(mut z: u64) -> u64 {
    z = (z ^ (z >> 30)).wrapping_mul(0xbf58_476d_1ce4_e5b9);
    z = (z ^ (z >> 27)).wrapping_mul(0x94d0_49bb_1331_11eb);
    z ^ (z >> 31)
}

/// Lemire's fastrange: map a 32-bit value uniformly into `[0, n)` without a
/// division.
#[inline]
fn reduce(x: u32, n: u32) -> u32 {
    ((u64::from(x) * u64::from(n)) >> 32) as u32
}

impl XorFilter {
    /// The three region indices and the 8-bit fingerprint for a key hash.
    #[inline]
    fn slots(&self, hash: u64) -> (usize, usize, usize, u8) {
        let bl = self.block_length;
        let r0 = hash as u32;
        let r1 = hash.rotate_left(21) as u32;
        let r2 = hash.rotate_left(42) as u32;
        let h0 = reduce(r0, bl);
        let h1 = reduce(r1, bl) + bl;
        let h2 = reduce(r2, bl) + 2 * bl;
        let fp = ((hash ^ (hash >> 32)) & 0xff) as u8;
        (h0 as usize, h1 as usize, h2 as usize, fp)
    }

    #[inline]
    fn hash(seed: u64, key: u64) -> u64 {
        mix(key.wrapping_add(seed))
    }

    /// Build a filter over `keys`. Duplicate keys are tolerated (deduplicated
    /// internally). An empty key set yields a filter whose [`contains`] is
    /// always `false`.
    ///
    /// [`contains`]: Self::contains
    #[must_use]
    pub fn build(keys: &[u64]) -> Self {
        let mut deduped: Vec<u64> = keys.to_vec();
        deduped.sort_unstable();
        deduped.dedup();
        let size = deduped.len();

        if size == 0 {
            return Self {
                seed: 0,
                block_length: 1,
                fingerprints: vec![0u8; 3],
                size: 0,
            };
        }

        // capacity ≈ 1.23·n + 32, split into three equal segments.
        let cap = 32 + (1.23 * size as f64).ceil() as u32;
        let block_length = cap / 3 + 1;
        let array_len = (block_length as usize) * 3;

        // Try successive seeds until peeling fully succeeds.
        let mut seed: u64 = 0x726f_7370_616e_5f78; // "rospan_x"
        loop {
            let mut this = Self {
                seed,
                block_length,
                fingerprints: vec![0u8; array_len],
                size,
            };
            if this.try_construct(&deduped) {
                return this;
            }
            seed = mix(seed.wrapping_add(1));
        }
    }

    /// One peeling attempt with the current `self.seed`. Returns `true` and
    /// fills `self.fingerprints` on success; leaves `self` reusable on failure.
    fn try_construct(&mut self, keys: &[u64]) -> bool {
        let array_len = self.fingerprints.len();
        // Per-slot: XOR of all key-hashes touching it, and how many touch it.
        let mut mask = vec![0u64; array_len];
        let mut count = vec![0u32; array_len];

        for &key in keys {
            let h = Self::hash(self.seed, key);
            let (a, b, c, _) = self.slots(h);
            mask[a] ^= h;
            count[a] += 1;
            mask[b] ^= h;
            count[b] += 1;
            mask[c] ^= h;
            count[c] += 1;
        }

        // Peel: repeatedly remove a slot touched by exactly one key.
        let mut queue: Vec<usize> = (0..array_len).filter(|&i| count[i] == 1).collect();
        let mut stack: Vec<(u64, usize)> = Vec::with_capacity(keys.len());

        while let Some(slot) = queue.pop() {
            if count[slot] != 1 {
                continue; // stale entry — slot changed after being queued.
            }
            let h = mask[slot];
            stack.push((h, slot));
            let (a, b, c, _) = self.slots(h);
            for t in [a, b, c] {
                mask[t] ^= h;
                count[t] -= 1;
                if count[t] == 1 {
                    queue.push(t);
                }
            }
        }

        if stack.len() != keys.len() {
            return false; // peeling stalled — caller retries with a new seed.
        }

        // Assign fingerprints in reverse peel order so each key's "free" slot
        // is set from the (already-assigned) other two.
        for &(h, slot) in stack.iter().rev() {
            let (a, b, c, fp) = self.slots(h);
            let mut val = fp;
            for t in [a, b, c] {
                if t != slot {
                    val ^= self.fingerprints[t];
                }
            }
            self.fingerprints[slot] = val;
        }
        true
    }

    /// Approximate membership test. Returns `true` for every key in the built
    /// set (**no false negatives**) and `true` for a small fraction of
    /// non-members (false positives, ~0.4%).
    #[must_use]
    pub fn contains(&self, key: u64) -> bool {
        if self.size == 0 {
            return false;
        }
        let h = Self::hash(self.seed, key);
        let (a, b, c, fp) = self.slots(h);
        fp == (self.fingerprints[a] ^ self.fingerprints[b] ^ self.fingerprints[c])
    }

    /// Fingerprint-array footprint in bytes — the whole filter minus a
    /// constant header. Useful for reporting compactness.
    ///
    /// Pre-existing dead-code under `make check`/`make clippy`'s
    /// `RUSTFLAGS=-D warnings` (Makefile.toml's `[env]` section): `XorFilter`
    /// is a private, un-re-exported type (`mod xorf;` not `pub mod xorf;`
    /// in `ground/mod.rs`) and nothing calls this yet outside a doc comment
    /// claim. Kept as real public-API surface, not removed.
    #[must_use]
    #[allow(dead_code)]
    pub fn byte_len(&self) -> usize {
        self.fingerprints.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn no_false_negatives() {
        let keys: Vec<u64> = (0..5000u64)
            .map(|i| i.wrapping_mul(0x9E37_79B9_7F4A_7C15))
            .collect();
        let f = XorFilter::build(&keys);
        for &k in &keys {
            assert!(f.contains(k), "member {k} must be reported present");
        }
    }

    #[test]
    fn low_false_positive_rate() {
        let keys: Vec<u64> = (0..2000u64).collect();
        let f = XorFilter::build(&keys);
        // Probe 100k values known not to be members.
        let mut fp = 0usize;
        let trials = 100_000u64;
        for i in 0..trials {
            let probe = 1_000_000 + i;
            if f.contains(probe) {
                fp += 1;
            }
        }
        let rate = fp as f64 / trials as f64;
        assert!(
            rate < 0.02,
            "false-positive rate {rate} too high (expected ~0.004)"
        );
    }

    #[test]
    fn empty_filter_contains_nothing() {
        let f = XorFilter::build(&[]);
        assert!(!f.contains(0));
        assert!(!f.contains(12345));
    }

    #[test]
    fn tolerates_duplicate_keys() {
        let keys = vec![7u64, 7, 7, 42, 42, 100];
        let f = XorFilter::build(&keys);
        for k in [7u64, 42, 100] {
            assert!(f.contains(k));
        }
    }
}
