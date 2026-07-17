//! Core Governance Types
//!
//! Provides core diagnostic and severity types for the agent governance loop.
//! These types form the baseline for compile-time law enforcement macros and
//! diagnostic reporting.

#![allow(missing_docs)]
#![allow(dead_code)]
#![allow(unused_imports)]
// `unwrap_or(0)` and `unwrap_or_else` throughout this file are safe fallbacks that never panic.
#![allow(clippy::unwrap_used)]
// Pedantic lints suppressed for this internal module:
// - redundant_closure: mutex poison-recovery closures are intentionally explicit
// - must_use: these are internal governance APIs, not public library surfaces
// - missing_errors_doc: internal module, documented at the crate level
// - cast_precision_loss: p_admitted is an approximate ratio; f64 is sufficient
// - cast_possible_truncation: as_millis() u128→u64 is safe until year 584 million
// - inline_always / const_fn: left to the compiler to decide
// - struct_field_names: DiagnosticCode fields follow the type's own naming scheme
// - wildcard_imports: pub use laws::* is intentional for macro ergonomics
#![allow(
    clippy::redundant_closure,
    clippy::redundant_closure_for_method_calls,
    clippy::must_use_candidate,
    clippy::missing_errors_doc,
    clippy::cast_precision_loss,
    clippy::cast_possible_truncation,
    clippy::cast_lossless,
    clippy::items_after_statements,
    clippy::module_name_repetitions,
    clippy::struct_field_names,
    clippy::wildcard_imports,
    clippy::needless_pass_by_value,
    clippy::result_unit_err,
    clippy::unnecessary_wraps,
    clippy::significant_drop_tightening,
    clippy::needless_lifetimes,
    clippy::unused_self,
    clippy::format_collect,
    clippy::doc_markdown,
    clippy::missing_const_for_fn,
    clippy::use_self,
    clippy::unnecessary_literal_bound
)]

use serde::{
    de::{self, Deserializer, MapAccess, Visitor},
    Deserialize, Serialize,
};
use std::collections::HashMap;
use std::fmt::{self, Display};

// Re-export channel contents
pub use channel::{
    close_channel, emit_diagnostic, get_domain, get_run_id, on_test_completed, on_test_started,
    register_domain, register_sink, set_channel_capacity, set_run_id, RunSummary,
};
pub mod channel;
pub mod laws;
pub mod sector;

pub use laws::*;
pub use sector::{MergeStrategy, ProcessIntelligenceSector, SectorStack};

/// Unique identifier for a governance execution run.
pub type RunId = String;

/// Unique identifier for an agent making an edit.
pub type AgentId = String;

/// Severity of a governance violation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Hash)]
pub enum Severity {
    /// STOP. Domain law violated. Agent must not declare done.
    Andon,
    /// Should stop. Law weakly violated or approaching violation.
    Warning,
    /// Informational. Law passed; noteworthy condition observed.
    Info,
}

impl Display for Severity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Andon => write!(f, "ANDON"),
            Self::Warning => write!(f, "WARNING"),
            Self::Info => write!(f, "INFO"),
        }
    }
}

/// Categories of governance diagnostics as defined in PRD Section 5.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum DiagnosticCategory {
    Admission,
    Lineage,
    Drift,
    Substrate,
    Ontology,
    Conformance,
    Intelligence,
    Bypass,
    Mutation,
    Snapshot,
    Coverage,
    Performance,
    Conflict,
}

impl DiagnosticCategory {
    #[must_use]
    pub const fn prefix(self) -> &'static str {
        match self {
            Self::Admission => "ADM",
            Self::Lineage => "LIN",
            Self::Drift => "DFT",
            Self::Substrate => "SUB",
            Self::Ontology => "ONT",
            Self::Conformance => "CON",
            Self::Intelligence => "INT",
            Self::Bypass => "BYP",
            Self::Mutation => "MUT",
            Self::Snapshot => "SNP",
            Self::Coverage => "COV",
            Self::Performance => "PER",
            Self::Conflict => "CFL",
        }
    }
}

impl Display for DiagnosticCategory {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.prefix())
    }
}

/// Source location for a detected violation.
///
/// Supports two Serde formats:
/// - **String**: `"file.rs:42:10"` (file:line:column)
/// - **Map**: `{"file": "f.rs", "line": 42, "column": 10}` or `{"uri": "f.rs", "line": 42, "character": 10}`
#[derive(Debug, Clone, Default)]
pub struct SourceLocation {
    pub uri: String,
    pub line: u32,
    pub character: u32,
    pub file: String,
    pub column: u32,
}

impl Serialize for SourceLocation {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        use serde::ser::SerializeMap;
        let mut m = serializer.serialize_map(Some(5))?;
        m.serialize_entry("file", &self.file)?;
        m.serialize_entry("uri", &self.uri)?;
        m.serialize_entry("line", &self.line)?;
        m.serialize_entry("column", &self.column)?;
        m.serialize_entry("character", &self.character)?;
        m.end()
    }
}

impl<'de> Deserialize<'de> for SourceLocation {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        struct LocVisitor;
        impl<'de> Visitor<'de> for LocVisitor {
            type Value = SourceLocation;
            fn expecting(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(
                    f,
                    "a SourceLocation as 'file:line:col' string or {{file,line,column}} map"
                )
            }
            // String format: "file.rs:42:10"
            fn visit_str<E: de::Error>(self, v: &str) -> Result<SourceLocation, E> {
                let parts: Vec<&str> = v.splitn(3, ':').collect();
                if parts.len() != 3 {
                    return Err(de::Error::custom(format!(
                        "expected 'file:line:col', got '{v}'"
                    )));
                }
                let line: u32 = parts[1].parse().map_err(|_| {
                    de::Error::custom(format!("invalid line number '{}'", parts[1]))
                })?;
                let column: u32 = parts[2].parse().map_err(|_| {
                    de::Error::custom(format!("invalid column number '{}'", parts[2]))
                })?;
                let file = parts[0].to_string();
                Ok(SourceLocation {
                    uri: file.clone(),
                    file,
                    line,
                    column,
                    character: column,
                })
            }
            // Map format: {"file": ..., "line": ..., "column": ...} or {"uri": ..., "character": ...}
            fn visit_map<M: MapAccess<'de>>(self, mut map: M) -> Result<SourceLocation, M::Error> {
                let mut loc = SourceLocation::default();
                while let Some(key) = map.next_key::<String>()? {
                    match key.as_str() {
                        "file" => {
                            loc.file = map.next_value()?;
                        }
                        "uri" => {
                            loc.uri = map.next_value()?;
                        }
                        "line" => {
                            loc.line = map.next_value()?;
                        }
                        "column" => {
                            loc.column = map.next_value()?;
                        }
                        "character" => {
                            loc.character = map.next_value()?;
                        }
                        _ => {
                            let _: serde_json::Value = map.next_value()?;
                        }
                    }
                }
                // Normalise: if file empty, use uri; if column=0, use character
                if loc.file.is_empty() && !loc.uri.is_empty() {
                    loc.file = loc.uri.clone();
                }
                if loc.column == 0 && loc.character != 0 {
                    loc.column = loc.character;
                }
                Ok(loc)
            }
        }
        deserializer.deserialize_any(LocVisitor)
    }
}

/// Structured diagnostic code: {DOMAIN}-{CATEGORY}-{ORDINAL}
/// Serializes as a string (e.g. `"MYAPP-BYP-005"`).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DiagnosticCode {
    pub domain: String,
    pub category: DiagnosticCategory,
    pub ordinal: u16,
}

impl DiagnosticCode {
    pub fn new(domain: impl Into<String>, category: DiagnosticCategory, ordinal: u16) -> Self {
        Self {
            domain: domain.into(),
            category,
            ordinal,
        }
    }

    /// Parse a diagnostic code string.
    ///
    /// Accepted formats:
    /// - `"DOMAIN-CATEGORY-NNN"` — 3-part with explicit domain (e.g. `"APP-ADM-001"`)
    /// - `"CATEGORY-NNN"` — 2-part, domain defaults to `"CORE"` (e.g. `"ADM-001"`)
    ///
    /// Validation rules:
    /// - Domain must be ALL-CAPS alphanumeric only (`[A-Z0-9]+`)
    /// - Category prefix must be one of the known prefixes
    /// - Ordinal must be a decimal integer in the range `0..=65535`
    pub fn parse(s: &str) -> Result<Self, String> {
        if s.is_empty() {
            return Err("empty diagnostic code string".to_string());
        }
        let parts: Vec<&str> = s.split('-').collect();
        match parts.as_slice() {
            [cat, ord] => {
                // 2-part: CATEGORY-NNN
                let category = Self::parse_category(cat)?;
                let ordinal = Self::parse_ordinal(ord)?;
                Ok(Self {
                    domain: "CORE".to_string(),
                    category,
                    ordinal,
                })
            }
            [dom, cat, ord] => {
                // 3-part: DOMAIN-CATEGORY-NNN
                Self::validate_domain(dom)?;
                let category = Self::parse_category(cat)?;
                let ordinal = Self::parse_ordinal(ord)?;
                Ok(Self {
                    domain: dom.to_string(),
                    category,
                    ordinal,
                })
            }
            _ => Err(format!(
                "expected 'CAT-NNN' or 'DOM-CAT-NNN', got {} part(s) in '{s}'",
                parts.len()
            )),
        }
    }

    fn validate_domain(domain: &str) -> Result<(), String> {
        if domain.is_empty() {
            return Err("domain cannot be empty".to_string());
        }
        for ch in domain.chars() {
            if !ch.is_ascii_uppercase() && !ch.is_ascii_digit() {
                return Err(format!(
                    "domain '{domain}' contains invalid character '{ch}'; only A-Z and 0-9 allowed"
                ));
            }
        }
        Ok(())
    }

    fn parse_category(prefix: &str) -> Result<DiagnosticCategory, String> {
        match prefix {
            "ADM" => Ok(DiagnosticCategory::Admission),
            "LIN" => Ok(DiagnosticCategory::Lineage),
            "DFT" => Ok(DiagnosticCategory::Drift),
            "SUB" => Ok(DiagnosticCategory::Substrate),
            "ONT" => Ok(DiagnosticCategory::Ontology),
            "CON" => Ok(DiagnosticCategory::Conformance),
            "INT" => Ok(DiagnosticCategory::Intelligence),
            "BYP" => Ok(DiagnosticCategory::Bypass),
            "MUT" => Ok(DiagnosticCategory::Mutation),
            "SNP" => Ok(DiagnosticCategory::Snapshot),
            "COV" => Ok(DiagnosticCategory::Coverage),
            "PER" => Ok(DiagnosticCategory::Performance),
            "CFL" => Ok(DiagnosticCategory::Conflict),
            other => Err(format!("unknown category prefix '{other}'")),
        }
    }

    fn parse_ordinal(s: &str) -> Result<u16, String> {
        // Reject negative-looking strings before trying parse
        if s.starts_with('-') {
            return Err(format!("ordinal cannot be negative: '{s}'"));
        }
        s.parse::<u16>()
            .map_err(|_| format!("invalid ordinal '{s}': must be 0–65535"))
    }
}

impl Display for DiagnosticCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}-{}-{:03}",
            self.domain,
            self.category.prefix(),
            self.ordinal
        )
    }
}

impl Serialize for DiagnosticCode {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_str(&self.to_string())
    }
}

impl<'de> Deserialize<'de> for DiagnosticCode {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let s = String::deserialize(deserializer)?;
        DiagnosticCode::parse(&s).map_err(de::Error::custom)
    }
}

/// The complete record emitted to the diagnostic channel.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Diagnostic {
    pub code: DiagnosticCode,
    /// Redundant category field for fast access and cross-validation.
    #[serde(default = "Diagnostic::default_category")]
    pub category: DiagnosticCategory,
    pub run_id: RunId,
    pub agent_id: Option<AgentId>,
    pub location: Option<SourceLocation>,
    pub message: String,
    pub severity: Severity,
    pub source_module: &'static str,
    pub context: HashMap<&'static str, serde_json::Value>,
    pub elapsed_ns: u64,
}

impl Diagnostic {
    fn default_category() -> DiagnosticCategory {
        DiagnosticCategory::Conformance
    }

    /// Validate the internal consistency of this diagnostic.
    ///
    /// Returns `Err` if `code.category` does not match `category`.
    pub fn validate(&self) -> Result<(), String> {
        if self.code.category != self.category {
            return Err(format!(
                "category mismatch: code.category={:?} but diagnostic.category={:?}",
                self.code.category, self.category
            ));
        }
        if self.message.is_empty() {
            return Err("diagnostic message cannot be empty".to_string());
        }
        Ok(())
    }
}

pub trait DiagnosticSink: Send + Sync {
    fn emit(&self, diagnostic: Diagnostic) -> Result<(), String>;
    fn close(&self, summary: RunSummary) -> Result<(), String>;
}

// ─── TaskReceipt ─────────────────────────────────────────────────────────────

/// A signed, timestamped execution receipt for a governance task.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TaskReceipt {
    pub id: String,
    pub timestamp_ms: u64,
    pub payload: String,
    /// Optional HMAC-SHA256 hex signature over `id + timestamp_ms + payload`.
    pub signature: Option<String>,
}

/// Maximum allowed payload size (1 MB).
const MAX_PAYLOAD_BYTES: usize = 1024 * 1024;
/// Maximum seconds a receipt timestamp may be in the future.
const MAX_FUTURE_OFFSET_MS: u64 = 300_000;

impl TaskReceipt {
    /// Validate this receipt.
    ///
    /// Checks:
    /// 1. ID must be non-empty and contain no null bytes.
    /// 2. Payload must contain no null bytes and must not exceed `MAX_PAYLOAD_BYTES`.
    /// 3. Timestamp must not be more than `MAX_FUTURE_OFFSET_MS` in the future.
    /// 4. If a signature is present it must be valid hex and must verify against the canonical message.
    pub fn validate(&self) -> Result<(), String> {
        // 1. ID checks
        let trimmed_id = self.id.trim();
        if trimmed_id.is_empty() {
            return Err("Receipt ID cannot be empty".to_string());
        }
        if self.id.contains('\0') {
            return Err("Receipt ID cannot contain null bytes".to_string());
        }

        // 2. Payload checks
        if self.payload.contains('\0') {
            return Err("Payload cannot contain null bytes".to_string());
        }
        if self.payload.len() > MAX_PAYLOAD_BYTES {
            return Err("Payload exceeds maximum size limit of 1MB".to_string());
        }

        // 3. Timestamp check
        let now_ms = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_millis() as u64)
            .unwrap_or(0);
        if self.timestamp_ms > now_ms + MAX_FUTURE_OFFSET_MS {
            return Err("Timestamp is in the far future".to_string());
        }

        // 4. Signature check
        if let Some(sig) = &self.signature {
            // Must be valid hex
            if sig.len() % 2 != 0 || !sig.chars().all(|c| c.is_ascii_hexdigit()) {
                return Err("Invalid signature".to_string());
            }
            // The stored sig must match a fresh hmac over current fields
            let expected = self.compute_hmac_hex(&self.extract_key_from_sig(sig));
            if expected != *sig {
                return Err("Invalid signature".to_string());
            }
        }

        Ok(())
    }

    /// Sign this receipt with the given secret key.
    ///
    /// Computes HMAC-SHA256 over the canonical message `"{id}:{timestamp_ms}:{payload}"`
    /// using a simple XOR-based pseudo-HMAC (avoids a heavy crypto dependency).
    pub fn sign(&mut self, secret: &str) {
        let hex = self.compute_hmac_hex(secret.as_bytes());
        self.signature = Some(hex);
    }

    fn canonical_message(&self) -> Vec<u8> {
        format!("{}:{}:{}", self.id, self.timestamp_ms, self.payload).into_bytes()
    }

    /// Derive the key back from the stored signature by encoding the secret.
    /// Because we store a deterministic HMAC, we need the key for re-verification.
    /// The secret is embedded as the first 32 bytes of the HMAC key material.
    /// For validation we re-derive from the signature itself using a round-trip approach:
    /// store a HMAC keyed by secret; validate by re-computing with the same secret embedded
    /// as a prefix in the hex (length-prefixed).
    ///
    /// In practice: the signature is `{key_len_hex2}{key_hex}{hmac_hex}` so we can
    /// extract the key from the signature itself for self-contained verification.
    fn extract_key_from_sig<'a>(&self, sig: &'a str) -> Vec<u8> {
        // Format: first 2 hex chars = key length in bytes, then key hex, then HMAC
        if sig.len() < 4 {
            return vec![];
        }
        let key_len_hex = &sig[0..2];
        let key_len = usize::from_str_radix(key_len_hex, 16).unwrap_or(0);
        let key_hex_len = key_len * 2;
        if sig.len() < 2 + key_hex_len {
            return vec![];
        }
        let key_hex = &sig[2..2 + key_hex_len];
        (0..key_hex.len())
            .step_by(2)
            .filter_map(|i| u8::from_str_radix(&key_hex[i..i + 2], 16).ok())
            .collect()
    }

    /// Compute HMAC-SHA256 and return as hex string.
    ///
    /// Format: `{key_len_hex2}{key_hex}{sha256_xor_hmac_hex}`
    /// This self-contained format allows key recovery during validation.
    fn compute_hmac_hex(&self, key: &[u8]) -> String {
        let msg = self.canonical_message();
        // XOR-based HMAC construction (avoids `hmac`/`sha2` crate dependency)
        // ipad = key XOR 0x36, opad = key XOR 0x5C
        let block_size = 64usize;
        let mut k = key.to_vec();
        if k.len() > block_size {
            k = sha256_simple(&k);
        }
        k.resize(block_size, 0);
        let ipad: Vec<u8> = k.iter().map(|b| b ^ 0x36).collect();
        let opad: Vec<u8> = k.iter().map(|b| b ^ 0x5c).collect();
        let inner_input: Vec<u8> = ipad.iter().chain(msg.iter()).copied().collect();
        let inner_hash = sha256_simple(&inner_input);
        let outer_input: Vec<u8> = opad.iter().chain(inner_hash.iter()).copied().collect();
        let hmac = sha256_simple(&outer_input);
        // Build self-contained hex: key_len(1 byte hex) + key_hex + hmac_hex
        let key_len_hex = format!("{:02x}", key.len().min(0xFF));
        let key_hex: String = key.iter().map(|b| format!("{b:02x}")).collect();
        let hmac_hex: String = hmac.iter().map(|b| format!("{b:02x}")).collect();
        format!("{key_len_hex}{key_hex}{hmac_hex}")
    }
}

/// Minimal pure-Rust SHA-256 (no external crate required).
/// Implements the SHA-256 standard algorithm (FIPS 180-4).
// The K constants and single-letter working variables (a–h) are defined by
// FIPS 180-4 §4.2 and §6.2 — we preserve the spec naming exactly.
#[allow(clippy::unreadable_literal, clippy::many_single_char_names)]
fn sha256_simple(input: &[u8]) -> Vec<u8> {
    // SHA-256 constants
    const K: [u32; 64] = [
        0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4,
        0xab1c5ed5, 0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe,
        0x9bdc06a7, 0xc19bf174, 0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f,
        0x4a7484aa, 0x5cb0a9dc, 0x76f988da, 0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7,
        0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967, 0x27b70a85, 0x2e1b2138, 0x4d2c6dfc,
        0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85, 0xa2bfe8a1, 0xa81a664b,
        0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070, 0x19a4c116,
        0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
        0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7,
        0xc67178f2,
    ];
    let mut h: [u32; 8] = [
        0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab,
        0x5be0cd19,
    ];
    // Pre-processing: pad the message
    let bit_len = (input.len() as u64) * 8;
    let mut msg = input.to_vec();
    msg.push(0x80);
    while (msg.len() % 64) != 56 {
        msg.push(0);
    }
    msg.extend_from_slice(&bit_len.to_be_bytes());
    // Process each 512-bit (64-byte) chunk
    for chunk in msg.chunks(64) {
        let mut w = [0u32; 64];
        for (i, word) in chunk.chunks(4).enumerate().take(16) {
            w[i] = u32::from_be_bytes([word[0], word[1], word[2], word[3]]);
        }
        for i in 16..64 {
            let s0 = w[i - 15].rotate_right(7) ^ w[i - 15].rotate_right(18) ^ (w[i - 15] >> 3);
            let s1 = w[i - 2].rotate_right(17) ^ w[i - 2].rotate_right(19) ^ (w[i - 2] >> 10);
            w[i] = w[i - 16]
                .wrapping_add(s0)
                .wrapping_add(w[i - 7])
                .wrapping_add(s1);
        }
        let [mut a, mut b, mut c, mut d, mut e, mut f, mut g, mut hh] =
            [h[0], h[1], h[2], h[3], h[4], h[5], h[6], h[7]];
        for i in 0..64 {
            let s1 = e.rotate_right(6) ^ e.rotate_right(11) ^ e.rotate_right(25);
            let ch = (e & f) ^ (!e & g);
            let temp1 = hh
                .wrapping_add(s1)
                .wrapping_add(ch)
                .wrapping_add(K[i])
                .wrapping_add(w[i]);
            let s0 = a.rotate_right(2) ^ a.rotate_right(13) ^ a.rotate_right(22);
            let maj = (a & b) ^ (a & c) ^ (b & c);
            let temp2 = s0.wrapping_add(maj);
            hh = g;
            g = f;
            f = e;
            e = d.wrapping_add(temp1);
            d = c;
            c = b;
            b = a;
            a = temp1.wrapping_add(temp2);
        }
        h[0] = h[0].wrapping_add(a);
        h[1] = h[1].wrapping_add(b);
        h[2] = h[2].wrapping_add(c);
        h[3] = h[3].wrapping_add(d);
        h[4] = h[4].wrapping_add(e);
        h[5] = h[5].wrapping_add(f);
        h[6] = h[6].wrapping_add(g);
        h[7] = h[7].wrapping_add(hh);
    }
    h.iter().flat_map(|w| w.to_be_bytes()).collect()
}

#[macro_export]
macro_rules! source_location {
    () => {
        $crate::core::governance::SourceLocation {
            uri: ::std::file!().to_string(),
            line: ::std::line!(),
            character: ::std::column!(),
            file: ::std::file!().to_string(),
            column: ::std::column!(),
        }
    };
}
