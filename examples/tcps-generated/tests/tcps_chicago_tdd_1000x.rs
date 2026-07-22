//! TCPS product-evidence fabric.
//!
//! The previous implementation generated a Cartesian product of every capability,
//! surface, and law, then discharged every claim by proving only that one file was
//! readable. That was a vacuous proof. This replacement admits only evidence that
//! is actually observed:
//!
//! - each capability artifact must parse according to its real format;
//! - each mapped artifact must match its product SHA-256 manifest entry;
//! - the product manifest must contain 129 unique, well-formed entries;
//! - the ggen lock must name the complete eight-pack composition with valid BLAKE3
//!   identities;
//! - the receipt root binds the validated evidence rather than a rhetorical count.

mod support;

use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::OnceLock;

use chicago_tdd_tools::core::contract::{
    ResourceEnvelope, TestContract, TestContractRegistry,
};
use serde::Serialize;
use serde_json::Value as JsonValue;
use support::{
    domain_digest, git_blob_oid, merkle_root, read_nonempty, serialize_json, sha256_hex, utf8,
    EvidenceDigest, EvidenceError,
};

include!("product_evidence/catalog.rs");
include!("product_evidence/manifest.rs");
include!("product_evidence/validators.rs");
include!("product_evidence/evidence.rs");
include!("product_evidence/tests.rs");
