//! Genesis Core Primitives (Heap-free and no-std friendly)
//!
//! This module implements key primitives for the Genesis embedded core and ggen's outer membrane:
//! - [`Pair2`]: Bounded relationship tuple of two entities
//! - [`RelationPage`]: Fixed-size heap-free index of relationships
//! - [`Construct8`]: Bounded kinetic delta primitive (subject, predicate, object, graph, mask, provenance, admission, receipt_hint)
//! - [`Receipt`]: Cryptographically signed, heap-free execution receipt
//! - [`Replay`]: Deterministic execution replay log and engine
//! - [`Refusal`]: Concrete out-of-manifold execution refusal with observed boundary evidence

// No longer no_std at mod level

use core::fmt;

/// Size constants for heap-free layout boundaries
pub const MAX_RELATION_PAIRS: usize = 32;
pub const MAX_REPLAY_STEPS: usize = 16;
pub const IDENTIFIER_SIZE: usize = 8;
pub const HASH_SIZE: usize = 32;
pub const SIGNATURE_SIZE: usize = 64;

// --- Newtype wrappers around [u8; 8] for Construct8 ---

/// Bounded Subject Node Identifier (8 bytes)
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Node8(pub [u8; IDENTIFIER_SIZE]);

/// Bounded Predicate Identifier (8 bytes)
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Predicate8(pub [u8; IDENTIFIER_SIZE]);

/// Bounded Object Identifier (8 bytes)
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Object8(pub [u8; IDENTIFIER_SIZE]);

/// Bounded Graph/Partition Identifier (8 bytes)
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Graph8(pub [u8; IDENTIFIER_SIZE]);

/// Bounded Execution Mask / Capability (8 bytes)
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Mask8(pub [u8; IDENTIFIER_SIZE]);

/// Bounded Provenance Link (8 bytes)
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Provenance8(pub [u8; IDENTIFIER_SIZE]);

/// Bounded Admission Token / Checksum (8 bytes)
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Admission8(pub [u8; IDENTIFIER_SIZE]);

/// Bounded Cryptographic Receipt Hint / Salt (8 bytes)
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ReceiptHint8(pub [u8; IDENTIFIER_SIZE]);

// Implementations of helper display and creation methods for Node8 etc.
macro_rules! impl_newtype_helpers {
    ($t:ident, $name:expr) => {
        impl $t {
            /// Create from a literal slice
            pub const fn from_bytes(bytes: [u8; IDENTIFIER_SIZE]) -> Self {
                Self(bytes)
            }

            /// Expose the underlying byte representation
            pub const fn as_bytes(&self) -> &[u8; IDENTIFIER_SIZE] {
                &self.0
            }
        }

        impl fmt::Debug for $t {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{}(hex: ", $name)?;
                for byte in &self.0 {
                    write!(f, "{:02x}", byte)?;
                }
                write!(f, ")")
            }
        }
    };
}

impl_newtype_helpers!(Node8, "Node8");
impl_newtype_helpers!(Predicate8, "Predicate8");
impl_newtype_helpers!(Object8, "Object8");
impl_newtype_helpers!(Graph8, "Graph8");
impl_newtype_helpers!(Mask8, "Mask8");
impl_newtype_helpers!(Provenance8, "Provenance8");
impl_newtype_helpers!(Admission8, "Admission8");
impl_newtype_helpers!(ReceiptHint8, "ReceiptHint8");

// --- Pair2 & RelationPage ---

/// Pair2: A bounded semantic binary relationship (e.g. subject-object connection)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Pair2 {
    /// Source / Subject entity
    pub subject: Node8,
    /// Destination / Object entity
    pub object: Node8,
}

impl Pair2 {
    /// Create a new semantic connection
    pub const fn new(subject: Node8, object: Node8) -> Self {
        Self { subject, object }
    }
}

/// RelationPage: A heap-free, fixed-size container representing a page/index of relationships
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RelationPage {
    /// Contained relationship pairs
    pub pairs: [Option<Pair2>; MAX_RELATION_PAIRS],
    /// Current count of active pairs stored
    pub length: usize,
}

impl RelationPage {
    /// Initialize an empty relation page
    pub const fn new() -> Self {
        Self {
            pairs: [None; MAX_RELATION_PAIRS],
            length: 0,
        }
    }

    /// Insert a new relationship pair. Returns `true` if inserted, `false` if full or duplicate.
    pub fn insert(&mut self, pair: Pair2) -> bool {
        // Check for duplicates
        for i in 0..MAX_RELATION_PAIRS {
            if let Some(ref p) = self.pairs[i] {
                if p == &pair {
                    return false; // Already present
                }
            }
        }

        // Find an empty slot
        for i in 0..MAX_RELATION_PAIRS {
            if self.pairs[i].is_none() {
                self.pairs[i] = Some(pair);
                self.length += 1;
                return true;
            }
        }
        false // Page is full
    }

    /// Check if a pair exists in the page
    pub fn contains(&self, pair: &Pair2) -> bool {
        for i in 0..MAX_RELATION_PAIRS {
            if let Some(ref p) = self.pairs[i] {
                if p == pair {
                    return true;
                }
            }
        }
        false
    }

    /// Remove a relationship pair. Returns `true` if found and removed.
    pub fn remove(&mut self, pair: &Pair2) -> bool {
        for i in 0..MAX_RELATION_PAIRS {
            if let Some(ref p) = self.pairs[i] {
                if p == pair {
                    self.pairs[i] = None;
                    self.length -= 1;
                    return true;
                }
            }
        }
        false
    }
}

// --- Construct8 ---

/// Construct8: Bounded constructive state projection/delta primitive.
/// It defines a kinetic delta that moves between graph truth and execution.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Construct8 {
    /// Bounded subject
    pub subject: Node8,
    /// Bounded predicate
    pub predicate: Predicate8,
    /// Bounded object
    pub object: Object8,
    /// Bounded graph target
    pub graph: Graph8,
    /// Bounded capability mask
    pub mask: Mask8,
    /// Provenance tag
    pub provenance: Provenance8,
    /// Admission validator token
    pub admission: Admission8,
    /// Cryptographic receipt verification hint
    pub receipt_hint: ReceiptHint8,
}

impl Construct8 {
    /// Create a new Construct8 delta primitive
    #[allow(clippy::too_many_arguments)]
    pub const fn new(
        subject: Node8,
        predicate: Predicate8,
        object: Object8,
        graph: Graph8,
        mask: Mask8,
        provenance: Provenance8,
        admission: Admission8,
        receipt_hint: ReceiptHint8,
    ) -> Self {
        Self {
            subject,
            predicate,
            object,
            graph,
            mask,
            provenance,
            admission,
            receipt_hint,
        }
    }

    /// Serialize structural representation to bytes for hashing
    pub fn to_bytes(&self) -> [u8; 64] {
        let mut out = [0u8; 64];
        out[0..8].copy_from_slice(self.subject.as_bytes());
        out[8..16].copy_from_slice(self.predicate.as_bytes());
        out[16..24].copy_from_slice(self.object.as_bytes());
        out[24..32].copy_from_slice(self.graph.as_bytes());
        out[32..40].copy_from_slice(self.mask.as_bytes());
        out[40..48].copy_from_slice(self.provenance.as_bytes());
        out[48..56].copy_from_slice(self.admission.as_bytes());
        out[56..64].copy_from_slice(self.receipt_hint.as_bytes());
        out
    }
}

// --- Receipt ---

/// Receipt: Heap-free cryptographic execution receipt, containing observed boundary evidence.
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Receipt {
    /// Operation identity hash/checksum
    pub operation_id: [u8; 16],
    /// Cryptographic hash of observed boundary inputs
    pub inputs_hash: [u8; HASH_SIZE],
    /// Cryptographic hash of produced boundary outputs
    pub outputs_hash: [u8; HASH_SIZE],
    /// Optional parent receipt hash for causal linking
    pub previous_receipt_hash: Option<[u8; HASH_SIZE]>,
    /// Digital signature of receipt contents
    pub signature: Option<[u8; SIGNATURE_SIZE]>,
    /// Verizable public key of the signer
    pub public_key: Option<[u8; 32]>,
    /// Deterministic epoch timestamp
    pub timestamp: u64,
}

impl fmt::Debug for Receipt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Receipt")
            .field("operation_id", &self.operation_id)
            .field("inputs_hash", &HexSlice(&self.inputs_hash))
            .field("outputs_hash", &HexSlice(&self.outputs_hash))
            .field("previous_receipt_hash", &self.previous_receipt_hash.as_ref().map(|h| HexSlice(h)))
            .field("signature", &self.signature.as_ref().map(|s| HexSlice64(s)))
            .field("public_key", &self.public_key.as_ref().map(|pk| HexSlice(pk)))
            .field("timestamp", &self.timestamp)
            .finish()
    }
}

struct HexSlice<'a>(&'a [u8; HASH_SIZE]);
impl<'a> fmt::Debug for HexSlice<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for byte in self.0 {
            write!(f, "{:02x}", byte)?;
        }
        Ok(())
    }
}

struct HexSlice64<'a>(&'a [u8; SIGNATURE_SIZE]);
impl<'a> fmt::Debug for HexSlice64<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for byte in self.0 {
            write!(f, "{:02x}", byte)?;
        }
        Ok(())
    }
}

impl Receipt {
    /// Create a new, unsigned Receipt with actual observed hashes
    pub const fn new(
        operation_id: [u8; 16],
        inputs_hash: [u8; HASH_SIZE],
        outputs_hash: [u8; HASH_SIZE],
        previous_receipt_hash: Option<[u8; HASH_SIZE]>,
        timestamp: u64,
    ) -> Self {
        Self {
            operation_id,
            inputs_hash,
            outputs_hash,
            previous_receipt_hash,
            signature: None,
            public_key: None,
            timestamp,
        }
    }

    /// Calculate hash over receipt contents (excluding signature field)
    pub fn calculate_hash(&self) -> [u8; HASH_SIZE] {
        let mut hasher = blake3::Hasher::new();
        hasher.update(&self.operation_id);
        hasher.update(&self.inputs_hash);
        hasher.update(&self.outputs_hash);
        if let Some(prev) = self.previous_receipt_hash {
            hasher.update(&prev);
        } else {
            hasher.update(&[0u8; HASH_SIZE]);
        }
        hasher.update(&self.timestamp.to_le_bytes());
        *hasher.finalize().as_bytes()
    }

    /// Sign the receipt payload using Ed25519
    pub fn sign(mut self, signing_key: &ed25519_dalek::SigningKey) -> Self {
        use ed25519_dalek::Signer;
        let hash = self.calculate_hash();
        let sig = signing_key.sign(&hash);
        self.signature = Some(sig.to_bytes());
        self.public_key = Some(signing_key.verifying_key().to_bytes());
        self
    }

    /// Verify signature and structure authenticity
    pub fn verify(&self) -> Result<(), RefusalCode> {
        use ed25519_dalek::Verifier;
        let pk_bytes = self.public_key.ok_or(RefusalCode::BoundaryEvidenceMissing)?;
        let sig_bytes = self.signature.ok_or(RefusalCode::BoundaryEvidenceMissing)?;
        
        let verifying_key = ed25519_dalek::VerifyingKey::from_bytes(&pk_bytes)
            .map_err(|_| RefusalCode::InvalidSignature)?;
        let signature = ed25519_dalek::Signature::from_bytes(&sig_bytes);
        
        let hash = self.calculate_hash();
        verifying_key.verify(&hash, &signature)
            .map_err(|_| RefusalCode::InvalidSignature)
    }
}

// --- Refusal ---

/// RefusalReason classifying state refusal boundaries
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum RefusalCode {
    /// Missing vital boundary evidence/hash
    BoundaryEvidenceMissing = 1,
    /// Required OCEL-shaped tracing trace missing
    ExpectedOCELMissing = 2,
    /// Missing tool call cryptographic hash
    ToolCallHashMissing = 3,
    /// Invalid signature on execution/receipt gate
    InvalidSignature = 4,
    /// Inconsistent parent/child causal receipt links
    CausalInconsistency = 5,
    /// Delta/Mask constraint violation
    ConstraintViolation = 6,
}

/// Refusal: Concrete out-of-manifold execution refusal with observed boundary evidence
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Refusal {
    /// Specific failure code
    pub code: RefusalCode,
    /// Cryptographic hash of the operation being refused
    pub op_hash: [u8; HASH_SIZE],
    /// Actual observed evidence bytes up to the point of failure
    pub evidence: [u8; 64],
    /// Refusal occurrence timestamp
    pub timestamp: u64,
}

impl Refusal {
    /// Create a new refusal binding concrete evidence
    pub const fn new(
        code: RefusalCode,
        op_hash: [u8; HASH_SIZE],
        evidence: [u8; 64],
        timestamp: u64,
    ) -> Self {
        Self {
            code,
            op_hash,
            evidence,
            timestamp,
        }
    }
}

// --- Replay ---

/// Replay: Deterministic execution log structure
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Replay {
    /// Sequence of target Construct8 deltas
    pub steps: [Option<Construct8>; MAX_REPLAY_STEPS],
    /// Actual count of delta steps stored
    pub length: usize,
}

impl Replay {
    /// Create an empty replay log
    pub const fn new() -> Self {
        Self {
            steps: [None; MAX_REPLAY_STEPS],
            length: 0,
        }
    }

    /// Add a Construct8 delta step
    pub fn push(&mut self, step: Construct8) -> bool {
        if self.length >= MAX_REPLAY_STEPS {
            return false;
        }
        self.steps[self.length] = Some(step);
        self.length += 1;
        true
    }

    /// Replay the step sequence deterministically onto a target RelationPage state.
    /// Returns the updated state on success, or a concrete `Refusal` on failure.
    pub fn run(
        &self,
        mut state: RelationPage,
        current_time: u64,
    ) -> Result<RelationPage, Refusal> {
        for i in 0..self.length {
            if let Some(ref step) = self.steps[i] {
                // Check capability mask: validation constraint
                // We enforce that the mask cannot be all zeros [0, 0, 0, 0, 0, 0, 0, 0]
                if step.mask.as_bytes() == &[0u8; IDENTIFIER_SIZE] {
                    let mut op_hash = [0u8; HASH_SIZE];
                    op_hash[0..8].copy_from_slice(step.subject.as_bytes());
                    op_hash[8..16].copy_from_slice(step.predicate.as_bytes());
                    return Err(Refusal::new(
                        RefusalCode::ConstraintViolation,
                        op_hash,
                        step.to_bytes(),
                        current_time,
                    ));
                }

                // Mutate state: insert the semantic subject-object link
                let pair = Pair2::new(step.subject, Node8::from_bytes(*step.object.as_bytes()));
                if !state.insert(pair) {
                    // Fail if page is full or can't be processed
                    let mut op_hash = [0u8; HASH_SIZE];
                    op_hash[0..8].copy_from_slice(step.subject.as_bytes());
                    op_hash[8..16].copy_from_slice(step.predicate.as_bytes());
                    return Err(Refusal::new(
                        RefusalCode::ConstraintViolation,
                        op_hash,
                        step.to_bytes(),
                        current_time,
                    ));
                }
            }
        }
        Ok(state)
    }
}
