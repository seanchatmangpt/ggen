# genesis-lockchain Documentation

Merkle-linked provenance storage for audit trails and deterministic receipts.

## Overview

The `genesis-lockchain` crate provides a Merkle-linked chain of receipts for complete audit trails and provenance tracking. It ensures deterministic receipt generation (hash(A) = hash(μ(O))) and cryptographic linking between receipts.

## Quick Start

```rust
use genesis_lockchain::{Lockchain, LockchainEntry, ReceiptHash};
use alloc::collections::BTreeMap;

// Create lockchain
let mut chain = Lockchain::new();

// Create receipt entry
let entry = LockchainEntry {
    receipt_id: "receipt-123".to_string(),
    receipt_hash: [0; 32], // Will be computed
    parent_hash: None,
    timestamp_ms: 1000,
    metadata: BTreeMap::new(),
};

// Append to lockchain
let hash = chain.append(entry)?;

// Verify receipt integrity
let is_valid = chain.verify("receipt-123")?;
```

## Core Components

### Lockchain

Main lockchain implementation:

```rust
pub struct Lockchain {
    entries: Vec<LockchainEntry>,
    merkle_root: Option<ReceiptHash>,
    #[cfg(feature = "std")]
    git_repo_path: Option<String>,
}
```

**Methods:**
- `new()` - Create empty lockchain
- `with_git_repo(path)` - Create lockchain with Git integration
- `append(entry)` - Append receipt entry and compute hash
- `get_receipt(id)` - Get receipt by ID
- `verify(id)` - Verify receipt integrity
- `merkle_root()` - Get current Merkle root
- `entries()` - Get all entries
- `merge_receipts(ids)` - Merge multiple receipts into Merkle tree

### LockchainEntry

Receipt entry structure:

```rust
pub struct LockchainEntry {
    pub receipt_id: String,
    pub receipt_hash: ReceiptHash,
    pub parent_hash: Option<ReceiptHash>,
    pub timestamp_ms: u64,
    pub metadata: BTreeMap<String, String>,
}
```

**Fields:**
- `receipt_id` - Unique receipt identifier
- `receipt_hash` - SHA-256 hash (computed on append)
- `parent_hash` - Hash of previous entry (Merkle linking)
- `timestamp_ms` - Timestamp in milliseconds
- `metadata` - Additional metadata (key-value pairs)

### ReceiptHash

32-byte SHA-256 hash:

```rust
pub type ReceiptHash = [u8; 32];
```

## Usage Examples

### Basic Lockchain Operations

```rust
use genesis_lockchain::{Lockchain, LockchainEntry};
use alloc::collections::BTreeMap;

let mut chain = Lockchain::new();

// Create first entry
let entry1 = LockchainEntry {
    receipt_id: "receipt-1".to_string(),
    receipt_hash: [0; 32],
    parent_hash: None,
    timestamp_ms: 1000,
    metadata: {
        let mut m = BTreeMap::new();
        m.insert("operation".to_string(), "boot.init".to_string());
        m
    },
};

let hash1 = chain.append(entry1)?;

// Create second entry (linked to first)
let entry2 = LockchainEntry {
    receipt_id: "receipt-2".to_string(),
    receipt_hash: [0; 32],
    parent_hash: Some(hash1), // Link to previous
    timestamp_ms: 2000,
    metadata: BTreeMap::new(),
};

let hash2 = chain.append(entry2)?;

// Verify chain integrity
assert!(chain.verify("receipt-1")?);
assert!(chain.verify("receipt-2")?);

// Get Merkle root
let root = chain.merkle_root();
```

### Receipt Merging (Merkle Tree)

```rust
use genesis_lockchain::Lockchain;

let mut chain = Lockchain::new();

// Append multiple receipts
chain.append(entry1)?;
chain.append(entry2)?;
chain.append(entry3)?;

// Merge receipts into Merkle tree
let receipt_ids = vec![
    "receipt-1".to_string(),
    "receipt-2".to_string(),
    "receipt-3".to_string(),
];

let merkle_hash = chain.merge_receipts(&receipt_ids)?;

// Merkle hash represents combined hash of all receipts
```

### Git Integration

```rust
use genesis_lockchain::Lockchain;

// Create lockchain with Git integration
let mut chain = Lockchain::with_git_repo("./receipts-repo".to_string());

// Append entry (automatically commits to Git)
let entry = LockchainEntry {
    receipt_id: "receipt-123".to_string(),
    receipt_hash: [0; 32],
    parent_hash: None,
    timestamp_ms: 1000,
    metadata: BTreeMap::new(),
};

chain.append(entry)?;
// Receipt file written to ./receipts-repo/receipts/receipt-123.json
```

### Receipt Verification

```rust
use genesis_lockchain::Lockchain;

let mut chain = Lockchain::new();
chain.append(entry)?;

// Verify receipt integrity
match chain.verify("receipt-123") {
    Ok(true) => println!("Receipt is valid"),
    Ok(false) => println!("Receipt integrity check failed"),
    Err(e) => println!("Error: {:?}", e),
}

// Get receipt details
if let Some(receipt) = chain.get_receipt("receipt-123") {
    println!("Receipt ID: {}", receipt.receipt_id);
    println!("Hash: {:x?}", receipt.receipt_hash);
    println!("Parent: {:?}", receipt.parent_hash);
    println!("Timestamp: {} ms", receipt.timestamp_ms);
}
```

### Provenance Tracking

```rust
use genesis_lockchain::{Lockchain, LockchainEntry};
use alloc::collections::BTreeMap;

// Create receipt with provenance metadata
let mut metadata = BTreeMap::new();
metadata.insert("operation".to_string(), "hook.execute".to_string());
metadata.insert("hook_id".to_string(), "hook-123".to_string());
metadata.insert("ticks".to_string(), "5".to_string());
metadata.insert("span_id".to_string(), "span-456".to_string());

let entry = LockchainEntry {
    receipt_id: format!("receipt-{}", timestamp),
    receipt_hash: [0; 32],
    parent_hash: previous_hash,
    timestamp_ms: current_timestamp(),
    metadata,
};

let hash = chain.append(entry)?;

// Provenance: hash(A) = hash(μ(O))
// Receipt hash represents the action (A) that produced observation (O)
```

## Key Features

- **Merkle Linking**: Cryptographic chain of receipts with parent hash linking
- **Deterministic**: Same inputs produce same receipts (hash(A) = hash(μ(O)))
- **Provenance Tracking**: Complete audit trail for all operations
- **Receipt Merging**: Build Merkle trees for batch operations
- **Git Integration**: Optional Git commit integration for receipt storage
- **Integrity Verification**: Verify receipt hash and parent chain
- **URDNA2015 Canonicalization**: Deterministic hash computation

## Hash Computation

Receipts are hashed using SHA-256 after URDNA2015-like canonicalization:

1. **Canonical Order**: receipt_id → receipt_hash → parent_hash → timestamp → metadata (sorted keys)
2. **SHA-256**: Hash canonical representation
3. **Parent Linking**: Each receipt links to previous receipt's hash

**Canonicalization ensures:**
- Deterministic hashing (same input = same hash)
- Order-independent metadata (sorted keys)
- Provenance preservation (hash(A) = hash(μ(O)))

## Dependencies

- `sha2` - SHA-256 hashing
- `sha3` (optional) - Alternative hashing
- `urdna2015` (optional) - RDF canonicalization
- `serde_json` (optional, std feature) - JSON serialization
- `hex` (optional, std feature) - Hex encoding

## Feature Flags

- `std` - Enables std library features (Git integration, file I/O)

## Performance

- **Append**: O(1) amortized (hash computation is O(n) where n = entry size)
- **Lookup**: O(n) linear search (consider BTreeMap for O(log n) if needed)
- **Verification**: O(1) hash computation + O(n) parent lookup
- **Merging**: O(n log n) Merkle tree construction

## Related Documentation

- [Architecture Guide](../../../docs/ARCHITECTURE.md) - 🆕 Consolidated 80/20 guide (System architecture)
- [Architecture Reference](../../../docs/architecture.md) - Detailed architecture reference
- [Integration](../../../docs/integration.md) - Integration guide
- [ETL Pipeline](../../../rust/genesis-etl/docs/README.md) - ETL pipeline integration
- [Formal Foundations](../../../docs/formal-foundations.md) - Mathematical foundations (provenance laws)
