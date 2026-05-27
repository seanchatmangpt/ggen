# Lockchain Auditing & Time-Travel

**Version:** 1.0.0
**Features:** Receipt auditing, historical queries, time-travel verification
**Status:** Production Ready ✅

---

## Overview

`genesis-lockchain` enables cryptographic auditing and time-travel queries through:

1. **Receipt-Level Auditing** - Verify individual receipts via Merkle proofs
2. **Cycle-Level Queries** - Query historical execution cycles
3. **Time-Travel Verification** - Reconstruct state at any point in history
4. **Continuity Checking** - Detect gaps in execution history
5. **Content Addressing** - BLAKE3-based cryptographic hashing

---

## Receipt Auditing

### CLI Integration

**From `genesis-cli/src/receipt.rs`:**

```bash
# Get receipt by ID
$ knhk receipt get <receipt-id>
{
  "id": "receipt-12345",
  "ticks": 5,
  "lanes": 4,
  "span_id": 100,
  "a_hash": 0xABCD1234,
  "timestamp_ms": 1699564800000
}

# Verify receipt integrity (Merkle proof)
$ knhk receipt verify <receipt-id>
{
  "id": "receipt-12345",
  "valid": true,
  "merkle_proof": ["0x...", "0x...", "0x..."],
  "root": "0x92a4...",
  "cycle": 100
}

# List all receipts
$ knhk receipt list
{
  "receipts": [
    "receipt-12345",
    "receipt-12346",
    ...
  ]
}
```

### Programmatic API

```rust
use genesis_lockchain::{LockchainStorage, MerkleTree};

// Load storage
let storage = LockchainStorage::new("/var/lib/genesis/lockchain.db")?;

// Get cycle entry (contains Merkle root)
let entry = storage.get(cycle)?;

// Generate proof for specific receipt
let tree = reconstruct_tree_from_receipts(cycle_receipts)?;
let proof = tree.generate_proof(receipt_index)?;

// Verify receipt was in cycle
let leaf_hash = hash_receipt(&receipt);
assert!(MerkleTree::verify(&leaf_hash, &proof, &entry.root));
```

---

## Time-Travel Queries

### Historical State Reconstruction

**Query state at specific cycle:**

```rust
use genesis_lockchain::LockchainStorage;

// Load storage
let storage = LockchainStorage::new("/var/lib/genesis/lockchain.db")?;

// Time-travel to cycle 100
let entry = storage.get(100)?;
println!("Root at cycle 100: {:?}", entry.root);
println!("Quorum proof: {:?}", entry.proof);

// Reconstruct all receipts for cycle 100
let receipts = get_receipts_for_cycle(100)?;
for receipt in receipts {
    println!("Receipt: {:?}", receipt);
}
```

### Range Queries

```rust
// Query all cycles in range [100, 200)
let entries = storage.range_query(100, 200)?;

for entry in entries {
    println!("Cycle {}: Root {:?}", entry.cycle, entry.root);
}
```

### Continuity Verification

```rust
// Check for gaps in execution history
if !storage.verify_continuity(100, 200)? {
    eprintln!("WARNING: Gap detected in cycles 100-200!");

    // Find the gap
    for cycle in 100..200 {
        if storage.get(cycle).is_err() {
            eprintln!("Missing cycle: {}", cycle);
        }
    }
}
```

---

## Content Addressing

### BLAKE3 Hashing

**From `rust/docs/content_addressing.md`:**

Lockchain uses BLAKE3 for cryptographic hashing:

```rust
use genesis_hot::{ContentId, content_hash};

// Hash receipt data
let data = format!("{}-{}-{}-{}",
    cycle, subject, predicate, object
);
let hash = content_hash(data.as_bytes());

// Create content-addressed ID
let cid = ContentId::from_bytes(data.as_bytes());
assert!(cid.is_valid());
assert!(cid.is_computed());

// Get hex representation
let hex = cid.to_hex();
println!("Content ID: {}", hex); // 64 character hex
```

**Performance:**
- 16 bytes: <1 tick (~50ns)
- 64 bytes: <1 tick (~200ns)
- 1 KB: ~1000 cycles

---

## Receipt Validation

### Policy-Based Validation

**From `genesis-validation/policies/receipt_validation.rego`:**

Receipts are validated against Rego policies:

```rego
package knhk.receipt_validation

# Receipt ID must be non-empty
violation[msg] {
    input.receipt_id == ""
    msg := "Receipt ID cannot be empty"
}

# Hash must be 32 bytes (256 bits)
violation[msg] {
    count(input.receipt_hash) != 32
    msg := sprintf("Receipt hash must be 32 bytes, got %d", [count(input.receipt_hash)])
}

# Ticks must be ≤8 (Chatman Constant)
violation[msg] {
    input.ticks > 8
    msg := sprintf("Receipt ticks %d exceed budget (8)", [input.ticks])
}

# Timestamp must be valid
violation[msg] {
    input.timestamp_ms == 0
    msg := "Receipt timestamp cannot be zero"
}

# All checks pass
valid {
    input.receipt_id != ""
    count(input.receipt_hash) == 32
    input.ticks <= 8
    input.timestamp_ms > 0
}
```

### Programmatic Validation

```rust
use genesis_validation::PolicyEngine;

// Load policy
let engine = PolicyEngine::new("policies/receipt_validation.rego")?;

// Validate receipt
let receipt_json = serde_json::json!({
    "receipt_id": "receipt-12345",
    "receipt_hash": hash_bytes,  // 32-byte array
    "ticks": 5,
    "timestamp_ms": 1699564800000
});

match engine.validate(&receipt_json)? {
    Ok(_) => println!("Receipt is valid"),
    Err(violations) => {
        for v in violations {
            eprintln!("Violation: {}", v);
        }
    }
}
```

---

## Audit Trails

### Git Integration

**From `storage.rs`:**

Every cycle creates a Git commit for immutable audit trail:

```bash
# View lockchain history
$ cd /var/lib/genesis/lockchain.git
$ git log --oneline
abc123 Lockchain: cycle 102, root 5c3a...
def456 Lockchain: cycle 101, root 8f1b...
789abc Lockchain: cycle 100, root 92a4...

# Show specific cycle
$ git show abc123
commit abc123...
Author: KNHK Lockchain <lockchain@knhk.io>
Date:   Thu Nov 7 00:00:00 2025 +0000

    Lockchain: cycle 102, root 5c3a...

    {
      "cycle": 102,
      "root": "5c3a...",
      "proof": { ... },
      "timestamp": 1699564800
    }

# Verify commit signature (future: GPG signing)
$ git verify-commit abc123
```

### Export for Compliance

```rust
// Export lockchain for compliance/backup
let json = storage.export()?;
std::fs::write("lockchain-backup.json", json)?;

// Verify exported data
let imported = LockchainStorage::import(&json)?;
assert_eq!(imported.get_latest_root()?, storage.get_latest_root()?);
```

---

## Time-Travel Use Cases

### 1. Incident Investigation

```rust
// Investigate incident at cycle 150
let entry = storage.get(150)?;

// Get all receipts for that cycle
let receipts = get_receipts_for_cycle(150)?;

// Check which operations executed
for receipt in receipts {
    println!("Operation: {}, Subject: {}, Object: {}",
        receipt.predicate, receipt.subject, receipt.object);
}

// Verify each receipt was in Merkle tree
let tree = reconstruct_tree_from_receipts(receipts)?;
assert_eq!(tree.compute_root()?, entry.root);
```

### 2. Compliance Audits

```rust
// Audit all operations in date range
let start_cycle = timestamp_to_cycle(start_date);
let end_cycle = timestamp_to_cycle(end_date);

let entries = storage.range_query(start_cycle, end_cycle)?;

// Generate compliance report
let mut report = ComplianceReport::new();
for entry in entries {
    let receipts = get_receipts_for_cycle(entry.cycle)?;
    report.add_cycle(entry.cycle, receipts);
}
report.save("audit-2025-q4.pdf")?;
```

### 3. State Reconstruction

```rust
// Reconstruct state at cycle N
fn reconstruct_state(cycle: u64) -> Result<SystemState> {
    let mut state = SystemState::new();

    // Replay all operations from cycle 0 to N
    for c in 0..=cycle {
        let receipts = get_receipts_for_cycle(c)?;
        for receipt in receipts {
            state.apply_operation(receipt)?;
        }
    }

    Ok(state)
}

// Verify state matches
let state_100 = reconstruct_state(100)?;
let state_200 = reconstruct_state(200)?;
assert_ne!(state_100.hash(), state_200.hash());
```

### 4. Forensic Analysis

```rust
// Find when specific data was modified
fn find_modification(subject: &str, object: &str) -> Vec<u64> {
    let mut cycles = Vec::new();

    for cycle in 0..storage.get_latest_root()?.unwrap().cycle {
        let receipts = get_receipts_for_cycle(cycle)?;
        for receipt in receipts {
            if receipt.subject == subject && receipt.object == object {
                cycles.push(cycle);
            }
        }
    }

    cycles
}

// Usage
let modifications = find_modification("user:alice", "balance:1000");
println!("Balance modified at cycles: {:?}", modifications);
```

---

## Merkle Proof Verification

### Visual Representation

```
Receipt Proof Verification:

         Root Hash (from storage)
        /          \
    Hash(A,B)    Hash(C,D)
    /    \       /    \
   A      B     C      D     ← Receipts

Proof for Receipt B:
  Step 1: Hash(B) with sibling A → Hash(A,B)
  Step 2: Hash(A,B) with uncle Hash(C,D) → Root

Verification:
  current = Hash(B)
  current = Hash(A, current)
  current = Hash(current, Hash(C,D))
  assert(current == Root)
```

### Code Example

```rust
use genesis_lockchain::{MerkleTree, hash_receipt};

// Build tree from cycle receipts
let mut tree = MerkleTree::new();
for receipt in receipts {
    tree.add_receipt(&receipt);
}
let root = tree.compute_root()?;

// Generate proof for receipt index 1
let proof = tree.generate_proof(1)?;

// Verify proof
let leaf = hash_receipt(&receipts[1]);
assert!(MerkleTree::verify(&leaf, &proof, &root));

// Proof contains sibling hashes at each layer
println!("Proof path: {} hashes", proof.len());
for (i, hash) in proof.iter().enumerate() {
    println!("Layer {}: {:?}", i, hash);
}
```

---

## Performance Considerations

### Query Optimization

**Range Queries:**
```rust
// ❌ SLOW: Individual queries in loop
for cycle in 100..200 {
    let entry = storage.get(cycle)?; // 100 database lookups
}

// ✅ FAST: Single range query
let entries = storage.range_query(100, 200)?; // 1 database scan
```

**Latest Root:**
```rust
// ❌ SLOW: Scan entire database
let max_cycle = entries.iter().max_by_key(|e| e.cycle)?;

// ✅ FAST: Direct lookup
let latest = storage.get_latest_root()?;
```

### Caching Strategies

```rust
use std::collections::HashMap;

// Cache recent roots in memory
struct LockchainCache {
    storage: LockchainStorage,
    cache: HashMap<u64, Hash>,
}

impl LockchainCache {
    fn get_root(&mut self, cycle: u64) -> Result<Hash> {
        if let Some(root) = self.cache.get(&cycle) {
            return Ok(*root);
        }

        let entry = self.storage.get(cycle)?;
        self.cache.insert(cycle, entry.root);
        Ok(entry.root)
    }
}
```

---

## Security Considerations

### Audit Log Integrity

**Git commits provide:**
- ✅ Immutable history (cannot rewrite without detection)
- ✅ Cryptographic linking (each commit references parent)
- ✅ Distributed verification (can clone and verify independently)

**Merkle trees provide:**
- ✅ Receipt-level integrity (cannot modify single receipt)
- ✅ Efficient proofs (O(log n) size, not O(n))
- ✅ Cryptographic binding (BLAKE3 collision resistance)

**Quorum consensus provides:**
- ✅ Byzantine fault tolerance (up to f < threshold failures)
- ✅ Distributed agreement (no single point of trust)
- ✅ Cryptographic proofs (signature verification in v1.1)

### Attack Scenarios

**Scenario 1: Modify historical receipt**
- Attack: Change receipt in cycle 100
- Detection: Merkle root changes, breaks chain integrity
- Mitigation: Lockchain storage is append-only

**Scenario 2: Delete cycles**
- Attack: Remove entries from database
- Detection: `verify_continuity()` fails
- Mitigation: Git history provides backup

**Scenario 3: Forge consensus**
- Attack: Claim fake quorum agreement
- Detection: Signature verification fails (v1.1)
- Mitigation: Threshold prevents single-node forgery

---

## Best Practices

### 1. Regular Backups

```bash
# Backup Sled database
$ cp -r /var/lib/genesis/lockchain.db /backup/lockchain-$(date +%Y%m%d).db

# Backup Git repository
$ cd /var/lib/genesis/lockchain.git
$ git bundle create /backup/lockchain-$(date +%Y%m%d).bundle --all
```

### 2. Continuity Monitoring

```rust
// Periodic continuity checks
use tokio::time::{interval, Duration};

async fn monitor_continuity(storage: Arc<LockchainStorage>) {
    let mut ticker = interval(Duration::from_secs(60));

    loop {
        ticker.tick().await;

        let latest = storage.get_latest_root()?.unwrap().cycle;
        let start = latest.saturating_sub(1000);

        if !storage.verify_continuity(start, latest)? {
            alert!("Lockchain continuity broken!");
        }
    }
}
```

### 3. Receipt Validation

```rust
// Always validate receipts before adding to lockchain
use genesis_validation::PolicyEngine;

let policy = PolicyEngine::new("policies/receipt_validation.rego")?;

for receipt in receipts {
    if let Err(violations) = policy.validate(&receipt) {
        warn!("Invalid receipt: {:?}", violations);
        continue; // Skip invalid receipts
    }

    tree.add_receipt(&receipt);
}
```

---

## Troubleshooting

### Missing Cycles

```bash
# Find gaps in lockchain
$ knhk lockchain verify-continuity --start 0 --end 1000
WARNING: Gap detected!
Missing cycles: [150, 151, 152]

# Check Git log for those cycles
$ cd /var/lib/genesis/lockchain.git
$ git log --grep="cycle 150"
# (no results = cycle not persisted)
```

### Invalid Merkle Proofs

```rust
// Debug proof verification failure
let leaf = hash_receipt(&receipt);
let proof = tree.generate_proof(index)?;
let root = tree.compute_root()?;

if !MerkleTree::verify(&leaf, &proof, &root) {
    eprintln!("Proof verification failed!");
    eprintln!("Leaf: {:?}", leaf);
    eprintln!("Proof path: {:?}", proof);
    eprintln!("Expected root: {:?}", root);

    // Manually reconstruct
    let mut current = leaf;
    for (i, sibling) in proof.iter().enumerate() {
        println!("Layer {}: {:?} + {:?}", i, current, sibling);
        current = blake3::hash(&[&current[..], &sibling[..]].concat()).into();
    }
    println!("Computed root: {:?}", current);
}
```

---

## Future Enhancements

### v1.1 Roadmap

1. **Time-Travel Snapshots**
   - Efficient state snapshots at checkpoints
   - Incremental delta compression
   - Instant state reconstruction

2. **Advanced Queries**
   - Full-text search on receipt content
   - Temporal queries (operations in date range)
   - Pattern matching (find all operations matching X)

3. **Audit Dashboard**
   - Web UI for lockchain exploration
   - Visual Merkle tree navigation
   - Real-time continuity monitoring

4. **Compliance Reporting**
   - Automated audit report generation
   - PDF/HTML export formats
   - Configurable retention policies

---

## References

- [Merkle Trees for Certificate Transparency](https://datatracker.ietf.org/doc/html/rfc6962)
- [BLAKE3 Cryptographic Hash](https://github.com/BLAKE3-team/BLAKE3)
- [Git Internals](https://git-scm.com/book/en/v2/Git-Internals-Git-Objects)
- [Byzantine Fault Tolerance](https://pmg.csail.mit.edu/papers/osdi99.pdf)

---

**Document Version:** 1.0.0
**Last Updated:** 2025-11-07
**Maintainer:** KNHK Team
