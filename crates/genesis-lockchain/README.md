# genesis-lockchain

Distributed locking mechanism with receipt-based verification for the KNHK framework.

## Features

- Receipt-based verification using BLAKE3 hashing
- Distributed consensus with git-based backend
- Non-blocking lock operations
- Canonical RDF representation for receipt generation
- Persistent lock state across sessions

## Usage

```rust
use genesis_lockchain::{LockchainManager, Receipt};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let manager = LockchainManager::new("./lockchain")?;
    let receipt = manager.create_receipt(data)?;

    // Verify receipt
    manager.verify_receipt(&receipt)?;

    Ok(())
}
```

## License

Licensed under MIT license.
