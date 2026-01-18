# Architecture: Ontology Packs in the Marketplace

Understanding the marketplace infrastructure and design decisions.

---

## System Architecture

```
┌─────────────────────────────────────────────────────────┐
│                    Marketplace API                      │
│  - Pack registry                                        │
│  - Search/discovery                                     │
│  - Authentication                                       │
│  - Download serving                                     │
└─────────────────────────────────────────────────────────┘
                       ↑
                       │ HTTPS
                       ↓
┌─────────────────────────────────────────────────────────┐
│                   ggen CLI Client                       │
│  - Pack installation                                    │
│  - Local registry                                       │
│  - Code generation                                      │
│  - Signature verification                               │
└─────────────────────────────────────────────────────────┘
                       ↑
                       │ File I/O
                       ↓
┌─────────────────────────────────────────────────────────┐
│            Local Pack Registry                          │
│  ~/.ggen/ontology-packs/                                │
│    ├─ schema.org/                                       │
│    ├─ foaf/                                             │
│    └─ dublin-core/                                      │
└─────────────────────────────────────────────────────────┘
```

---

## Pack Distribution

### 1. Packaging
```
my-pack/
  ├─ pack.yaml
  ├─ ontology/
  └─ templates/

↓ ggen ontology pack

my-pack-1.0.0.gpack (tar.gz with metadata)
```

### 2. Signing
```
my-pack-1.0.0.gpack

↓ ggen ontology sign

my-pack-1.0.0.signed.gpack (includes signature)
```

### 3. Publishing
```
Upload to marketplace
→ Validation
→ Review
→ Indexing
→ Available for download
```

### 4. Installation
```
User: ggen ontology install my-pack
→ Download signed pack
→ Verify signature
→ Extract to ~/.ggen/ontology-packs/
→ Register in local index
```

---

## Security Model

### Cryptographic Signing

Every pack is signed with Ed25519:
```
Pack contents → Hash (SHA-256) → Sign with private key → Signature
```

### Verification

```
Download pack → Extract signature → Verify with public key → Trust decision
```

Users can:
- Trust marketplace keys (default)
- Trust specific publisher keys
- Require signature for all packs

---

## Versioning and Updates

### Semantic Versioning
- `1.0.0` → `1.0.1` (patch): Auto-update safe
- `1.0.0` → `1.1.0` (minor): Auto-update safe
- `1.0.0` → `2.0.0` (major): Manual upgrade required

### Update Notifications
```bash
$ ggen ontology list
schema.org (1.0.0) → 1.1.0 available
foaf (1.0.0) - up to date
```

---

## Related Explanations

- [Case Study: Evolution](case-study-evolution.md)
- [Understanding Pack Composition](pack-composition.md)
- [Tutorial: Publishing](../tutorials/04-publishing-pack.md)
