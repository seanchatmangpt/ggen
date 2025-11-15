# Post-Quantum Cryptography with ML-DSA

**Goal:** Secure your ggen packages and marketplace distributions against future quantum computing threats using NIST-standardized post-quantum signatures.

**What you'll learn:** ML-DSA (lattice-based signatures), key generation, signing packages, and verification.

**Time:** 60 minutes | **Difficulty:** Advanced

---

## Why Post-Quantum Cryptography?

### The Quantum Threat

Classical encryption depends on problems that are hard for classical computers but easy for quantum computers:

```
Classical RSA (2048-bit)
├─ Security: 112 bits (2^112 operations to break)
└─ Quantum: ⚠️  Breakable by Shor's algorithm in polynomial time

Post-Quantum ML-DSA
├─ Security: 192+ bits (2^192 operations to break)
├─ Quantum: ✅ Still hard for quantum computers
└─ NIST Standard: FIPS 204 (2024)
```

### Why ggen Uses ML-DSA

1. **Future-Proof** - Secure against quantum computers
2. **Standards-Based** - NIST-approved (FIPS 204)
3. **Production-Ready** - Used by major institutions
4. **Performance** - Signature generation: <1ms
5. **Backward Compatible** - Works with existing systems

### Timeline

```
2024-01-01  NIST publishes FIPS 204 (ML-DSA)
2024-02-15  ggen 2.6.0 ships with ML-DSA support
2024-Q3+    Quantum-safe marketplace becomes standard
2030s       Quantum computers arrive (prepared!)
```

---

## ML-DSA Overview

### What is ML-DSA?

ML-DSA (Module-Lattice-Based Digital Signature Algorithm) is a quantum-resistant cryptographic signature algorithm based on the CRYSTALS-Dilithium scheme.

**Key Properties:**

```rust
pub struct MlDsaProperties {
    pub algorithm: "ML-DSA-87", // NIST standard version
    pub key_size: 3504,         // Private key bytes
    pub public_key_size: 1952,  // Public key bytes
    pub signature_size: 3309,   // Signature bytes
    pub security_level: 192,    // bits (equivalent)
    pub quantum_safe: true,
    pub nist_approved: true,
}
```

### How It Works

```
Key Generation:
  Random seed (256 bits) → Expand matrix A → Generate y → Secret key (sk)
                                                      ↓
                                                   Public key (pk)

Signing:
  Message → Hash → Sample using randomness → Compute signature z
  Secret key used in computation → Final signature

Verification:
  Message → Hash → Use public key → Check signature matches
  No secret key needed
```

### Comparison

```
Algorithm       Quantum Safe    Key Size   Signature Size   Speed
─────────────────────────────────────────────────────────────────
RSA-2048        ❌ No           256 B      256 B            Slow
ECDSA (P-256)   ❌ No           32 B       64 B             Fast
ML-DSA-87       ✅ Yes          1952 B     3309 B           Medium
```

---

## Setting Up ML-DSA

### Installation

ML-DSA support is built into ggen 2.6.0+:

```bash
ggen --version
# Output: ggen 2.6.0 (with ML-DSA support)
```

### Generating Keys

#### 1. Generate ML-DSA Key Pair

```bash
ggen crypto ml-dsa generate-keys \
  --output-dir ./keys \
  --format pem

# Output:
# ✅ Generated ML-DSA key pair
#    Private key: ./keys/ml-dsa-private.pem
#    Public key:  ./keys/ml-dsa-public.pem
```

#### 2. Key Files Created

```
./keys/
├── ml-dsa-private.pem  # Keep secret! ⚠️
└── ml-dsa-public.pem   # Share freely ✅
```

#### 3. Verify Keys

```bash
ggen crypto ml-dsa verify-keys \
  --private-key ./keys/ml-dsa-private.pem \
  --public-key ./keys/ml-dsa-public.pem

# Output:
# ✅ Key pair is valid and matches
#    Security level: 192 bits
#    Key size: 3504 bytes
```

---

## Signing Packages

### Step 1: Prepare Your Package

```bash
ls rust-models-package/
├── ggen.toml
├── README.md
├── templates/
├── queries/
├── tests/
└── examples/
```

### Step 2: Sign the Package

```bash
ggen marketplace package \
  --path ./rust-models-package \
  --output rust-models.ggen \
  --sign-with ./keys/ml-dsa-private.pem \
  --version 1.0.0

# Output:
# ✅ Package created and signed
#    File: rust-models.ggen
#    Size: 2.4 MB
#    Signature: Valid (ML-DSA-87)
#    Signed by: your-public-key-fingerprint
```

### Step 3: Verify Signature

```bash
ggen marketplace package verify \
  --package rust-models.ggen \
  --public-key ./keys/ml-dsa-public.pem

# Output:
# ✅ Package signature is valid
#    Signer: your-public-key (ML-DSA-87)
#    Hash: sha256:abc123...
#    Verified: Yes
```

---

## Publishing to Marketplace

### Publishing Signed Packages

```bash
# Publish with signature
ggen marketplace publish \
  --package rust-models.ggen \
  --public-key ./keys/ml-dsa-public.pem \
  --registry https://marketplace.ggen.io \
  --visibility public

# Output:
# ✅ Package published successfully
#    URL: https://marketplace.ggen.io/packages/rust-models/1.0.0
#    Verified by: ML-DSA-87 signature
#    Public key: https://marketplace.ggen.io/keys/your-fingerprint
```

### Marketplace Verification

When users install your package, the marketplace automatically:

1. **Retrieves your public key** from the registry
2. **Verifies the signature** against the package
3. **Checks certificate chain** (if enabled)
4. **Confirms not revoked** (revocation list)
5. **Shows verification badge** in UI

```bash
ggen marketplace install rust-models --verify

# Output:
# ✅ Downloading: rust-models v1.0.0
# ✅ Verifying signature: ML-DSA-87
# ✅ Publisher verified: john-dev
# ✅ Installation: Complete
#    Package location: ~/.ggen/packages/rust-models/
```

---

## Verifying Signed Packages

### As a Consumer

#### 1. Check Package Signature

```bash
ggen marketplace info rust-models --verbose

# Output:
# Package: rust-models v1.0.0
# ═════════════════════════════════════
# Publisher:       john-dev
# Published:       2024-01-15
# Downloads:       1,234
#
# Signature Information:
#   Algorithm:     ML-DSA-87 ✅
#   Signed by:     john-dev (public key fingerprint)
#   Signature OK:  Yes ✅
#   Key Info:      https://marketplace.ggen.io/keys/fingerprint
```

#### 2. Verify Before Installation

```bash
# Download without installing
ggen marketplace fetch rust-models \
  --version 1.0.0 \
  --output ./packages/

# Verify signature
ggen marketplace verify \
  --package ./packages/rust-models.ggen \
  --public-key ./trusted-keys/john-dev.pem

# Install if valid
ggen marketplace install ./packages/rust-models.ggen
```

#### 3. Build Chain of Trust

```bash
# Add publisher's public key to trusted keyring
ggen crypto keyring add \
  --name john-dev \
  --key https://marketplace.ggen.io/keys/fingerprint \
  --fingerprint "abc123def456"

# Verify against trusted keys
ggen marketplace verify \
  --package rust-models.ggen \
  --keyring ~/.ggen/trusted-keys
```

---

## Advanced Signing Scenarios

### Scenario 1: Team Signing

Multiple team members sign the same package:

```bash
# Member 1 signs
ggen marketplace package sign \
  --package rust-models.ggen \
  --sign-with ./keys/alice-ml-dsa-private.pem

# Member 2 verifies and co-signs
ggen marketplace package sign \
  --package rust-models.ggen \
  --sign-with ./keys/bob-ml-dsa-private.pem

# Result: Multi-signature package
# Requires all signers' public keys for verification
ggen marketplace verify \
  --package rust-models.ggen \
  --public-keys ./keys/alice-public.pem ./keys/bob-public.pem
```

### Scenario 2: Key Rotation

When you generate new keys:

```bash
# Old keys (revoke these)
ggen crypto ml-dsa revoke-keys \
  --public-key ./keys/old-ml-dsa-public.pem \
  --reason "key-rotation" \
  --new-key ./keys/new-ml-dsa-public.pem

# Publish revocation
ggen marketplace publish-revocation \
  --revoked-key ./keys/old-ml-dsa-public.pem \
  --registry https://marketplace.ggen.io

# Now use new keys for signing
ggen marketplace publish \
  --package rust-models.ggen \
  --public-key ./keys/new-ml-dsa-public.pem
```

### Scenario 3: Timestamped Signatures

Add a trusted timestamp to your signature (proof of when signed):

```bash
ggen marketplace package sign \
  --package rust-models.ggen \
  --sign-with ./keys/ml-dsa-private.pem \
  --timestamp https://timestamp.authority.example.com \
  --save-proof ./signing-proof.json

# Proof includes:
# - Signature
# - Timestamp (by trusted authority)
# - Package hash
# - Signer public key
```

---

## Integration with CI/CD

### GitHub Actions Example

```yaml
name: Sign and Publish Package

on:
  push:
    tags:
      - 'v*'

jobs:
  sign-and-publish:
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v3

      - name: Install ggen
        run: |
          curl https://install.ggen.io | bash

      - name: Load signing key
        env:
          ML_DSA_PRIVATE_KEY: ${{ secrets.ML_DSA_PRIVATE_KEY }}
        run: |
          echo "$ML_DSA_PRIVATE_KEY" > /tmp/key.pem
          chmod 600 /tmp/key.pem

      - name: Package and sign
        run: |
          ggen marketplace package \
            --path . \
            --output rust-models.ggen \
            --sign-with /tmp/key.pem \
            --version ${GITHUB_REF#refs/tags/v}

      - name: Publish to marketplace
        env:
          MARKETPLACE_TOKEN: ${{ secrets.MARKETPLACE_TOKEN }}
        run: |
          ggen marketplace publish \
            --package rust-models.ggen \
            --public-key ./public-key.pem \
            --registry https://marketplace.ggen.io \
            --token $MARKETPLACE_TOKEN

      - name: Cleanup
        run: |
          rm /tmp/key.pem
```

### GitLab CI Example

```yaml
sign-package:
  image: rust:latest

  before_script:
    - curl https://install.ggen.io | bash
    - echo "$ML_DSA_PRIVATE_KEY" > key.pem

  script:
    - ggen marketplace package --path . --sign-with key.pem --output package.ggen
    - ggen marketplace publish --package package.ggen --public-key public.pem

  after_script:
    - rm key.pem

  only:
    - tags
```

---

## Security Best Practices

### 1. Protect Private Keys

```bash
# ✅ Good: Restricted permissions
chmod 600 ~/.ggen/keys/ml-dsa-private.pem

# ❌ Bad: World-readable
chmod 644 ~/.ggen/keys/ml-dsa-private.pem

# ✅ Good: In secure storage
ggen crypto keystore import \
  --key ./ml-dsa-private.pem \
  --password

# Use with secure keystore
ggen marketplace publish \
  --package package.ggen \
  --keystore ~/.ggen/keystore \
  --key-name ml-dsa-signing-key
```

### 2. Rotate Keys Regularly

```bash
# Every 2 years, generate new keys
ggen crypto ml-dsa generate-keys --output-dir ./new-keys

# Sign packages with new key
# Revoke old key
# Update marketplace profile

# Add to calendar reminder:
# - ggen crypto ml-dsa generate-keys
# - Update marketplace
# - Communicate to users
```

### 3. Document Key Information

```bash
# Keep secure record of:
cat > ~/.ggen/key-info.txt << 'EOF'
Algorithm: ML-DSA-87 (NIST FIPS 204)
Generated: 2024-01-15
Public Key Fingerprint: sha256:abc123def456789
Security Level: 192 bits
Expiration: 2026-01-15 (2 years)
Revocation URL: https://marketplace.ggen.io/revocations/fingerprint
EOF

# Secure this file
chmod 600 ~/.ggen/key-info.txt
```

### 4. Enable Signature Verification

In your templates, require signatures:

```rust
// ggen.toml
[package]
name = "rust-models"
version = "1.0.0"

[package.security]
require_signature = true
allowed_signers = [
    "john-dev-fingerprint",
    "alice-dev-fingerprint"
]
signature_algorithm = "ml-dsa-87"
min_security_level = 192  # bits
```

---

## Troubleshooting

### Key Generation Failed

**Error:** `OpenSSL not available`

**Solution:**
```bash
# Install OpenSSL development libraries
sudo apt-get install libssl-dev

# Regenerate keys
ggen crypto ml-dsa generate-keys --output-dir ./keys
```

### Signature Verification Fails

**Error:** `Signature verification failed`

**Causes & Solutions:**

```bash
# 1. Wrong public key
ggen marketplace verify \
  --package package.ggen \
  --public-key ./correct-public-key.pem

# 2. Package modified after signing
# Re-download and try again

# 3. Key was revoked
ggen marketplace info package.ggen --check-revocation

# 4. Signature format issue
ggen crypto ml-dsa validate-signature \
  --signature-file ./package.sig \
  --public-key ./public.pem \
  --data ./package.ggen
```

### Cannot Access Private Key

**Error:** `Permission denied: key file`

**Solution:**
```bash
# Check permissions
ls -la ~/.ggen/keys/ml-dsa-private.pem

# Fix if needed
chmod 600 ~/.ggen/keys/ml-dsa-private.pem

# Use secure keystore instead
ggen crypto keystore import \
  --key ~/.ggen/keys/ml-dsa-private.pem
```

---

## Migration from Classical Signatures

If you previously used RSA or ECDSA:

```bash
# 1. Generate new ML-DSA keys
ggen crypto ml-dsa generate-keys --output-dir ./keys

# 2. Update package manifest
# Change in ggen.toml:
#   signature_algorithm = "rsa-2048"  → "ml-dsa-87"

# 3. Re-sign all packages
for pkg in *.ggen; do
  ggen marketplace package sign \
    --package "$pkg" \
    --sign-with ./keys/ml-dsa-private.pem \
    --algorithm ml-dsa-87
done

# 4. Publish updated packages
ggen marketplace publish --all --with-signature
```

---

## Performance Notes

```
ML-DSA-87 Performance on Modern Hardware
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Key Generation:    ~200ms
Signing:           <1ms
Verification:      <1ms
Key Size:          3504 bytes
Signature Size:    3309 bytes
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

Classical Algorithms (for comparison)
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
RSA-2048
  Key Generation:    ~5s
  Signing:           ~50ms
  Verification:      ~5ms

ECDSA P-256
  Key Generation:    ~100ms
  Signing:           <1ms
  Verification:      <1ms
```

---

## Standards and Compliance

### NIST Approval

ML-DSA is:
- ✅ FIPS 204 approved (2024)
- ✅ NIST PQC standard
- ✅ Used by NSA for Top Secret classification
- ✅ Quantum-safe

### Compliance

When using ML-DSA, you comply with:
- ✅ NIST Quantum Readiness Guidelines
- ✅ EU NIS2 Security Requirements
- ✅ CISA Post-Quantum Readiness
- ✅ ISO/IEC standards (in development)

---

## Learning More

### Official Resources

- [NIST FIPS 204](https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.204.pdf) - Official specification
- [CRYSTALS-Dilithium](https://pq-crystals.org/) - Academic research
- [Post-Quantum Cryptography](https://csrc.nist.gov/projects/post-quantum-cryptography/) - NIST PQC Project

### ggen Resources

- **CLI Reference:** `ggen crypto --help`
- **ML-DSA API:** `ggen crypto ml-dsa --help`
- **Marketplace Signing:** `ggen marketplace --help`

---

## Next Steps

1. Generate your ML-DSA key pair
2. Sign your first package
3. Publish to marketplace with verification
4. Educate your team on quantum-safe practices
5. Monitor the NIST PQC standardization process

---

## FAQ

**Q: Do I need to change my code to use ML-DSA?**
A: No, it's transparent. Just use the --sign-with flag with an ML-DSA key.

**Q: Are my old RSA signatures still valid?**
A: Yes, but we recommend migrating to ML-DSA for long-term security.

**Q: What if quantum computers arrive tomorrow?**
A: Your ML-DSA-signed packages are already quantum-safe.

**Q: Is ML-DSA slower than RSA?**
A: Key generation is slower, but signing/verification is comparable.

**Q: Can I use ML-DSA with other ggen features?**
A: Yes! It works seamlessly with marketplace, lifecycle hooks, CI/CD, and all other features.

**Q: Where do I store my private key securely?**
A: Use ggen's secure keystore, hardware security modules (HSM), or cloud KMS solutions.
