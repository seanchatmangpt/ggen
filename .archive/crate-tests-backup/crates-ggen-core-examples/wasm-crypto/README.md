# WASM Crypto Module

High-performance cryptographic operations compiled to WebAssembly for web environments.

## Features

- **Password Hashing**: Argon2id with secure defaults
- **Encryption**: AES-256-GCM authenticated encryption
- **Digital Signatures**: Ed25519 signing and verification
- **Hashing**: SHA-256 cryptographic hashing
- **Random Generation**: Cryptographically secure random bytes

## Quick Start

```bash
# Install dependencies
cargo make install-deps

# Build and optimize
cargo make all

# Serve demo locally
cargo make serve
# Open http://localhost:8080
```

## Build Lifecycle

```bash
# Development workflow
cargo make dev              # Build + serve with watch mode

# Individual phases
cargo make wasm-build       # Build WASM module
cargo make wasm-optimize    # Optimize with wasm-opt
cargo make wasm-test        # Run tests in browser
cargo make size-check       # Analyze binary size
cargo make deploy           # Prepare for production

# Utilities
cargo make clean            # Remove artifacts
```

## Size Optimization

Target: **< 50KB gzipped**

Optimizations applied:
- Cargo profile: `opt-level = "z"`, LTO enabled
- wasm-opt: `-Oz` maximum size reduction
- Strip symbols and debug info
- Single codegen unit for better optimization

## API Usage

```javascript
import init, * as wasm from './pkg/wasm_crypto.js';

await init();

// Password hashing
const hash = wasm.hash_password("myPassword", null);
const valid = wasm.verify_password("myPassword", hash);

// Encryption
const key = wasm.generate_key();
const encrypted = wasm.encrypt_aes(data, key, null);
const decrypted = wasm.decrypt_aes(encrypted.ciphertext, key, encrypted.nonce);

// Digital signatures
const keypair = wasm.generate_keypair();
const signature = wasm.sign_message(message, keypair.secretKey);
const valid = wasm.verify_signature(message, signature, keypair.publicKey);

// Hashing
const hash = wasm.hash_sha256(data);
const random = wasm.random_bytes(32);
```

## Browser Compatibility

- Chrome/Edge 89+
- Firefox 89+
- Safari 15+
- Requires WebAssembly and crypto.getRandomValues support

## Security Considerations

- Uses `getrandom` with `js` feature for browser CSPRNG
- Constant-time operations where applicable
- No secret data in panic messages
- Secure defaults for all cryptographic operations

## Performance

Typical operation times (Chrome on M1 Mac):
- Password hash: ~100-200ms (intentionally slow)
- AES encryption: ~0.5-2ms
- Ed25519 sign: ~0.3-1ms
- SHA-256: ~0.1-0.5ms

## License

MIT
