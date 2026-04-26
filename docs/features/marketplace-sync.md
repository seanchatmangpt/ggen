# Marketplace Sync Command

## Overview

The `ggen marketplace sync` command refreshes package metadata from the marketplace registry with intelligent caching. It performs checksum-based change detection to minimize bandwidth and I/O, supporting both full refreshes and cache-aware incremental updates.

## Features

- **Cache-Aware Syncing**: Only downloads packages with changed checksums by default
- **Checksum Verification**: Detects changes via SHA-256 digest comparison
- **Custom Registry URLs**: Sync from alternative registry endpoints
- **Dry-Run Mode**: Preview changes without writing to cache
- **Progress Reporting**: Verbose output with per-package status
- **Automatic Cache Location**: Uses XDG cache standard (~/.cache/ggen/packs/)
- **Idempotent Operations**: Running sync multiple times has the same effect

## Usage

### Basic Sync (Cache-Aware)

Sync packages from marketplace with automatic cache validation:

```bash
ggen marketplace sync
```

This command:
1. Reads marketplace registry
2. Compares package checksums with cached versions
3. Downloads only changed packages
4. Updates cache metadata with epoch bump

### Force Full Refresh

Force download of all packages, bypassing cache checks:

```bash
ggen marketplace sync --force
```

Useful when:
- Cache may be corrupted
- You want to ensure fresh metadata
- Testing cache invalidation logic

### Dry-Run Preview

Preview what would be synced without modifying files:

```bash
ggen marketplace sync --dry-run
```

Output shows:
- Number of packages evaluated
- Number that would be synced
- Number that would be skipped
- Cache location
- Expected changes

Combine with other flags:
```bash
ggen marketplace sync --dry-run --verbose
```

### Custom Registry URL

Sync from alternative marketplace registry:

```bash
ggen marketplace sync --source https://custom-registry.example.com
```

### Verbose Progress Reporting

Show detailed progress for each package:

```bash
ggen marketplace sync --verbose
```

Output includes:
- Current package being processed
- Cache hit/miss status
- Checksum validation results
- Sync success/failure per package

### Combined Flags

```bash
# Force refresh with detailed reporting
ggen marketplace sync --force --verbose

# Dry-run with custom registry
ggen marketplace sync --dry-run --source https://registry.example.com

# Force, dry-run, and verbose together
ggen marketplace sync --force --dry-run --verbose
```

## Cache Location

The default cache location follows XDG standards:

```bash
~/.cache/ggen/packs/
```

Override with environment variable:

```bash
export GGEN_MARKETPLACE_CACHE=/custom/path
ggen marketplace sync
```

Cache structure:
```
~/.cache/ggen/packs/
├── .cache-metadata              # Epoch timestamp of last sync
├── package-a/
│   ├── .checksum               # SHA-256 of package-a
│   └── [package contents]
├── package-b/
│   ├── .checksum               # SHA-256 of package-b
│   └── [package contents]
└── ...
```

## Output

Default output format (text):

```
Marketplace sync complete: 45 synced, 12 updated, 33 skipped
Cache directory: /Users/user/.cache/ggen/packs
Duration: 2341ms
```

### Verbose Output

```
Cache directory: /Users/user/.cache/ggen/packs
Marketplace registry: https://marketplace.ggen.dev/registry
Found 45 packages to evaluate
[1/45] Processing pkg-a@1.0.0
  ✓ Synced
[2/45] Processing pkg-b@2.1.0
  ✓ Skipped (cache valid)
...
[45/45] Processing pkg-z@3.0.0
  ✓ Synced

Marketplace sync complete: 45 synced, 12 updated, 33 skipped
```

## Implementation Architecture

### Three-Layer Pattern

```
CLI Layer (marketplace.rs)
  ↓ Input validation, flag parsing
Domain Layer (resolution + caching logic)
  ↓ Pure business logic
Storage Layer (file system)
  ↓ Cache directory operations
```

### Sync Algorithm

1. **Resolve**: Determine cache directory from env var or XDG default
2. **Validate**: Ensure cache directory is writable
3. **List**: Get marketplace package list with checksums
4. **Evaluate**: For each package:
   - Read cached checksum (if exists)
   - Compare with current checksum
   - Mark for download if changed or missing
5. **Download**: Fetch changed packages in parallel (future optimization)
6. **Checksum**: Write new checksum file for validation on next sync
7. **Invalidate**: Bump epoch timestamp in `.cache-metadata`

### Checksum-Based Change Detection

Cache validity determined by comparing SHA-256 checksums:

```
Cached version:  .checksum file contains "abc123def456..."
Current version: Registry provides  "abc123def456..."
                                       ↓
                                    MATCH? Yes → Skip
                                    MATCH? No  → Download
```

### Dry-Run Mode

When `--dry-run` is enabled:
- Cache directory is created but not populated
- No files are written to cache
- `.cache-metadata` is NOT created
- Output shows what WOULD happen

## Exit Codes

| Code | Meaning |
|------|---------|
| 0 | Success |
| 1 | Cache directory creation failed |
| 2 | Invalid registry URL |
| 3 | Checksum validation failed |
| 4 | Metadata write failed |

## Environment Variables

| Variable | Default | Purpose |
|----------|---------|---------|
| `GGEN_MARKETPLACE_CACHE` | `~/.cache/ggen/packs` | Override cache location |

## Performance Characteristics

- First sync (empty cache): ~2-5s (network-bound)
- Subsequent sync (cached): <500ms (checksum comparison only)
- Dry-run: <100ms (no network, no I/O)
- Force refresh: ~2-5s (full download)

## Troubleshooting

### Cache Not Updating

Verify cache location and permissions:

```bash
# Check cache directory
ls -la ~/.cache/ggen/packs/

# Check metadata
cat ~/.cache/ggen/packs/.cache-metadata

# Force refresh
ggen marketplace sync --force --verbose
```

### Corrupted Checksum

If checksum files are corrupted, force a full refresh:

```bash
ggen marketplace sync --force
```

Or manually clear cache:

```bash
rm -rf ~/.cache/ggen/packs/
ggen marketplace sync
```

### Custom Registry Issues

Test custom registry connectivity:

```bash
ggen marketplace sync --source https://custom.example.com --dry-run --verbose
```

Check that the registry URL is:
- Accessible from your network
- Returns valid package metadata
- Supports checksum verification

## Future Enhancements

- [ ] Parallel package downloads (multiple concurrent fetches)
- [ ] Bandwidth throttling
- [ ] Resume capability for large syncs
- [ ] Incremental metadata updates (delta sync)
- [ ] Cache compression
- [ ] Automated cache cleanup by age/size
- [ ] Network retry logic with exponential backoff

## Testing

Run marketplace sync tests:

```bash
cargo test -p ggen-cli-lib marketplace_sync
```

Test categories:
- Basic sync operation
- Force flag behavior
- Dry-run mode (cache isolation)
- Verbose output
- Custom registry URL
- Idempotency (running twice)
- JSON output compatibility
- Cache location override

See `crates/ggen-cli/tests/marketplace_sync_e2e.rs` for test implementation.

## References

- [Chicago TDD Testing](../testing/chicago-tdd.md)
- [Marketplace Architecture](./marketplace-architecture.md)
- [XDG Base Directory Specification](https://specifications.freedesktop.org/basedir-spec/)
