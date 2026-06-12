## Challenge Summary

**Overall risk assessment**: MEDIUM

## Challenges

### [Medium] Challenge 1: Lack of Untracked File Verification (Bypass by Injection of New Files)

- Assumption challenged: Verifying that all files listed in the archive `pack.dat` match their corresponding files on disk is sufficient to guarantee cache integrity.
- Attack scenario: An attacker injects a new, malicious file (e.g. a script, configuration file, or executable) directly into the cache directory. Because the cache verification logic in `verify_digest` only iterates over the entries inside `pack.dat`, it does not check if there are extra untracked files in the cache directory. If the application runtime or consumer walks the cache directory or imports files from it dynamically, it will load the malicious file, bypassing the cache verification.
- Blast radius: MEDIUM (depends on whether the consumer walks the directory or loads files dynamically).
- Mitigation: Perform a directory walk of `pack.cache_path` (excluding `pack.dat`) and ensure that every file found on disk is present in the archive's entry list (e.g., by matching against a set of valid archive paths).

### [Low] Challenge 2: Incomplete Entry Type Whitelisting in Tar Extraction

- Assumption challenged: Blocking only `Symlink` and `Link` entry types in the TAR extractor is sufficient to prevent all dangerous/malicious entries.
- Attack scenario: A malicious archive contains special UNIX file types like character devices (`EntryType::Char`), block devices (`EntryType::Block`), or named pipes/FIFOs (`EntryType::Fifo`). If extracted, these entries could block the process indefinitely (e.g., reading from a FIFO) or create device nodes if the process runs with elevated privileges.
- Blast radius: LOW to MEDIUM.
- Mitigation: Implement a strict whitelist of allowed entry types. Only allow `EntryType::Regular`, `EntryType::Directory`, and internal tar metadata types (e.g., `GNULongName`, `GNULongLink`, `GNUSparse`, `XHeader`, `XGlobalHeader`), and reject all others.

### [Low] Challenge 3: Path Resolution Escape in Cache Verification

- Assumption challenged: Checking for `ParentDir` (`..`) components is sufficient to prevent path traversal when checking file paths during cache verification.
- Attack scenario: An archive in `pack.dat` contains an entry with an absolute path (e.g., `/etc/passwd`). In Rust, joining an absolute path to a base path (e.g., `pack.cache_path.join("/etc/passwd")`) discards the base path and returns the absolute path directly. The check for `ParentDir` does not catch this, and the verification code will attempt to read the file from the host filesystem outside the cache directory.
- Blast radius: LOW (read-only file boundary escape).
- Mitigation: Add a check to verify that `disk_path.starts_with(&pack.cache_path)` is true before reading any file from disk during verification.

## Stress Test Results

- Verify refined Zip Slip prevention in TAR extraction → Extraction fails with `Error::InstallationFailed` → Symlink traversal blocked and no files written outside extraction directory → PASS
- Verify refined Cache Verification after tampering → Cache verification fails when an extracted file is modified on disk → Tampered files successfully detected → PASS
- Verify Cache Verification with injected new file → Cache verification succeeds despite the presence of an extra untracked file → Untracked file injection not detected → FAIL (Loophole confirmed)
- Verify Cache Verification with absolute paths → Cache verification attempts to resolve and read absolute paths outside the cache directory → Path resolution escape confirmed → FAIL (Loophole confirmed)

## Unchallenged Areas

- ZIP Symlink Extraction — not challenged because the ZIP extraction code does not support or extract symlinks/hardlinks, meaning ZIP symlinks are treated as regular files or folders and cannot escape the target directory.
