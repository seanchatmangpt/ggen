# W3: Script Adequacy

Witness/verifier binaries must implement strict error paths, handle validation failures, and exit with non-zero codes on failure, verified by `script_adequacy.json`.
No stub or success-bypass paths are allowed.
