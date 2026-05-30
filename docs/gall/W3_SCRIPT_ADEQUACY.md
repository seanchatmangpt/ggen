<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [W3: Script Adequacy](#w3-script-adequacy)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# W3: Script Adequacy

Witness/verifier binaries must implement strict error paths, handle validation failures, and exit with non-zero codes on failure, verified by `script_adequacy.json`.
No stub or success-bypass paths are allowed.
