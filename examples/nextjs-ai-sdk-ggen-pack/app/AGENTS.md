# Agent Operating Law

1. Edit ontology or templates for projection changes; do not patch projected files without changing their owner.
2. Keep tool execution behind `ToolBroker`.
3. A mutating tool requires `needsApproval: true` and an integration receipt.
4. Preserve explicit statuses: `ALIVE`, `PARTIAL_ALIVE`, `BLOCKED`, `BUILD_BROKEN`, `UNKNOWN`, `UNSUPPORTED`.
5. Run the verification ladder before raising the claim ceiling.
6. A second `ggen sync run` must be byte-identical when admitted inputs are unchanged.
