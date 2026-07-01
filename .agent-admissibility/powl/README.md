# Repair routes (POWL)

Repair routes are POWL models (partial orders of steps with choice/loop). They are
PROMOTED from mined dominant failure edges — run `ggen lsp mine` after `ggen lsp check`
has accumulated agent-edit events, then review `../ocel/discovery/error-edge-mining.md`.
Seeded cold-start routes ship inside the `ggen-lsp` binary; mined routes land here.
