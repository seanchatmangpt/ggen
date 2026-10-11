# ggen-abb-sbb

IO-free ABB/SBB manufacture admission kernel: SELECT an existing SBB
(small black box) or MANUFACTURE a missing realization — never DO.

Exposes `admit()`, `plan()`, and `depgraph()` (transitive dependency
closure with cycle and unbound-port refusals). Integrated into the ggen
workspace in v26.10.9 and wired as `ggen-engine`'s Stage 2c admission
gate.

Reference: `docs/rfc/v26.9.26/abb-sbb-implementation.md` in the
[workspace repository](https://github.com/seanchatmangpt/ggen).

Licensed under the workspace MIT license (`LICENSE` at the repository
root).
