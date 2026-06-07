## 2026-06-06T13:38:39-07:00
Write Tier 1 and Tier 2 E2E test cases for Feature 1 (Pack Descriptor & Dependency Resolution) and Feature 2 (Core Projection Maps & Staging Gate) under crates/ggen-projection/tests/.
- crates/ggen-projection/tests/f1_dependency_resolution.rs (10 test cases)
- crates/ggen-projection/tests/f2_projection_maps.rs (10 test cases)
Must be opaque-box, mock-free, using real structures and validation.
Verify using cargo test -p ggen-projection.
Write handoff.md and send message back.
