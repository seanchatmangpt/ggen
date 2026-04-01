These files are not Cargo integration tests (only tests/*.rs at this directory's parent are built).
They were moved here because they targeted an older AtomicPackId / Bundle / Ownership API.
Restore and rewrite against crates/ggen-marketplace/src/{atomic,bundle,ownership}.rs when desired.
