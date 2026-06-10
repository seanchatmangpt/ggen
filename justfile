# Marketplace pack tests — fast gate (Layers 1+2, no network required)
test-marketplace:
    cargo test -p ggen-core --test lsp_max_pack_test
    cargo test -p ggen-core --test all_marketplace_packs_validation_test

# Full gate including compilation proof (requires crates.io network)
test-marketplace-full:
    cargo test -p ggen-core --test lsp_max_pack_test -- --include-ignored
    cargo test -p ggen-core --test all_marketplace_packs_validation_test
