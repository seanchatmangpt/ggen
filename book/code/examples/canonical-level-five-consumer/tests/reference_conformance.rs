use canonical_level_five_consumer::{Capability, CAPABILITIES};

#[test]
fn reference_contract_is_preserved() {
    assert_eq!(CAPABILITIES, &[
        Capability { name: "alpha", code: 10 },
        Capability { name: "beta", code: 20 },
        Capability { name: "gamma", code: 30 },
    ]);
}
