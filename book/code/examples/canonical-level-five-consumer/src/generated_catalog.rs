#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Capability { pub name: &'static str, pub code: u16 }

pub const CAPABILITIES: &[Capability] = &[
    Capability { name: "alpha", code: 10 },
    Capability { name: "beta", code: 20 },
    Capability { name: "gamma", code: 30 },
];

#[cfg(test)]
mod generated_proof {
    use super::*;
    #[test]
    fn independent_reference_values_survive_generation() {
        assert_eq!(CAPABILITIES.len(), 3);
        assert_eq!(CAPABILITIES[0], Capability { name: "alpha", code: 10 });
        assert_eq!(CAPABILITIES[2], Capability { name: "gamma", code: 30 });
    }
}
