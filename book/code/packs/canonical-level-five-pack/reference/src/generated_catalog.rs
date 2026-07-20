#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Capability { pub name: &'static str, pub code: u16 }
pub const CAPABILITIES: &[Capability] = &[
    Capability { name: "alpha", code: 10 },
    Capability { name: "beta", code: 20 },
    Capability { name: "gamma", code: 30 },
];
