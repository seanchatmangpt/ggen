

/// The Frozen Canon Registry for Stewards of the Pentecost.
/// Every part must declare its canon basis.
pub trait CanonBasis {
    fn canon_basis(&self) -> &'static [&'static str];
}

pub const SCRIPTURE_ROMANS_15_7: &str = "Romans 15:7";
pub const SCRIPTURE_1_COR_4_2: &str = "1 Corinthians 4:2";
pub const SCRIPTURE_GAL_6_2: &str = "Galatians 6:2";

pub const AA_STEP_TWO: &str = "AA Step Two";
pub const AA_TRADITION_ONE: &str = "AA Tradition One";

pub const AVATAR_PAUL: &str = "Avatar: Paul (Order)";
pub const AVATAR_PETER: &str = "Avatar: Peter (Witness)";
