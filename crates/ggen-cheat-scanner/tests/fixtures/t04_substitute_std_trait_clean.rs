//! Clean fixture: a Fake* type sharing only a ubiquitous std trait
//! (Default) with a production type is not a collaborator substitute.
pub struct RealSummary;
impl Default for RealSummary {
    fn default() -> Self {
        RealSummary
    }
}

pub struct FakeDataGenerator;
impl Default for FakeDataGenerator {
    fn default() -> Self {
        FakeDataGenerator
    }
}
