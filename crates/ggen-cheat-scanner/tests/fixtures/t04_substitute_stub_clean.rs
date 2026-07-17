// Clean fixture for CHEAT-T04's cross-file check: a pure trait-shape stub
// (mirroring bcinr-mfw-ir's pattern) where "MockOnly" is the ONLY
// implementer of `UniqueTrait` -- nothing to substitute for, so this must
// not be flagged.
trait UniqueTrait {
    fn value(&self) -> u32;
}

struct MockOnly;

impl UniqueTrait for MockOnly {
    fn value(&self) -> u32 {
        0
    }
}
