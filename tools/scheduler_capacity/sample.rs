pub struct CapacitySample { pub cycle:u64, pub useful_batches:u64, pub github_ops:u64, pub notion_ops:u64, pub slack_ops:u64 }
impl CapacitySample {
 pub fn lower_bound(&self, previous:u64)->u64 { previous.max(self.cycle) }
 pub fn external_ops(&self)->u64 { self.github_ops+self.notion_ops+self.slack_ops }
}
pub fn censored_max(samples:&[CapacitySample], previous:u64)->u64 {
 samples.iter().fold(previous, |m,s| m.max(s.cycle))
}
