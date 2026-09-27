#[derive(Clone,Copy,Debug,PartialEq,Eq)]
pub enum ProbePhase { Bind, Queue, Manufacture, SparseCheckpoint }
pub fn phase(cycle:u64)->ProbePhase {
 match cycle { 0..=5=>ProbePhase::Bind, 6..=15=>ProbePhase::Queue,
 c if c>=20 && c%20==0=>ProbePhase::SparseCheckpoint, _=>ProbePhase::Manufacture }
}
pub fn should_sample_slack(cycle:u64,last:Option<u64>)->bool {
 cycle>=20 && last.map_or(true,|l|cycle.saturating_sub(l)>=20)
}
