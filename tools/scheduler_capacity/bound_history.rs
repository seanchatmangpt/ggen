pub struct BoundHistory { pub max_observed:u64 }
impl BoundHistory {
 pub fn new(previous:u64)->Self{Self{max_observed:previous}}
 pub fn admit_censored_run(&mut self,observed:u64){self.max_observed=self.max_observed.max(observed)}
 pub fn hard_max(&self)->Option<u64>{None}
}
pub fn crossed(observed:u64,threshold:u64)->bool{observed>=threshold}
pub fn milestones(observed:u64)->Vec<u64>{(100..=observed).step_by(20).collect()}
