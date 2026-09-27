#[derive(Clone,Copy,Debug,PartialEq,Eq)]
pub enum Termination { SchedulerObserved, BudgetCensored, UsefulWorkExhausted, SurfacesUnavailable, Unknown }
pub struct RunEnvelope { pub highest_cycle:u64, pub previous_bound:u64, pub terminal_observed:bool }
impl RunEnvelope {
 pub fn lower_bound(&self)->u64 { self.previous_bound.max(self.highest_cycle) }
 pub fn classify(&self)->Termination {
   if self.terminal_observed { Termination::SchedulerObserved } else { Termination::BudgetCensored }
 }
 pub fn hard_max_supported(&self)->bool { self.terminal_observed && self.highest_cycle>=self.previous_bound }
}
