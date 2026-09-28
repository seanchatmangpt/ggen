//! Integration contract: provenance_chain.
use serde::{Deserialize,Serialize};
#[derive(Debug,Clone,Serialize,Deserialize,PartialEq,Eq)]struct Event{subject:String,policy:String,decision:String,epoch:u64,receipt:Option<String>}
#[derive(Default)]struct Trace{events:Vec<Event>}
impl Trace{
 fn admit(&mut self,subject:&str,policy:&str,epoch:u64){self.events.push(Event{subject:subject.into(),policy:policy.into(),decision:"admit".into(),epoch,receipt:Some(format!("{subject}:{policy}:{epoch}"))});}
 fn refuse(&mut self,subject:&str,policy:&str,epoch:u64){self.events.push(Event{subject:subject.into(),policy:policy.into(),decision:"refuse".into(),epoch,receipt:None});}
 fn admitted(&self,policy:&str)->bool{self.events.iter().any(|e|e.policy==policy&&e.decision=="admit")}
 fn complete(&self)->bool{["provenance","identity","evidence"].iter().all(|p|self.admitted(p))}
 fn subject_stable(&self)->bool{self.events.first().map(|e|self.events.iter().all(|x|x.subject==e.subject)).unwrap_or(true)}
 fn epochs_monotonic(&self)->bool{self.events.windows(2).all(|w|w[0].epoch<=w[1].epoch)}
 fn receipts_bound(&self)->bool{self.events.iter().filter(|e|e.decision=="admit").all(|e|e.receipt.as_ref().is_some_and(|r|r.contains(&e.subject)&&r.contains(&e.policy)))}
}
#[cfg(test)]mod tests{use super::*;
 #[test]fn complete_trace_requires_every_policy(){let mut t=Trace::default();t.admit("s","provenance",1);t.admit("s","identity",2);t.admit("s","evidence",3);assert!(t.complete());assert!(t.subject_stable());assert!(t.epochs_monotonic());assert!(t.receipts_bound());}
 #[test]fn refusal_does_not_fake_completion(){let mut t=Trace::default();t.refuse("s","provenance",1);assert!(!t.complete());}
 #[test]fn subject_drift_is_observable(){let mut t=Trace::default();t.admit("a","provenance",1);t.admit("b","identity",2);assert!(!t.subject_stable());}
 #[test]fn epoch_regression_is_observable(){let mut t=Trace::default();t.admit("s","provenance",2);t.admit("s","identity",1);assert!(!t.epochs_monotonic());}
}
