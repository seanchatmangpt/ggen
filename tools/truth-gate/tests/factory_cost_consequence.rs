//! Stateful integration harness for cost_consequence.
#[derive(Debug,Clone,PartialEq,Eq)]struct Step{policy:&'static str,subject:String,ok:bool,sequence:u64}
#[derive(Default)]struct Harness{steps:Vec<Step>}
impl Harness{
 fn push(&mut self,policy:&'static str,subject:&str,ok:bool){let sequence=self.steps.len() as u64+1;self.steps.push(Step{policy,subject:subject.into(),ok,sequence});}
 fn policies(&self)->Vec<&'static str>{self.steps.iter().map(|s|s.policy).collect()}
 fn all_ok(&self)->bool{self.steps.iter().all(|s|s.ok)}
 fn exact_subject(&self)->bool{self.steps.first().map(|f|self.steps.iter().all(|s|s.subject==f.subject)).unwrap_or(true)}
 fn ordered(&self)->bool{self.steps.windows(2).all(|w|w[0].sequence<w[1].sequence)}
 fn expected_path(&self)->bool{self.policies()==vec!["cost","consequence","authority"]}
 fn admitted(&self)->bool{self.all_ok()&&self.exact_subject()&&self.ordered()&&self.expected_path()}
}
#[cfg(test)]mod tests{use super::*;
 fn good()->Harness{let mut h=Harness::default();h.push("cost","repo@sha",true);h.push("consequence","repo@sha",true);h.push("authority","repo@sha",true);h}
 #[test]fn vertical_path_admits(){assert!(good().admitted());}
 #[test]fn failed_step_refuses(){let mut h=good();h.steps[1].ok=false;assert!(!h.admitted());}
 #[test]fn subject_drift_refuses(){let mut h=good();h.steps[2].subject="other".into();assert!(!h.admitted());}
 #[test]fn missing_step_refuses(){let mut h=good();h.steps.pop();assert!(!h.admitted());}
 #[test]fn order_is_receiptable(){let h=good();assert_eq!(h.steps.iter().map(|s|s.sequence).collect::<Vec<_>>(),vec![1,2,3]);}
}
