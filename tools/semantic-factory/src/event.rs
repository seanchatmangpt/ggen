//! event contract.
#[derive(Debug,Clone,PartialEq,Eq)] pub struct Event{pub event_id:String}
impl Event{pub fn admits(&self)->bool{!self.event_id.is_empty()} pub const fn facet()->&'static str{"event"} pub fn receipt_fragment(&self,s:&str)->String{format!("{}|{}|{}",Self::facet(),s,self.admits())}}
#[cfg(test)] mod tests{use super::*;#[test] fn name(){assert_eq!(Event::facet(),"event");}}
