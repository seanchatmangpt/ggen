//! planner contract.
#[derive(Debug,Clone,PartialEq,Eq)] pub struct Planner{pub planned:bool,pub authorized:bool}
impl Planner{pub fn admits(&self)->bool{self.planned && !self.authorized} pub const fn facet()->&'static str{"planner"} pub fn receipt_fragment(&self,s:&str)->String{format!("{}|{}|{}",Self::facet(),s,self.admits())}}
#[cfg(test)] mod tests{use super::*;#[test] fn name(){assert_eq!(Planner::facet(),"planner");}}
