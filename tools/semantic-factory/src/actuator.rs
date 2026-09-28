//! Typed actuator admission edge for the DfCM semantic factory.
#[derive(Debug,Clone,PartialEq,Eq)]
pub struct Actuator{pub authorized:bool,pub actuated:bool}
impl Actuator{
 pub fn admits(&self)->bool{self.authorized && self.actuated}
 pub const fn facet()->&'static str{"actuator"}
 pub fn receipt_fragment(&self,subject:&str)->String{format!("{}|{}|{}",Self::facet(),subject,self.admits())}
}
#[cfg(test)] mod tests{use super::*;#[test] fn stable_name(){assert_eq!(Actuator::facet(),"actuator");}}
