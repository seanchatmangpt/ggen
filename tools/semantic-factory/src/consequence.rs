//! Typed consequence admission edge for the DfCM semantic factory.
#[derive(Debug,Clone,PartialEq,Eq)]
pub struct Consequence{pub effect:String}
impl Consequence{
 pub fn admits(&self)->bool{!self.effect.is_empty()}
 pub const fn facet()->&'static str{"consequence"}
 pub fn receipt_fragment(&self,subject:&str)->String{format!("{}|{}|{}",Self::facet(),subject,self.admits())}
}
#[cfg(test)] mod tests{use super::*;#[test] fn stable_name(){assert_eq!(Consequence::facet(),"consequence");}}
