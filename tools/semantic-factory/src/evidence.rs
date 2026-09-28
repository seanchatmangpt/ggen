//! Typed evidence admission edge for the DfCM semantic factory.
#[derive(Debug,Clone,PartialEq,Eq)]
pub struct Evidence{pub weight:u32}
impl Evidence{
 pub fn admits(&self)->bool{self.weight > 0}
 pub const fn facet()->&'static str{"evidence"}
 pub fn receipt_fragment(&self,subject:&str)->String{format!("{}|{}|{}",Self::facet(),subject,self.admits())}
}
#[cfg(test)] mod tests{use super::*;#[test] fn stable_name(){assert_eq!(Evidence::facet(),"evidence");}}
