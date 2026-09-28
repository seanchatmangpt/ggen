//! Typed repair admission edge for the DfCM semantic factory.
#[derive(Debug,Clone,PartialEq,Eq)]
pub struct Repair{pub changed:bool}
impl Repair{
 pub fn admits(&self)->bool{self.changed}
 pub const fn facet()->&'static str{"repair"}
 pub fn receipt_fragment(&self,subject:&str)->String{format!("{}|{}|{}",Self::facet(),subject,self.admits())}
}
#[cfg(test)] mod tests{use super::*;#[test] fn stable_name(){assert_eq!(Repair::facet(),"repair");}}
