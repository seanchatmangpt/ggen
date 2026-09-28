//! Typed provenance admission edge for the DfCM semantic factory.
#[derive(Debug,Clone,PartialEq,Eq)]
pub struct Provenance{pub source:String}
impl Provenance{
 pub fn admits(&self)->bool{!self.source.is_empty()}
 pub const fn facet()->&'static str{"provenance"}
 pub fn receipt_fragment(&self,subject:&str)->String{format!("{}|{}|{}",Self::facet(),subject,self.admits())}
}
#[cfg(test)] mod tests{use super::*;#[test] fn stable_name(){assert_eq!(Provenance::facet(),"provenance");}}
