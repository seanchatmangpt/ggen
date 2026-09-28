//! Typed authority admission edge for the DfCM semantic factory.
#[derive(Debug,Clone,PartialEq,Eq)]
pub struct Authority{pub granted:bool}
impl Authority{
 pub fn admits(&self)->bool{self.granted}
 pub const fn facet()->&'static str{"authority"}
 pub fn receipt_fragment(&self,subject:&str)->String{format!("{}|{}|{}",Self::facet(),subject,self.admits())}
}
#[cfg(test)] mod tests{use super::*;#[test] fn stable_name(){assert_eq!(Authority::facet(),"authority");}}
