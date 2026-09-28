//! Typed serializer admission edge for the DfCM semantic factory.
#[derive(Debug,Clone,PartialEq,Eq)]
pub struct Serializer{pub serialized:bool}
impl Serializer{
 pub fn admits(&self)->bool{self.serialized}
 pub const fn facet()->&'static str{"serializer"}
 pub fn receipt_fragment(&self,subject:&str)->String{format!("{}|{}|{}",Self::facet(),subject,self.admits())}
}
#[cfg(test)] mod tests{use super::*;#[test] fn stable_name(){assert_eq!(Serializer::facet(),"serializer");}}
