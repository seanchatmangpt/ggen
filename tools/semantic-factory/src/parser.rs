//! Typed parser admission edge for the DfCM semantic factory.
#[derive(Debug,Clone,PartialEq,Eq)]
pub struct Parser{pub parsed:bool}
impl Parser{
 pub fn admits(&self)->bool{self.parsed}
 pub const fn facet()->&'static str{"parser"}
 pub fn receipt_fragment(&self,subject:&str)->String{format!("{}|{}|{}",Self::facet(),subject,self.admits())}
}
#[cfg(test)] mod tests{use super::*;#[test] fn stable_name(){assert_eq!(Parser::facet(),"parser");}}
