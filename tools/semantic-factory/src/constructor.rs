//! Typed constructor admission edge for the DfCM semantic factory.
#[derive(Debug,Clone,PartialEq,Eq)]
pub struct Constructor{pub constructed:bool,pub actuated:bool}
impl Constructor{
 pub fn admits(&self)->bool{self.constructed && !self.actuated}
 pub const fn facet()->&'static str{"constructor"}
 pub fn receipt_fragment(&self,subject:&str)->String{format!("{}|{}|{}",Self::facet(),subject,self.admits())}
}
#[cfg(test)] mod tests{use super::*;#[test] fn stable_name(){assert_eq!(Constructor::facet(),"constructor");}}
