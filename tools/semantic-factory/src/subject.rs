//! Typed subject admission edge for the DfCM semantic factory.
#[derive(Debug,Clone,PartialEq,Eq)]
pub struct SubjectKey{pub subject:String,pub expected:String}
impl SubjectKey{
 pub fn admits(&self)->bool{self.subject == self.expected}
 pub const fn facet()->&'static str{"subject"}
 pub fn receipt_fragment(&self,subject:&str)->String{format!("{}|{}|{}",Self::facet(),subject,self.admits())}
}
#[cfg(test)] mod tests{use super::*;#[test] fn stable_name(){assert_eq!(SubjectKey::facet(),"subject");}}
