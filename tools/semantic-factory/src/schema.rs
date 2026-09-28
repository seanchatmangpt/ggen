//! Typed schema admission edge for the DfCM semantic factory.
#[derive(Debug,Clone,PartialEq,Eq)]
pub struct Schema{pub schema_id:String,pub expected:String}
impl Schema{
 pub fn admits(&self)->bool{self.schema_id == self.expected}
 pub const fn facet()->&'static str{"schema"}
 pub fn receipt_fragment(&self,subject:&str)->String{format!("{}|{}|{}",Self::facet(),subject,self.admits())}
}
#[cfg(test)] mod tests{use super::*;#[test] fn stable_name(){assert_eq!(Schema::facet(),"schema");}}
