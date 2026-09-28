//! Typed generator admission edge for the DfCM semantic factory.
#[derive(Debug,Clone,PartialEq,Eq)]
pub struct Generator{pub generator_id:String,pub expected:String}
impl Generator{
 pub fn admits(&self)->bool{self.generator_id == self.expected}
 pub const fn facet()->&'static str{"generator"}
 pub fn receipt_fragment(&self,subject:&str)->String{format!("{}|{}|{}",Self::facet(),subject,self.admits())}
}
#[cfg(test)] mod tests{use super::*;#[test] fn stable_name(){assert_eq!(Generator::facet(),"generator");}}
