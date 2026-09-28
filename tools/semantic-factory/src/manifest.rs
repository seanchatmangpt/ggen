//! manifest contract.
#[derive(Debug,Clone,PartialEq,Eq)] pub struct Manifest{pub entries:usize}
impl Manifest{pub fn admits(&self)->bool{self.entries > 0} pub const fn facet()->&'static str{"manifest"} pub fn receipt_fragment(&self,s:&str)->String{format!("{}|{}|{}",Self::facet(),s,self.admits())}}
#[cfg(test)] mod tests{use super::*;#[test] fn name(){assert_eq!(Manifest::facet(),"manifest");}}
