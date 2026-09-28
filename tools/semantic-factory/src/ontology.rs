//! ontology contract.
#[derive(Debug,Clone,PartialEq,Eq)] pub struct Ontology{pub canonical:bool}
impl Ontology{pub fn admits(&self)->bool{self.canonical} pub const fn facet()->&'static str{"ontology"} pub fn receipt_fragment(&self,s:&str)->String{format!("{}|{}|{}",Self::facet(),s,self.admits())}}
#[cfg(test)] mod tests{use super::*;#[test] fn name(){assert_eq!(Ontology::facet(),"ontology");}}
