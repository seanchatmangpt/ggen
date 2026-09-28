//! Typed selector admission edge for the DfCM semantic factory.
#[derive(Debug,Clone,PartialEq,Eq)]
pub struct Selector{pub selected:bool,pub constructed:bool}
impl Selector{
 pub fn admits(&self)->bool{self.selected && !self.constructed}
 pub const fn facet()->&'static str{"selector"}
 pub fn receipt_fragment(&self,subject:&str)->String{format!("{}|{}|{}",Self::facet(),subject,self.admits())}
}
#[cfg(test)] mod tests{use super::*;#[test] fn stable_name(){assert_eq!(Selector::facet(),"selector");}}
