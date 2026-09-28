//! Typed route admission edge for the DfCM semantic factory.
#[derive(Debug,Clone,PartialEq,Eq)]
pub struct Route{pub lawful:bool}
impl Route{
 pub fn admits(&self)->bool{self.lawful}
 pub const fn facet()->&'static str{"route"}
 pub fn receipt_fragment(&self,subject:&str)->String{format!("{}|{}|{}",Self::facet(),subject,self.admits())}
}
#[cfg(test)] mod tests{use super::*;#[test] fn stable_name(){assert_eq!(Route::facet(),"route");}}
