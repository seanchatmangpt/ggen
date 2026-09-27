use std::collections::BTreeMap;
#[derive(Default)]
pub struct EdgeStats { pub success:u64, pub refused:u64, pub throttled:u64, pub errors:u64 }
#[derive(Default)]
pub struct EdgeHistogram { pub edges:BTreeMap<String,EdgeStats> }
impl EdgeHistogram {
 pub fn success(&mut self,e:&str){self.edges.entry(e.into()).or_default().success+=1}
 pub fn refused(&mut self,e:&str){self.edges.entry(e.into()).or_default().refused+=1}
 pub fn throttled(&mut self,e:&str){self.edges.entry(e.into()).or_default().throttled+=1}
 pub fn error(&mut self,e:&str){self.edges.entry(e.into()).or_default().errors+=1}
 pub fn first_unavailable<'a>(&'a self)->Option<&'a str>{
  self.edges.iter().find(|(_,s)|s.refused+s.throttled+s.errors>0).map(|(e,_)|e.as_str())
 }
}
