//! Deterministic receipt chain independent of external standing.
#[derive(Debug,Clone,PartialEq,Eq)] pub struct Receipt{pub id:String,pub subject:String,pub parent:Option<String>,pub operation:String,pub admitted:bool}
#[derive(Default)] pub struct Chain{items:Vec<Receipt>}
impl Chain{
 pub fn append(&mut self,r:Receipt)->Result<(),String>{
  if r.id.is_empty()||r.subject.is_empty(){return Err("identity-required".into());}
  if self.items.iter().any(|x|x.id==r.id){return Err("duplicate-receipt".into());}
  if let Some(p)=&r.parent{if !self.items.iter().any(|x|&x.id==p){return Err("parent-missing".into());}}
  self.items.push(r);Ok(())
 }
 pub fn replay(&self)->impl Iterator<Item=&Receipt>{self.items.iter()}
 pub fn last(&self)->Option<&Receipt>{self.items.last()}
 pub fn len(&self)->usize{self.items.len()}
 pub fn is_empty(&self)->bool{self.items.is_empty()}
}
#[cfg(test)] mod tests{use super::*;#[test] fn rejects_orphan(){let mut c=Chain::default();assert!(c.append(Receipt{id:"2".into(),subject:"s".into(),parent:Some("1".into()),operation:"x".into(),admitted:true}).is_err());}}
