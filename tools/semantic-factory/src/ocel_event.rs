//! Minimal OCEL-style process receipt for manufacturing events.
use std::collections::BTreeMap;
#[derive(Debug,Clone,PartialEq,Eq)] pub struct Event{pub id:String,pub activity:String,pub objects:Vec<String>,pub attributes:BTreeMap<String,String>}
impl Event{
 pub fn new(id:impl Into<String>,activity:impl Into<String>)->Self{Self{id:id.into(),activity:activity.into(),objects:Vec::new(),attributes:BTreeMap::new()}}
 pub fn object(mut self,id:impl Into<String>)->Self{self.objects.push(id.into());self}
 pub fn attr(mut self,k:impl Into<String>,v:impl Into<String>)->Self{self.attributes.insert(k.into(),v.into());self}
 pub fn valid(&self)->bool{!self.id.is_empty()&&!self.activity.is_empty()&&!self.objects.is_empty()}
 pub fn relates(&self,id:&str)->bool{self.objects.iter().any(|x|x==id)}
}
#[cfg(test)] mod tests{use super::*;#[test] fn event_needs_object(){assert!(!Event::new("e","construct").valid());assert!(Event::new("e","construct").object("artifact:a").valid());}}
