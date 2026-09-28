use serde::{Deserialize,Serialize};
#[derive(Clone,Debug,Serialize,Deserialize)] pub struct Contract{pub name:String,pub major:u32,pub required:Vec<String>,pub optional:Vec<String>}
pub fn compatible(p:&Contract,c:&Contract)->bool{p.name==c.name&&p.major==c.major&&c.required.iter().all(|r|p.required.contains(r)||p.optional.contains(r))}
pub fn missing(p:&Contract,c:&Contract)->Vec<String>{c.required.iter().filter(|r|!p.required.contains(r)&&!p.optional.contains(r)).cloned().collect()}