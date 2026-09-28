use serde::{Deserialize,Serialize};
#[derive(Clone,Debug,Serialize,Deserialize)]pub struct Consumer{pub name:String,pub contract:String,pub required_standing:String,pub scopes:Vec<String>}
#[derive(Clone,Debug,Default)]pub struct ConsumerRegistry{pub consumers:Vec<Consumer>}
impl ConsumerRegistry{pub fn register(&mut self,c:Consumer)->Result<(),String>{if self.consumers.iter().any(|x|x.name==c.name){return Err("duplicate consumer".into())}self.consumers.push(c);Ok(())}pub fn for_contract(&self,c:&str)->Vec<&Consumer>{self.consumers.iter().filter(|x|x.contract==c).collect()}}