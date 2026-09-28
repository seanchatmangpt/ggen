use serde::{Deserialize,Serialize};
#[derive(Clone,Debug,Serialize,Deserialize,PartialEq,Eq,Hash)] pub enum Capability{Observe,Select,Construct,Actuate,Publish}
#[derive(Clone,Debug,Serialize,Deserialize)] pub struct Grant{pub principal:String,pub capability:Capability,pub scope:String}
#[derive(Clone,Debug,Default,Serialize,Deserialize)] pub struct AuthoritySet{pub grants:Vec<Grant>}
impl AuthoritySet{pub fn allows(&self,p:&str,c:&Capability,s:&str)->bool{self.grants.iter().any(|g|g.principal==p&&&g.capability==c&&(g.scope=="*"||s.starts_with(&g.scope)))} pub fn require(&self,p:&str,c:Capability,s:&str)->Result<(),String>{if self.allows(p,&c,s){Ok(())}else{Err(format!("authority denied: {p:?} {c:?} {s}"))}}}