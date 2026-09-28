use serde::{Deserialize,Serialize};
#[derive(Clone,Copy,Debug,Serialize,Deserialize,PartialEq,Eq,PartialOrd,Ord)] pub enum Phase{Observe,Select,Construct,Do,Receipt,Standing}
#[derive(Clone,Debug,Serialize,Deserialize)] pub struct Transition{pub from:Phase,pub to:Phase,pub reason:String}
pub fn lawful(t:&Transition)->bool{use Phase::*;matches!((t.from,t.to),(Observe,Select)|(Select,Construct)|(Construct,Do)|(Do,Receipt)|(Receipt,Standing))&&!t.reason.trim().is_empty()}