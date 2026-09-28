use serde::{Deserialize,Serialize};
#[derive(Clone,Debug,Serialize,Deserialize)] pub struct Consequence{pub name:String,pub reversible:bool,pub blast_radius:u32,pub cost:u64}
pub fn risk(c:&Consequence)->u64{(c.blast_radius as u64).saturating_mul(c.cost).saturating_mul(if c.reversible{1}else{10})}
pub fn safest(mut xs:Vec<Consequence>)->Vec<Consequence>{xs.sort_by_key(risk);xs}