use serde::{Deserialize,Serialize};
#[derive(Clone,Copy,Debug,Serialize,Deserialize,PartialEq,Eq,PartialOrd,Ord)]pub struct Version{pub major:u32,pub minor:u32,pub patch:u32}
#[derive(Clone,Copy,Debug,PartialEq,Eq)]pub enum Change{Patch,Compatible,Breaking}
pub fn classify(old:Version,new:Version)->Result<Change,String>{if new<old{return Err("version regression".into())}if new.major>old.major{Ok(Change::Breaking)}else if new.minor>old.minor{Ok(Change::Compatible)}else{Ok(Change::Patch)}}