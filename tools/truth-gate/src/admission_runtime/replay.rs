use serde::{Deserialize,Serialize}; use super::receipt::Receipt;
#[derive(Clone,Debug,Serialize,Deserialize)] pub struct ReplayResult{pub receipt_id:String,pub deterministic:bool,pub expected:String,pub observed:String}
pub trait Replayer{fn replay(&self,r:&Receipt)->Result<String,String>;}
pub fn verify<R:Replayer>(r:&R,x:&Receipt)->Result<ReplayResult,String>{let o=r.replay(x)?;Ok(ReplayResult{receipt_id:x.id.clone(),deterministic:o==x.output_digest,expected:x.output_digest.clone(),observed:o})}