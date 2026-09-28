use serde::{Deserialize,Serialize};
#[derive(Clone,Copy,Debug,Serialize,Deserialize,PartialEq,Eq)] pub enum Store{GitHub,Notion,Ocel,Slack}
#[derive(Clone,Copy,Debug,Serialize,Deserialize,PartialEq,Eq)] pub enum Truth{Executable,Control,History,Transient}
pub fn admits(s:Store,t:Truth)->bool{matches!((s,t),(Store::GitHub,Truth::Executable)|(Store::Notion,Truth::Control)|(Store::Ocel,Truth::History)|(Store::Slack,Truth::Transient))}
pub fn require(s:Store,t:Truth)->Result<(),String>{if admits(s,t){Ok(())}else{Err(format!("store-law violation: {s:?} cannot admit {t:?}"))}}