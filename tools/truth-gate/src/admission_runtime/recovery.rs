use serde::{Deserialize,Serialize};
#[derive(Clone,Debug,Serialize,Deserialize,PartialEq,Eq)] pub enum FailureClass{Local,Tool,Refused,RateLimit,AuthMissing,Truncated}
#[derive(Clone,Debug,Serialize,Deserialize)] pub enum Recovery{Repair{edge:String},RemoveEdge{edge:String},Route{from:String,to:String},Stop{reason:String}}
pub fn decide(edge:&str,class:FailureClass,alternate:Option<&str>)->Recovery{match class{FailureClass::Local=>Recovery::Repair{edge:edge.into()},_=>alternate.map(|a|Recovery::Route{from:edge.into(),to:a.into()}).unwrap_or_else(||Recovery::RemoveEdge{edge:edge.into()})}}