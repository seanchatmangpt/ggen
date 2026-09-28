//! Edge-local FOND routing: failure removes one edge, never the whole graph.
use std::collections::{BTreeMap,BTreeSet};
#[derive(Debug,Clone,PartialEq,Eq,PartialOrd,Ord)] pub enum Edge{GitData,GitContents,NotionRows,NotionPages,SlackSend,SlackEdit}
#[derive(Debug,Clone,PartialEq,Eq)] pub enum Outcome{Success,Refused,RateLimited,Error}
#[derive(Default)] pub struct Router{disabled:BTreeSet<Edge>,observed:BTreeMap<Edge,Vec<Outcome>>}
impl Router{
 pub fn record(&mut self,e:Edge,o:Outcome){if o!=Outcome::Success{self.disabled.insert(e.clone());}self.observed.entry(e).or_default().push(o);}
 pub fn available(&self,e:&Edge)->bool{!self.disabled.contains(e)}
 pub fn choose<'a>(&self,candidates:&'a[Edge])->Option<&'a Edge>{candidates.iter().find(|e|self.available(e))}
 pub fn failures(&self)->usize{self.disabled.len()}
 pub fn observations(&self,e:&Edge)->usize{self.observed.get(e).map(Vec::len).unwrap_or(0)}
}
#[cfg(test)] mod tests{use super::*;#[test] fn failure_is_local(){let mut r=Router::default();r.record(Edge::SlackSend,Outcome::Refused);assert!(r.available(&Edge::GitData));assert!(!r.available(&Edge::SlackSend));}}
