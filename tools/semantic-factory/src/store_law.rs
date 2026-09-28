//! Store-law classifier keeps control, artifact, history, and event truth distinct.
#[derive(Debug,Clone,Copy,PartialEq,Eq)] pub enum Store{Notion,GitHub,Ocel,Slack}
#[derive(Debug,Clone,Copy,PartialEq,Eq)] pub enum Truth{Control,Artifact,History,Event}
pub const fn canonical_for(t:Truth)->Store{match t{Truth::Control=>Store::Notion,Truth::Artifact=>Store::GitHub,Truth::History=>Store::Ocel,Truth::Event=>Store::Slack}}
pub const fn proves(store:Store,truth:Truth)->bool{matches!((store,truth),(Store::Notion,Truth::Control)|(Store::GitHub,Truth::Artifact)|(Store::Ocel,Truth::History)|(Store::Slack,Truth::Event))}
#[cfg(test)] mod tests{use super::*;#[test] fn slack_does_not_prove_artifact(){assert!(!proves(Store::Slack,Truth::Artifact));assert_eq!(canonical_for(Truth::Artifact),Store::GitHub);}}
