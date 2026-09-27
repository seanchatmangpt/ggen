#[derive(Clone,Copy,Debug,PartialEq,Eq)]
pub enum FailureClass { Local, Tool, Refused, RateLimit, AuthMissing }
pub fn remove_edge_only(f:FailureClass)->bool {
 matches!(f,FailureClass::Tool|FailureClass::Refused|FailureClass::RateLimit|FailureClass::AuthMissing)
}
pub fn retry_identical(f:FailureClass)->bool { matches!(f,FailureClass::Local) }
pub fn stop_for_external_failure(_:FailureClass)->bool { false }
