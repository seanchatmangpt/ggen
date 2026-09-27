#[derive(Clone,Copy,Debug,PartialEq,Eq)]
pub enum EdgeOutcome { Success, LocalFailure, ToolFailure, Refused, RateLimited, AuthMissing }
#[derive(Clone,Copy,Debug,PartialEq,Eq)]
pub enum Route { Continue, RepairThenContinue, RemoveEdgeThenContinue }
pub fn route(o:EdgeOutcome)->Route { match o {
 EdgeOutcome::Success=>Route::Continue,
 EdgeOutcome::LocalFailure=>Route::RepairThenContinue,
 EdgeOutcome::ToolFailure|EdgeOutcome::Refused|EdgeOutcome::RateLimited|EdgeOutcome::AuthMissing=>Route::RemoveEdgeThenContinue,
}}
