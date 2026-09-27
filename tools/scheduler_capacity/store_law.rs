#[derive(Clone,Copy,Debug,PartialEq,Eq)]
pub enum Store { Notion, GitHub, Ocel, Slack }
pub fn canonical_for(store:Store, fact:&str)->bool {
 match store {
  Store::Notion => matches!(fact,"intent"|"dependency"|"decision"|"control"),
  Store::GitHub => matches!(fact,"artifact"|"code"),
  Store::Ocel => matches!(fact,"history"|"receipt"),
  Store::Slack => false,
 }
}
pub fn critical_path(store:Store)->bool { matches!(store,Store::GitHub) }
