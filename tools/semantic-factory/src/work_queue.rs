//! Ranked useful-work queue for implementation-first runs.
use std::cmp::Ordering;
#[derive(Debug,Clone)] pub struct Work{pub id:String,pub leverage:u32,pub information_gain:u32,pub cost:u32,pub lawful:bool}
impl Work{pub fn score(&self)->f64{if !self.lawful{return f64::NEG_INFINITY;}((self.leverage as f64)*(self.information_gain as f64))/(self.cost.max(1) as f64)}}
pub fn rank(mut xs:Vec<Work>)->Vec<Work>{xs.sort_by(|a,b|b.score().partial_cmp(&a.score()).unwrap_or(Ordering::Equal));xs}
pub fn next(xs:&[Work])->Option<&Work>{xs.iter().filter(|x|x.lawful).max_by(|a,b|a.score().partial_cmp(&b.score()).unwrap_or(Ordering::Equal))}
#[cfg(test)] mod tests{use super::*;#[test] fn unlawful_never_wins(){let xs=vec![Work{id:"bad".into(),leverage:99,information_gain:99,cost:1,lawful:false},Work{id:"ok".into(),leverage:1,information_gain:1,cost:1,lawful:true}];assert_eq!(next(&xs).unwrap().id,"ok");}}
