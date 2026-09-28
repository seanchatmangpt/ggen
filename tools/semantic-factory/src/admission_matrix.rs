//! Composable admission matrix for exact-subject decisions.
#[derive(Debug,Clone)] pub struct Check{pub name:&'static str,pub passed:bool,pub detail:String}
#[derive(Debug,Clone)] pub struct Matrix{pub subject:String,pub checks:Vec<Check>}
impl Matrix{
 pub fn new(subject:impl Into<String>)->Self{Self{subject:subject.into(),checks:Vec::new()}}
 pub fn check(mut self,name:&'static str,passed:bool,detail:impl Into<String>)->Self{self.checks.push(Check{name,passed,detail:detail.into()});self}
 pub fn admitted(&self)->bool{!self.subject.is_empty()&&!self.checks.is_empty()&&self.checks.iter().all(|x|x.passed)}
 pub fn failures(&self)->impl Iterator<Item=&Check>{self.checks.iter().filter(|x|!x.passed)}
 pub fn receipt(&self)->String{let failed=self.failures().map(|x|x.name).collect::<Vec<_>>().join(",");format!("subject={};admitted={};failed={}",self.subject,self.admitted(),failed)}
}
#[cfg(test)] mod tests{use super::*;#[test] fn one_failure_refuses(){let m=Matrix::new("s").check("identity",true,"ok").check("authority",false,"none");assert!(!m.admitted());assert_eq!(m.failures().count(),1);}}
