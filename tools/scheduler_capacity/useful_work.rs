#[derive(Clone,Copy,Debug,Default)]
pub struct UsefulWork { pub batches:u64, pub files:u64, pub bytes:u64, pub lines:u64 }
impl UsefulWork {
 pub fn add(mut self, files:u64, bytes:u64, lines:u64)->Self {
  self.batches+=1; self.files+=files; self.bytes+=bytes; self.lines+=lines; self
 }
 pub fn density(&self,cycles:u64)->f64 {
  if cycles==0 {0.0} else {self.lines as f64/cycles as f64}
 }
}
