pub struct SparseSchedule { pub interval:u64, pub next:u64 }
impl SparseSchedule {
 pub fn new(interval:u64)->Self { Self{interval,next:interval} }
 pub fn due(&self,cycle:u64)->bool { cycle>=self.next }
 pub fn advance(&mut self,cycle:u64) {
  while self.next<=cycle { self.next=self.next.saturating_add(self.interval.max(1)); }
 }
}
pub fn external_failure_stops_probe(_: &str)->bool { false }
