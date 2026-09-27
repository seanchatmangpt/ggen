#[derive(Clone,Copy,Debug,Default)]
pub struct SurfaceBudget { pub cycles:u64, pub mutations:u64, pub reads:u64, pub throttles:u64 }
impl SurfaceBudget {
 pub fn observe_cycle(mut self)->Self { self.cycles+=1; self }
 pub fn observe_read(mut self)->Self { self.reads+=1; self }
 pub fn observe_mutation(mut self)->Self { self.mutations+=1; self }
 pub fn observe_throttle(mut self)->Self { self.throttles+=1; self }
 pub fn scheduler_bound(self)->u64 { self.cycles }
 pub fn tool_pressure(self)->u64 { self.reads+self.mutations }
}
pub fn independent_scheduler_signal(b:SurfaceBudget)->bool {
 b.cycles>0 && (b.throttles==0 || b.cycles>b.throttles)
}
