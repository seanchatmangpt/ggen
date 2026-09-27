#[derive(Clone,Debug)]
pub struct Checkpoint { pub cycle:u64, pub transport:String, pub durable:bool, pub identity:Option<String> }
#[derive(Default)]
pub struct Checkpoints { pub items:Vec<Checkpoint> }
impl Checkpoints {
 pub fn record(&mut self,c:Checkpoint){self.items.push(c)}
 pub fn latest_durable(&self)->Option<&Checkpoint>{self.items.iter().rev().find(|c|c.durable)}
 pub fn durable_count(&self)->usize{self.items.iter().filter(|c|c.durable).count()}
 pub fn next_due(&self,cycle:u64,interval:u64)->bool{interval>0 && cycle%interval==0}
}
