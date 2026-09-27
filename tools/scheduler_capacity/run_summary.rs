#[derive(Default)]
pub struct RunSummary { pub cycle:u64, pub code_batches:u64, pub github_ops:u64, pub notion_ops:u64, pub slack_ops:u64 }
impl RunSummary {
 pub fn observe_code(&mut self){self.cycle+=1;self.code_batches+=1}
 pub fn observe_github(&mut self){self.cycle+=1;self.github_ops+=1}
 pub fn observe_notion(&mut self){self.cycle+=1;self.notion_ops+=1}
 pub fn observe_slack(&mut self){self.cycle+=1;self.slack_ops+=1}
 pub fn lower_bound(&self,previous:u64)->u64{previous.max(self.cycle)}
}
