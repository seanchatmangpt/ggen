mod policy {
    pub mod test_policy;
    pub mod evidence_policy;
    pub mod config_policy;
}

use policy::test_policy;

fn main() {
    let content = "from unittest.mock import patch\n";
    let file_path = "/tmp/test.py";
    let violations = test_policy::check(content, file_path);
    println!("Found {} violations", violations.len());
    for v in &violations {
        println!("  - {} at {}", v.pattern, v.location);
    }
}
