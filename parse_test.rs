use oxigraph::io::{GraphFormat, GraphParser};
use std::fs::File;
use std::io::BufReader;

fn main() {
    let walker = walkdir::WalkDir::new(".");
    for entry in walker.into_iter().filter_map(|e| e.ok()) {
        let path = entry.path();
        if path.extension().and_then(|s| s.to_str()) == Some("ttl") {
            let file = match File::open(path) {
                Ok(f) => f,
                Err(_) => continue,
            };
            let mut parser = GraphParser::from_format(GraphFormat::Turtle).read_triples(BufReader::new(file)).expect("Reader");
            while let Some(res) = parser.next() {
                if let Err(e) = res {
                    println!("Failed {}: {}", path.display(), e);
                    break;
                }
            }
        }
    }
}
