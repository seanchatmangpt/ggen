/// Basic SPARQL CLI Examples
///
/// Demonstrates basic query execution, optimization, and federation

use anyhow::Result;

fn main() -> Result<()> {
    println!("SPARQL CLI - Basic Examples\n");

    // Example 1: Simple SELECT query
    println!("Example 1: Simple SELECT Query");
    println!("-------------------------------");
    let query = r#"
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>
        SELECT ?name ?email
        WHERE {
            ?person foaf:name ?name .
            ?person foaf:mbox ?email
        }
        LIMIT 10
    "#;
    println!("Query:\n{}", query);
    println!("Command: sparql-cli query execute --query '{}' --endpoint http://dbpedia.org/sparql\n", query);

    // Example 2: ASK query
    println!("Example 2: ASK Query");
    println!("--------------------");
    let ask_query = r#"
        ASK {
            ?person a foaf:Person .
            ?person foaf:name "Albert Einstein"
        }
    "#;
    println!("Query:\n{}", ask_query);
    println!("Command: sparql-cli query execute --query '{}' --endpoint http://dbpedia.org/sparql\n", ask_query);

    // Example 3: Query optimization
    println!("Example 3: Query Optimization");
    println!("-----------------------------");
    let complex_query = r#"
        SELECT ?person ?name ?birthDate
        WHERE {
            ?person a dbo:Person .
            ?person foaf:name ?name .
            ?person dbo:birthDate ?birthDate .
            FILTER(?birthDate > "1900-01-01"^^xsd:date)
        }
    "#;
    println!("Original query:\n{}", complex_query);
    println!("Command: sparql-cli optimization rewrite --query '{}' --level 3\n", complex_query);

    // Example 4: Federated query
    println!("Example 4: Federated Query");
    println!("---------------------------");
    let fed_query = r#"
        SELECT DISTINCT ?name
        WHERE {
            ?person foaf:name ?name .
            FILTER(CONTAINS(?name, "Einstein"))
        }
    "#;
    println!("Query:\n{}", fed_query);
    println!("Command: sparql-cli federation merge \\");
    println!("  --query '{}' \\", fed_query);
    println!("  --endpoints dbpedia,wikidata,yago \\");
    println!("  --strategy cost-based\n");

    // Example 5: Endpoint management
    println!("Example 5: Endpoint Management");
    println!("------------------------------");
    println!("Register endpoint:");
    println!("  sparql-cli endpoint register --name dbpedia --url http://dbpedia.org/sparql");
    println!("\nTest endpoint:");
    println!("  sparql-cli endpoint test --endpoint dbpedia");
    println!("\nBenchmark endpoint:");
    println!("  sparql-cli endpoint benchmark --endpoint dbpedia --iterations 100\n");

    // Example 6: Query explain
    println!("Example 6: Query Explain Plan");
    println!("------------------------------");
    println!("Get execution plan:");
    println!("  sparql-cli query explain --query '{}' --show-cost", complex_query);

    Ok(())
}
