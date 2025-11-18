# SPARQL Endpoint Setup Guide

This guide explains how to set up a SPARQL endpoint for the YAWL Editor application.

## Quick Start Options

### Option 1: Apache Jena Fuseki (Recommended for Development)

**Installation:**

```bash
# Download Fuseki
wget https://archive.apache.org/dist/jena/binaries/apache-jena-fuseki-4.10.0.tar.gz
tar -xzf apache-jena-fuseki-4.10.0.tar.gz
cd apache-jena-fuseki-4.10.0

# Start Fuseki with in-memory store
./fuseki-server --mem /yawl

# Fuseki will be available at: http://localhost:3030
```

**Load the YAWL Ontology:**

```bash
# From YAWL Editor directory
curl -X POST \
  -H "Content-Type: text/turtle" \
  --data-binary @ontology/yawl-workflow.ttl \
  http://localhost:3030/yawl/data
```

**Configuration:**

Update `.env.local`:

```
NEXT_PUBLIC_SPARQL_ENDPOINT=http://localhost:3030/yawl/query
NEXT_PUBLIC_SPARQL_GRAPH=http://localhost:3030/yawl/data
```

### Option 2: Docker with Fuseki

```bash
# Run Fuseki in Docker
docker run -p 3030:3030 -v $(pwd)/fuseki-data:/fuseki-data \
  atomgraph/fuseki:latest

# Load ontology
curl -X POST \
  -H "Content-Type: text/turtle" \
  --data-binary @ontology/yawl-workflow.ttl \
  http://localhost:3030/yawl/data
```

### Option 3: Virtuoso Open Source

**Installation:**

```bash
# Install Virtuoso (on Ubuntu/Debian)
sudo apt-get install virtuoso-opensource

# Start Virtuoso
virtuoso-t -f +configfile /etc/virtuoso/virtuoso.ini
```

**Load Ontology:**

```bash
# Use isql-v (interactive SQL client)
isql-v

# Then in isql:
SQL> DB.DBA.TTLP (file_to_string ('ontology/yawl-workflow.ttl'),
                   '',
                   'http://yawlfoundation.org/',
                   0);
```

**Configuration:**

```
NEXT_PUBLIC_SPARQL_ENDPOINT=http://localhost:8890/sparql
NEXT_PUBLIC_SPARQL_GRAPH=http://yawlfoundation.org/
```

### Option 4: GraphDB

**Docker Installation:**

```bash
# Run GraphDB
docker run -p 7200:7200 --name graphdb \
  ontotext/graphdb:10.0.0

# Access UI at: http://localhost:7200
```

**Setup via UI:**

1. Create repository: "yawl"
2. Go to Import > RDF
3. Upload `ontology/yawl-workflow.ttl`

**Configuration:**

```
NEXT_PUBLIC_SPARQL_ENDPOINT=http://localhost:7200/repositories/yawl
NEXT_PUBLIC_SPARQL_GRAPH=http://yawlfoundation.org/
```

## Testing Your SPARQL Connection

### Simple Query Test

```bash
# Test SPARQL endpoint connectivity
curl -H "Accept: application/sparql-results+json" \
  "http://localhost:3030/yawl/query?query=SELECT%20?s%20WHERE%20{%20?s%20?p%20?o%20}%20LIMIT%201"
```

### From the Application

The application includes a SPARQL client in `lib/sparql-client.ts`. You can test it:

```typescript
import { createDefaultSparqlClient } from '@/lib/sparql-client'
import { SPARQL_QUERIES } from '@/lib/sparql-queries'

const client = createDefaultSparqlClient()

// Test query
const result = await client.query(SPARQL_QUERIES.getAllProcesses)
console.log(result)
```

## Loading Sample Data

The `ontology/yawl-workflow.ttl` file contains just the schema. To add sample data:

```sparql
PREFIX ex: <http://yawlfoundation.org/ontology#>
PREFIX data: <http://yawlfoundation.org/data#>

INSERT DATA {
  data:process-001 a ex:Process ;
    ex:processIdentifier "proc-001" ;
    ex:processName "Loan Application" ;
    ex:processVersion "1.0" ;
    ex:processDescription "Application process for loans" .

  data:task-001 a ex:Task ;
    ex:taskName "Review Application" ;
    ex:belongsToProcess data:process-001 ;
    ex:taskType ex:ATOMIC .

  data:case-001 a ex:Case ;
    ex:caseID "case-001" ;
    ex:caseStatus "Running" ;
    ex:caseCreatedDate "2024-01-15T10:00:00Z"^^xsd:dateTime ;
    ex:instanceOfProcess data:process-001 .
}
```

## Troubleshooting

### Connection Refused

- Ensure SPARQL endpoint is running
- Check port number in `.env.local`
- Verify firewall allows localhost connections

### Ontology Not Loaded

```bash
# Check if data exists
curl "http://localhost:3030/yawl/query?query=SELECT%20?s%20WHERE%20{%20?s%20a%20?type%20}%20LIMIT%201"

# Should return at least one result if ontology is loaded
```

### Authentication Issues

If your SPARQL endpoint requires authentication:

1. Set `SPARQL_USERNAME` and `SPARQL_PASSWORD` in `.env.local`
2. Client will use HTTP Basic Auth automatically

### Performance Issues

For large datasets, enable indexes in your SPARQL store:

**Fuseki:**
- Use persistent store instead of in-memory
- Create appropriate graph URIs

**Virtuoso:**
- Run `EXEC sparql_rdf_doc_insert_queue(1)`
- Create full-text indexes

**GraphDB:**
- Create OWLIM-SE repository with GraphDB Inferences
- Enable predicate lists

## SPARQL Endpoint Comparison

| Feature | Fuseki | Virtuoso | GraphDB |
|---------|--------|----------|---------|
| Setup Difficulty | Easy | Medium | Easy |
| Memory Usage | Low | High | Medium |
| Query Performance | Good | Excellent | Excellent |
| SPARQL 1.1 | Yes | Yes | Yes |
| Free | Yes | Yes (Opensource) | Yes (Community) |
| Docker Support | Yes | Yes | Yes |
| UI | Web UI | iSQL | Web UI |
| Scalability | Medium | High | High |

## Production Deployment

### Recommended Setup

1. **Database**: GraphDB or Virtuoso
2. **Server**: Ubuntu 20.04 LTS
3. **Memory**: 4GB minimum, 16GB+ for large datasets
4. **Storage**: SSD for better performance
5. **Backup**: Regular RDF dumps

### Environment Variables for Production

```
NEXT_PUBLIC_SPARQL_ENDPOINT=https://sparql.example.com/query
NEXT_PUBLIC_SPARQL_GRAPH=https://example.com/data
SPARQL_USERNAME=readonly_user
SPARQL_PASSWORD=strong_password_here
```

## References

- [Apache Jena Documentation](https://jena.apache.org/)
- [Virtuoso Documentation](https://virtuoso.openlinksw.com/docs/)
- [GraphDB Documentation](https://graphdb.ontotext.com/documentation/)
- [SPARQL 1.1 Query Language](https://www.w3.org/TR/sparql11-query/)
- [SPARQL 1.1 Update Language](https://www.w3.org/TR/sparql11-update/)

## Getting Help

If you encounter issues:

1. Check SPARQL endpoint logs
2. Verify ontology is loaded: `SELECT ?p WHERE { ?s ?p ?o } LIMIT 1`
3. Test with curl before using client
4. Enable debug logging in application
