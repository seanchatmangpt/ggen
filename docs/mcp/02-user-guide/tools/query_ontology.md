# query_ontology Tool

Execute a SPARQL SELECT query against a Turtle ontology string.

## Parameters

| Name | Type | Required | Description |
|------|------|----------|-------------|
| `ttl` | string | Yes | Turtle ontology content |
| `sparql` | string | Yes | SPARQL SELECT query |

## Supported Queries

### SELECT Queries

```sparql
SELECT ?subject ?predicate ?object
WHERE {
  ?subject ?predicate ?object .
}
LIMIT 10
```

### FILTER Clauses

```sparql
SELECT ?name ?type
WHERE {
  ?s a ?type .
  ?s ex:name ?name .
  FILTER(?type = ex:Person)
}
```

### OPTIONAL Patterns

```sparql
SELECT ?person ?email
WHERE {
  ?person a ex:Person .
  OPTIONAL { ?person ex:email ?email }
}
```

## Example Usage

### Count All Triples

```json
{
  "name": "query_ontology",
  "arguments": {
    "ttl": "@prefix ex: <http://example.org/> .\nex:Subject ex:predicate ex:Object .",
    "sparql": "SELECT (COUNT(*) as ?count) WHERE { ?s ?p ?o }"
  }
}
```

### Find All Classes

```json
{
  "name": "query_ontology",
  "arguments": {
    "ttl": "@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .\n@prefix ex: <http://example.org/> .\nex:Person a rdfs:Class .",
    "sparql": "SELECT ?class WHERE { ?class a rdfs:Class }"
  }
}
```

### Get Instances by Type

```json
{
  "name": "query_ontology",
  "arguments": {
    "ttl": "<ontology content>",
    "sparql": "SELECT ?instance WHERE { ?instance a ex:Person }"
  }
}
```

## Output

### Success Response

```json
{
  "results": [
    {
      "subject": {
        "type": "uri",
        "value": "http://example.org/Subject"
      },
      "predicate": {
        "type": "uri",
        "value": "http://example.org/predicate"
      },
      "object": {
        "type": "literal",
        "value": "Object",
        "datatype": "http://www.w3.org/2001/XMLSchema#string"
      }
    }
  ],
  "count": 1,
  "execution_time_ms": 23
}
```

### Error Response

```json
{
  "error": {
    "code": "SPARQL_PARSE_ERROR",
    "message": "Invalid SPARQL syntax",
    "details": {
      "line": 3,
      "column": 12
    }
  }
}
```

## Error Codes

| Code | Description |
|------|-------------|
| `SPARQL_PARSE_ERROR` | Invalid SPARQL syntax |
| `INVALID_TTL` | Turtle content cannot be parsed |
| `QUERY_TIMEOUT` | Query exceeded time limit |
| `RESULT_TOO_LARGE` | Result set exceeds size limit |

## Limits

| Setting | Value |
|---------|-------|
| Max execution time | 30 seconds |
| Max result rows | 10,000 |
| Max triple count | 1,000,000 |

## Tips

1. **Use LIMIT**: Always add `LIMIT` clause to avoid large result sets
2. **Use SELECT ***: Select specific variables instead of all
3. **Index patterns**: Put most selective patterns first
4. **Validate first**: Use `validate` tool to check TTL before querying

## Related Tools

- [`validate`](./validate.md) - Validate ontology before querying
- [`validate_sparql`](./validate_sparql.md) - Validate SPARQL query syntax
- [`generate`](./generate.md) - Generate code from query results
