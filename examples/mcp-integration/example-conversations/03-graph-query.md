# Example 3: Knowledge Graph Queries

This example demonstrates using ggen-mcp to query and manipulate RDF knowledge graphs with SPARQL.

## Basic Graph Queries

### User:
```
Can you show me all the templates in my knowledge graph and their dependencies?
```

### Claude:
I'll query the knowledge graph to find all templates and their dependencies.

**Tool Call:** `graph_query`
```json
{
  "sparql": "PREFIX ggen: <http://ggen.ai/ontology#>\nPREFIX npm: <http://npm.org/>\n\nSELECT ?template ?templateName ?dep ?depName ?version\nWHERE {\n  ?template a ggen:Template ;\n            ggen:name ?templateName ;\n            ggen:hasDependency ?dep .\n  ?dep ggen:name ?depName ;\n       ggen:version ?version .\n}\nORDER BY ?templateName ?depName"
}
```

**Response:**
```json
{
  "status": "success",
  "results": {
    "bindings": [
      {
        "template": "http://ggen.ai/templates/react-typescript-starter",
        "templateName": "react-typescript-starter",
        "dep": "http://npm.org/react",
        "depName": "react",
        "version": "^18.2.0"
      },
      {
        "template": "http://ggen.ai/templates/react-typescript-starter",
        "templateName": "react-typescript-starter",
        "dep": "http://npm.org/typescript",
        "depName": "typescript",
        "version": "^5.0.0"
      },
      {
        "template": "http://ggen.ai/templates/fastapi-microservice",
        "templateName": "fastapi-microservice",
        "dep": "http://pypi.org/fastapi",
        "depName": "fastapi",
        "version": ">=0.100.0"
      },
      {
        "template": "http://ggen.ai/templates/fastapi-microservice",
        "templateName": "fastapi-microservice",
        "dep": "http://pypi.org/uvicorn",
        "depName": "uvicorn",
        "version": ">=0.23.0"
      }
    ],
    "count": 4
  }
}
```

Found **4 dependencies** across **2 templates**:

**📦 react-typescript-starter**
- react ^18.2.0
- typescript ^5.0.0

**📦 fastapi-microservice**
- fastapi >=0.100.0
- uvicorn >=0.23.0

Would you like me to query for more specific information?

## AI-Powered SPARQL Generation

### User:
```
Find all templates that use React 18 or newer and have TypeScript
```

### Claude:
Let me generate a SPARQL query to find React 18+ templates with TypeScript.

**Tool Call:** `ai_generate_sparql`
```json
{
  "intent": "Find templates with React version 18 or higher and TypeScript as a dependency",
  "schema": "ggen ontology with Template, hasDependency, Dependency entities",
  "provider": "anthropic"
}
```

**Response:**
```json
{
  "status": "success",
  "generated_query": "PREFIX ggen: <http://ggen.ai/ontology#>\nPREFIX npm: <http://npm.org/>\nPREFIX xsd: <http://www.w3.org/2001/XMLSchema#>\n\nSELECT DISTINCT ?template ?name ?description\nWHERE {\n  ?template a ggen:Template ;\n            ggen:name ?name ;\n            ggen:description ?description ;\n            ggen:hasDependency ?reactDep ;\n            ggen:hasDependency ?tsDep .\n  \n  ?reactDep ggen:name \"react\" ;\n            ggen:version ?reactVersion .\n  \n  ?tsDep ggen:name \"typescript\" .\n  \n  FILTER(regex(?reactVersion, \"^(\\\\^|>=)?1[8-9]\\\\.\", \"i\") ||\n         regex(?reactVersion, \"^(\\\\^|>=)?[2-9][0-9]\\\\.\", \"i\"))\n}\nORDER BY ?name",
  "explanation": "This query finds templates that have both React (version 18+) and TypeScript dependencies. The FILTER uses regex to match version strings starting with 18, 19, or 20+."
}
```

Generated SPARQL query:
```sparql
PREFIX ggen: <http://ggen.ai/ontology#>
PREFIX npm: <http://npm.org/>

SELECT DISTINCT ?template ?name ?description
WHERE {
  ?template a ggen:Template ;
            ggen:name ?name ;
            ggen:description ?description ;
            ggen:hasDependency ?reactDep ;
            ggen:hasDependency ?tsDep .

  ?reactDep ggen:name "react" ;
            ggen:version ?reactVersion .

  ?tsDep ggen:name "typescript" .

  FILTER(regex(?reactVersion, "^(\\^|>=)?1[8-9]\\.", "i") ||
         regex(?reactVersion, "^(\\^|>=)?[2-9][0-9]\\.", "i"))
}
ORDER BY ?name
```

Now let me execute this query:

**Tool Call:** `graph_query`
```json
{
  "sparql": "PREFIX ggen: <http://ggen.ai/ontology#>\nPREFIX npm: <http://npm.org/>\n\nSELECT DISTINCT ?template ?name ?description\nWHERE {\n  ?template a ggen:Template ;\n            ggen:name ?name ;\n            ggen:description ?description ;\n            ggen:hasDependency ?reactDep ;\n            ggen:hasDependency ?tsDep .\n  ?reactDep ggen:name \"react\" ;\n            ggen:version ?reactVersion .\n  ?tsDep ggen:name \"typescript\" .\n  FILTER(regex(?reactVersion, \"^(\\\\^|>=)?1[8-9]\\\\.\", \"i\") ||\n         regex(?reactVersion, \"^(\\\\^|>=)?[2-9][0-9]\\\\.\", \"i\"))\n}\nORDER BY ?name"
}
```

**Response:**
```json
{
  "status": "success",
  "results": {
    "bindings": [
      {
        "template": "http://ggen.ai/templates/react-typescript-starter",
        "name": "react-typescript-starter",
        "description": "Modern React app with TypeScript, Vite, and ESLint"
      },
      {
        "template": "http://ggen.ai/templates/nextjs-typescript",
        "name": "nextjs-typescript",
        "description": "Next.js with TypeScript, Tailwind CSS, and App Router"
      },
      {
        "template": "http://ggen.ai/templates/react-component-lib",
        "name": "react-component-lib",
        "description": "React component library with Storybook and TypeScript"
      }
    ],
    "count": 3
  }
}
```

Found **3 templates** with React 18+ and TypeScript:

1. **react-typescript-starter**
   - Modern React app with TypeScript, Vite, and ESLint

2. **nextjs-typescript**
   - Next.js with TypeScript, Tailwind CSS, and App Router

3. **react-component-lib**
   - React component library with Storybook and TypeScript

## Loading RDF Data

### User:
```
I have a template metadata file in RDF/Turtle format. Can you load it into the graph?
```

### Claude:
I'll load your RDF/Turtle file into the knowledge graph.

**Tool Call:** `graph_load`
```json
{
  "file": "/path/to/templates/metadata.ttl",
  "graph": "http://ggen.ai/templates",
  "format": "turtle"
}
```

**Response:**
```json
{
  "status": "success",
  "loaded": {
    "triples": 142,
    "entities": 23,
    "graph": "http://ggen.ai/templates"
  },
  "summary": {
    "templates": 8,
    "dependencies": 47,
    "authors": 5,
    "categories": 3
  },
  "message": "Successfully loaded 142 triples into graph http://ggen.ai/templates"
}
```

✅ **Loaded RDF data successfully!**

**Imported:**
- 📊 142 triples
- 🎯 23 entities
- 📦 8 templates
- 🔗 47 dependencies
- 👥 5 authors
- 📂 3 categories

Into graph: `http://ggen.ai/templates`

The data is now queryable. Would you like to run a query on the newly loaded data?

## Complex Analytical Queries

### User:
```
Show me templates grouped by category with their popularity scores
```

### Claude:
I'll query for templates grouped by category with popularity metrics.

**Tool Call:** `graph_query`
```json
{
  "sparql": "PREFIX ggen: <http://ggen.ai/ontology#>\nPREFIX schema: <http://schema.org/>\n\nSELECT ?category (COUNT(?template) as ?templateCount) (AVG(?downloads) as ?avgDownloads) (AVG(?rating) as ?avgRating)\nWHERE {\n  ?template a ggen:Template ;\n            ggen:category ?category ;\n            ggen:downloads ?downloads ;\n            ggen:rating ?rating .\n}\nGROUP BY ?category\nORDER BY DESC(?avgDownloads)"
}
```

**Response:**
```json
{
  "status": "success",
  "results": {
    "bindings": [
      {
        "category": "web",
        "templateCount": 12,
        "avgDownloads": 8542.5,
        "avgRating": 4.6
      },
      {
        "category": "api",
        "templateCount": 8,
        "avgDownloads": 6321.2,
        "avgRating": 4.7
      },
      {
        "category": "cli",
        "templateCount": 5,
        "avgDownloads": 4128.8,
        "avgRating": 4.5
      }
    ]
  }
}
```

**📊 Template Popularity by Category:**

| Category | Templates | Avg Downloads | Avg Rating |
|----------|-----------|---------------|------------|
| 🌐 Web | 12 | 8,542 | ⭐ 4.6 |
| 🔌 API | 8 | 6,321 | ⭐ 4.7 |
| 💻 CLI | 5 | 4,128 | ⭐ 4.5 |

**Insights:**
- Most popular category: **Web** (12 templates, 8.5K avg downloads)
- Highest rated: **API** (4.7 stars)
- Growing category: **CLI** (good ratings, room for growth)

## Extending Graphs with AI

### User:
```
Can AI help me add more semantic information to my template graph?
```

### Claude:
I'll use AI to analyze your templates and extend the knowledge graph with semantic relationships.

**Tool Call:** `ai_extend_graph`
```json
{
  "graph": "http://ggen.ai/templates",
  "context": "Analyze template relationships, identify design patterns, extract semantic metadata, and infer missing relationships",
  "provider": "anthropic"
}
```

**Response:**
```json
{
  "status": "success",
  "added_triples": 87,
  "relationships_inferred": [
    {
      "type": "similarTo",
      "count": 15,
      "example": "react-typescript-starter similarTo nextjs-typescript"
    },
    {
      "type": "extendsPattern",
      "count": 8,
      "example": "fastapi-auth extendsPattern fastapi-base"
    },
    {
      "type": "usesDesignPattern",
      "count": 12,
      "example": "react-component-lib usesDesignPattern ComponentDrivenDevelopment"
    },
    {
      "type": "compatibleWith",
      "count": 23,
      "example": "express-middleware compatibleWith express-api-base"
    }
  ],
  "semantic_tags_added": [
    "microservices",
    "serverless",
    "monorepo",
    "component-driven",
    "test-driven"
  ],
  "recommendations": [
    "Added 15 'similarTo' relationships between templates with similar dependencies",
    "Identified 8 template inheritance patterns",
    "Tagged templates with 5 architectural patterns",
    "Created 23 compatibility relationships"
  ]
}
```

🎯 **Graph extended with AI-generated knowledge!**

**Added 87 new triples:**

**Relationships Inferred:**
- 🔗 15 similarity relationships
- 🔗 8 extension patterns
- 🔗 12 design pattern annotations
- 🔗 23 compatibility links

**Semantic Tags:**
- microservices
- serverless
- monorepo
- component-driven
- test-driven

**Example Relationships:**
```turtle
ggen:react-typescript-starter ggen:similarTo ggen:nextjs-typescript .
ggen:fastapi-auth ggen:extendsPattern ggen:fastapi-base .
ggen:react-component-lib ggen:usesDesignPattern ggen:ComponentDrivenDevelopment .
ggen:express-middleware ggen:compatibleWith ggen:express-api-base .
```

Now you can query for templates by design patterns or find compatible templates!

## Exporting Graph Data

### User:
```
Export the extended graph so I can share it with my team
```

### Claude:
I'll export the knowledge graph to a file in RDF/XML format.

**Tool Call:** `graph_export`
```json
{
  "output": "/path/to/export/template-knowledge-graph.rdf",
  "graph": "http://ggen.ai/templates",
  "format": "rdfxml"
}
```

**Response:**
```json
{
  "status": "success",
  "exported": {
    "file": "/path/to/export/template-knowledge-graph.rdf",
    "format": "rdfxml",
    "triples": 229,
    "size": "45.2 KB"
  },
  "summary": {
    "templates": 25,
    "dependencies": 87,
    "relationships": 34,
    "semantic_tags": 15
  }
}
```

✅ **Graph exported successfully!**

**Export Details:**
- 📁 File: `template-knowledge-graph.rdf`
- 📊 Format: RDF/XML
- 🔢 Triples: 229
- 💾 Size: 45.2 KB

**Contents:**
- 25 templates
- 87 dependencies
- 34 relationships
- 15 semantic tags

Your team can now:
1. Import this graph into their own ggen instance
2. Use RDF tools like Protégé for visualization
3. Query with SPARQL from any RDF-compatible tool
4. Version control the graph as part of your repository

## Advanced Pattern Matching

### User:
```
Find templates that could work together to build a full-stack application
```

### Claude:
Let me query for compatible templates that form a complete stack.

**Tool Call:** `graph_query`
```json
{
  "sparql": "PREFIX ggen: <http://ggen.ai/ontology#>\n\nSELECT ?frontend ?frontendName ?backend ?backendName ?database ?databaseName\nWHERE {\n  ?frontend a ggen:Template ;\n            ggen:name ?frontendName ;\n            ggen:category \"web\" ;\n            ggen:compatibleWith ?backend .\n  \n  ?backend a ggen:Template ;\n           ggen:name ?backendName ;\n           ggen:category \"api\" ;\n           ggen:compatibleWith ?database .\n  \n  ?database a ggen:Template ;\n            ggen:name ?databaseName ;\n            ggen:category \"database\" .\n  \n  FILTER(?frontend != ?backend && ?backend != ?database)\n}\nLIMIT 5"
}
```

**Response:**
```json
{
  "status": "success",
  "results": {
    "bindings": [
      {
        "frontend": "http://ggen.ai/templates/react-typescript-starter",
        "frontendName": "react-typescript-starter",
        "backend": "http://ggen.ai/templates/fastapi-microservice",
        "backendName": "fastapi-microservice",
        "database": "http://ggen.ai/templates/postgres-docker",
        "databaseName": "postgres-docker"
      },
      {
        "frontend": "http://ggen.ai/templates/nextjs-typescript",
        "frontendName": "nextjs-typescript",
        "backend": "http://ggen.ai/templates/express-typescript-api",
        "backendName": "express-typescript-api",
        "database": "http://ggen.ai/templates/mongodb-setup",
        "databaseName": "mongodb-setup"
      }
    ],
    "count": 2
  }
}
```

🎯 **Found 2 compatible full-stack combinations:**

**Stack 1: Modern Python Stack**
- 🎨 Frontend: **react-typescript-starter**
- ⚙️ Backend: **fastapi-microservice**
- 💾 Database: **postgres-docker**

**Stack 2: Node.js Stack**
- 🎨 Frontend: **nextjs-typescript**
- ⚙️ Backend: **express-typescript-api**
- 💾 Database: **mongodb-setup**

These stacks are verified compatible through the knowledge graph relationships. Would you like me to generate a multi-project setup using one of these stacks?

## Key Takeaways

1. **Query:** Use `graph_query` with SPARQL for precise data retrieval
2. **AI Generation:** Use `ai_generate_sparql` to create queries from natural language
3. **Load Data:** Use `graph_load` to import RDF files
4. **Extend:** Use `ai_extend_graph` to add semantic relationships
5. **Export:** Use `graph_export` to share knowledge graphs

## Related Tools

- `ai_generate_ontology` - Create ontologies from domain descriptions
- `market_search` - Search marketplace (results stored in graph)
- `project_gen` - Generate projects (metadata added to graph)
- `template_validate` - Validation results stored in graph
