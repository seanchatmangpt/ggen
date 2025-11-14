# Task Management RDF Ontology

A production-ready RDF ontology with SHACL validation for task management systems.

## Overview

This ontology defines a complete data model for task management applications with built-in validation rules that can drive both backend validation and frontend UI constraints.

## Core Entities

### 1. **User** (`tm:User`)
Represents system users with authentication and authorization.

**Properties:**
- `tm:userName` (string, required, 1-100 chars) - Display name
- `tm:userEmail` (string, required, email format) - Authentication email
- `tm:userRole` (enum: "admin" | "user", required) - Authorization role
- `tm:userCreatedAt` (dateTime, required) - Account creation timestamp

**Example:**
```turtle
tmd:user1 a tm:User ;
    tm:userName "Alice Johnson" ;
    tm:userEmail "alice.johnson@example.com" ;
    tm:userRole "admin" ;
    tm:userCreatedAt "2024-01-15T09:00:00Z"^^xsd:dateTime .
```

### 2. **Project** (`tm:Project`)
Container for organizing related tasks.

**Properties:**
- `tm:projectName` (string, required, 1-200 chars) - Display name
- `tm:projectDescription` (string, optional, max 2000 chars) - Detailed description
- `tm:projectOwner` (User reference, required) - Project owner
- `tm:projectCreatedAt` (dateTime, required) - Creation timestamp

**Example:**
```turtle
tmd:project1 a tm:Project ;
    tm:projectName "Website Redesign" ;
    tm:projectDescription "Complete overhaul of company website" ;
    tm:projectOwner tmd:user1 ;
    tm:projectCreatedAt "2024-02-01T08:00:00Z"^^xsd:dateTime .
```

### 3. **Task** (`tm:Task`)
Individual work items with status, priority, and assignments.

**Properties:**
- `tm:taskTitle` (string, required, 1-300 chars) - Brief summary
- `tm:taskDescription` (string, optional, max 5000 chars) - Detailed description
- `tm:taskStatus` (enum: "todo" | "in-progress" | "done", required) - Current state
- `tm:taskPriority` (integer, required, 1-5) - Priority level
- `tm:belongsToProject` (Project reference, required) - Parent project
- `tm:taskAssignee` (User reference, optional) - Assigned user
- `tm:taskDueDate` (dateTime, optional) - Target completion date
- `tm:taskCreatedAt` (dateTime, required) - Creation timestamp
- `tm:taskUpdatedAt` (dateTime, optional) - Last modification timestamp

**Example:**
```turtle
tmd:task1 a tm:Task ;
    tm:taskTitle "Design homepage mockup" ;
    tm:taskDescription "Create high-fidelity mockups for new homepage" ;
    tm:taskStatus "in-progress" ;
    tm:taskPriority 5 ;
    tm:taskAssignee tmd:user2 ;
    tm:belongsToProject tmd:project1 ;
    tm:taskDueDate "2024-02-15T17:00:00Z"^^xsd:dateTime ;
    tm:taskCreatedAt "2024-02-02T10:00:00Z"^^xsd:dateTime ;
    tm:taskUpdatedAt "2024-02-14T16:30:00Z"^^xsd:dateTime .
```

### 4. **Comment** (`tm:Comment`)
Discussion entries attached to tasks.

**Properties:**
- `tm:commentContent` (string, required, 1-5000 chars) - Comment text
- `tm:commentAuthor` (User reference, required) - Comment author
- `tm:belongsToTask` (Task reference, required) - Parent task
- `tm:commentCreatedAt` (dateTime, required) - Creation timestamp

**Example:**
```turtle
tmd:comment1 a tm:Comment ;
    tm:commentContent "Great work on the mockups!" ;
    tm:commentAuthor tmd:user1 ;
    tm:belongsToTask tmd:task1 ;
    tm:commentCreatedAt "2024-02-14T15:00:00Z"^^xsd:dateTime .
```

## SHACL Validation Rules

The ontology includes comprehensive SHACL shapes for data validation:

### User Validation (`tm:UserShape`)
- ✅ Name: Required, 1-100 characters
- ✅ Email: Required, must match email regex pattern
- ✅ Role: Required, must be "admin" or "user"
- ✅ CreatedAt: Required timestamp

### Project Validation (`tm:ProjectShape`)
- ✅ Name: Required, 1-200 characters
- ✅ Description: Optional, max 2000 characters
- ✅ Owner: Required, must reference valid User
- ✅ CreatedAt: Required timestamp

### Task Validation (`tm:TaskShape`)
- ✅ Title: Required, 1-300 characters
- ✅ Description: Optional, max 5000 characters
- ✅ Status: Required, must be "todo", "in-progress", or "done"
- ✅ Priority: Required, must be integer 1-5
- ✅ Project: Required, must reference valid Project
- ✅ Assignee: Optional, must reference valid User if present
- ✅ DueDate: Optional timestamp
- ✅ CreatedAt: Required timestamp
- ✅ UpdatedAt: Optional timestamp

### Comment Validation (`tm:CommentShape`)
- ✅ Content: Required, 1-5000 characters
- ✅ Author: Required, must reference valid User
- ✅ Task: Required, must reference valid Task
- ✅ CreatedAt: Required timestamp

## UI Integration Guide

### Form Field Constraints

Extract validation rules from SHACL shapes to configure UI forms:

```javascript
// Example: Task form validation
const taskFormRules = {
  title: {
    required: true,
    minLength: 1,
    maxLength: 300,
    errorMessage: "Task title is required and must be 1-300 characters"
  },
  status: {
    required: true,
    enum: ["todo", "in-progress", "done"],
    errorMessage: "Task status must be 'todo', 'in-progress', or 'done'"
  },
  priority: {
    required: true,
    type: "integer",
    min: 1,
    max: 5,
    errorMessage: "Task priority is required and must be between 1 and 5"
  },
  projectId: {
    required: true,
    type: "reference",
    entity: "Project",
    errorMessage: "Task must belong to exactly one project"
  },
  assigneeId: {
    required: false,
    type: "reference",
    entity: "User"
  }
};
```

### Dropdown Options

Use `sh:in` constraints to populate select/dropdown fields:

```javascript
// User Role dropdown
<select name="role" required>
  <option value="admin">Admin</option>
  <option value="user">User</option>
</select>

// Task Status dropdown
<select name="status" required>
  <option value="todo">To Do</option>
  <option value="in-progress">In Progress</option>
  <option value="done">Done</option>
</select>

// Task Priority dropdown
<select name="priority" required>
  <option value="1">1 - Lowest</option>
  <option value="2">2 - Low</option>
  <option value="3">3 - Medium</option>
  <option value="4">4 - High</option>
  <option value="5">5 - Highest</option>
</select>
```

### Email Validation

Use the email regex pattern from SHACL:

```javascript
const EMAIL_PATTERN = /^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$/;

function validateEmail(email) {
  if (!EMAIL_PATTERN.test(email)) {
    return "Valid email address is required (e.g., user@example.com)";
  }
  return null;
}
```

## SPARQL Query Examples

### Get High-Priority Tasks

```sparql
PREFIX tm: <http://example.org/taskmanagement/ontology#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

SELECT ?task ?title ?assigneeName ?dueDate WHERE {
  ?task a tm:Task ;
        tm:taskTitle ?title ;
        tm:taskStatus "in-progress" ;
        tm:taskPriority ?priority ;
        tm:taskDueDate ?dueDate .

  OPTIONAL {
    ?task tm:taskAssignee ?assignee .
    ?assignee tm:userName ?assigneeName .
  }

  FILTER(?priority >= 4)
} ORDER BY ?dueDate
```

### Get Tasks Due This Week

```sparql
PREFIX tm: <http://example.org/taskmanagement/ontology#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

SELECT ?task ?title ?status ?projectName WHERE {
  ?task a tm:Task ;
        tm:taskTitle ?title ;
        tm:taskStatus ?status ;
        tm:taskDueDate ?dueDate ;
        tm:belongsToProject ?project .

  ?project tm:projectName ?projectName .

  FILTER(?dueDate >= "2024-02-19T00:00:00Z"^^xsd:dateTime &&
         ?dueDate < "2024-02-26T00:00:00Z"^^xsd:dateTime)
} ORDER BY ?dueDate
```

### Get Task Comments with Authors

```sparql
PREFIX tm: <http://example.org/taskmanagement/ontology#>
PREFIX tmd: <http://example.org/taskmanagement/data#>

SELECT ?comment ?content ?authorName ?createdAt WHERE {
  ?comment a tm:Comment ;
           tm:belongsToTask tmd:task1 ;
           tm:commentContent ?content ;
           tm:commentAuthor ?author ;
           tm:commentCreatedAt ?createdAt .

  ?author tm:userName ?authorName .
} ORDER BY ?createdAt
```

### Get User's Tasks by Status

```sparql
PREFIX tm: <http://example.org/taskmanagement/ontology#>
PREFIX tmd: <http://example.org/taskmanagement/data#>

SELECT ?status (COUNT(?task) as ?count) WHERE {
  ?task a tm:Task ;
        tm:taskStatus ?status ;
        tm:taskAssignee tmd:user2 .
} GROUP BY ?status
```

### Get Project Progress

```sparql
PREFIX tm: <http://example.org/taskmanagement/ontology#>
PREFIX tmd: <http://example.org/taskmanagement/data#>

SELECT
  ?status
  (COUNT(?task) as ?count)
  (AVG(?priority) as ?avgPriority)
WHERE {
  ?task a tm:Task ;
        tm:taskStatus ?status ;
        tm:taskPriority ?priority ;
        tm:belongsToProject tmd:project1 .
} GROUP BY ?status
```

## Validation Examples

### Python (pySHACL)

```python
import rdflib
from pyshacl import validate

# Load the ontology
graph = rdflib.Graph()
graph.parse("task-management.ttl", format="turtle")

# Validate data
conforms, results_graph, results_text = validate(
    graph,
    shacl_graph=graph,
    inference='rdfs',
    abort_on_first=False
)

if not conforms:
    print("Validation failures:")
    print(results_text)
else:
    print("All data is valid!")
```

### JavaScript (rdf-validate-shacl)

```javascript
const factory = require('rdf-ext');
const ParserN3 = require('@rdfjs/parser-n3');
const SHACLValidator = require('rdf-validate-shacl');
const fs = require('fs');

async function validateData() {
  const parser = new ParserN3({ factory });

  // Load shapes and data
  const stream = fs.createReadStream('task-management.ttl');
  const dataset = await factory.dataset().import(parser.import(stream));

  // Create validator
  const validator = new SHACLValidator(dataset, { factory });

  // Validate
  const report = validator.validate(dataset);

  if (!report.conforms) {
    console.log('Validation failures:');
    for (const result of report.results) {
      console.log(`- ${result.message[0].value}`);
    }
  } else {
    console.log('All data is valid!');
  }
}

validateData();
```

## Extension Examples

### Adding Time Tracking

```turtle
# New property for estimated hours
tm:taskEstimatedHours a rdf:Property ;
    rdfs:label "estimated hours" ;
    rdfs:comment "Estimated time to complete the task" ;
    rdfs:domain tm:Task ;
    rdfs:range xsd:decimal .

# Add to TaskShape
tm:TaskShape sh:property [
    sh:path tm:taskEstimatedHours ;
    sh:name "estimated hours" ;
    sh:datatype xsd:decimal ;
    sh:minInclusive 0.0 ;
    sh:maxInclusive 1000.0 ;
] .
```

### Adding Task Tags

```turtle
# New property for tags
tm:taskTags a rdf:Property ;
    rdfs:label "task tags" ;
    rdfs:comment "Categorization tags for the task" ;
    rdfs:domain tm:Task ;
    rdfs:range xsd:string .

# Add to TaskShape with enum constraint
tm:TaskShape sh:property [
    sh:path tm:taskTags ;
    sh:name "task tags" ;
    sh:datatype xsd:string ;
    sh:in ("bug" "feature" "documentation" "refactor" "testing") ;
] .
```

### Adding Task Dependencies

```turtle
# New property for dependencies
tm:taskBlockedBy a rdf:Property ;
    rdfs:label "blocked by" ;
    rdfs:comment "Other tasks that must be completed first" ;
    rdfs:domain tm:Task ;
    rdfs:range tm:Task .

# Add to TaskShape
tm:TaskShape sh:property [
    sh:path tm:taskBlockedBy ;
    sh:name "blocked by" ;
    sh:class tm:Task ;
] .
```

## Best Practices

### 1. Data Entry
- Always validate data against SHACL shapes before insertion
- Use appropriate namespaces (`:data` for instances, `:ontology` for schema)
- Include all required fields to avoid validation errors
- Use ISO 8601 format for dates (`2024-02-15T17:00:00Z`)

### 2. Querying
- Use SPARQL for complex queries and aggregations
- Create indexes on frequently queried properties
- Use OPTIONAL for properties that may not exist
- Filter results early in the query for better performance

### 3. UI Integration
- Parse SHACL shapes at build time to generate form validation
- Cache validation rules to avoid repeated parsing
- Display user-friendly error messages from `sh:message`
- Use enum constraints to populate dropdown options

### 4. Performance
- Use a triplestore (e.g., Apache Jena, Virtuoso) for large datasets
- Create appropriate indexes for common query patterns
- Consider materialized views for complex aggregations
- Batch insert/update operations when possible

## File Structure

```
docs/examples/
├── task-management.ttl       # Main ontology file
├── task-management-README.md # This file
└── task-management-tests/    # Validation test cases (optional)
```

## License

This ontology is provided as an example for educational purposes. Adapt and extend as needed for your use case.

## Further Reading

- [RDF Primer](https://www.w3.org/TR/rdf11-primer/)
- [SHACL Specification](https://www.w3.org/TR/shacl/)
- [SPARQL 1.1 Query Language](https://www.w3.org/TR/sparql11-query/)
- [Turtle Syntax](https://www.w3.org/TR/turtle/)
