# Template Processing Algorithms - Pseudocode Specification

**SPARC Phase 2: Pseudocode**
**Version:** 1.0.0
**Date:** 2025-11-01

## Table of Contents

1. [Core Data Structures](#core-data-structures)
2. [Template Parser Algorithm](#template-parser-algorithm)
3. [File Tree Generator Algorithm](#file-tree-generator-algorithm)
4. [RDF Metadata Processor](#rdf-metadata-processor)
5. [Variable Substitution Engine](#variable-substitution-engine)
6. [Template Composition](#template-composition)
7. [Validation Algorithm](#validation-algorithm)
8. [Complexity Analysis](#complexity-analysis)

---

## Core Data Structures

### TemplateNode Structure
```
STRUCTURE: TemplateNode
    id: String                          // Unique identifier
    node_type: NodeType                 // File, Directory, Virtual
    path: PathTemplate                  // Path with variable placeholders
    content: Option<ContentTemplate>    // Template content for files
    metadata: RDFMetadata              // RDF triples for this node
    children: List<TemplateNode>       // Child nodes (for directories)
    directives: List<Directive>        // Processing directives
    variables: Set<String>             // Required variables

ENUM: NodeType
    File(FileType)                     // Regular file
    Directory                          // Directory node
    Virtual                            // Virtual node (conditional)
    Symbolic                           // Symbolic link

STRUCTURE: PathTemplate
    segments: List<PathSegment>        // Path components
    is_absolute: Boolean               // Absolute vs relative

STRUCTURE: PathSegment
    literal: Option<String>            // Literal text
    variable: Option<Variable>         // Variable reference
    expression: Option<Expression>     // Computed expression
```

### FileTreeNode Structure
```
STRUCTURE: FileTreeNode
    path: PathBuf                      // Resolved absolute path
    node_type: NodeType                // Type of node
    content: Option<Bytes>             // Generated content
    permissions: FilePermissions       // Unix permissions
    metadata: GeneratedMetadata        // Applied metadata
    rdf_uri: Option<URI>              // RDF resource URI
    children: List<FileTreeNode>      // Generated children

STRUCTURE: GeneratedMetadata
    created_from: TemplateURI          // Source template
    variables_used: Map<String, Value> // Substituted variables
    generation_time: Timestamp         // When generated
    rdf_triples: List<Triple>         // Associated RDF data
```

### RDFMetadata Structure
```
STRUCTURE: RDFMetadata
    subject: Option<URI>               // RDF subject (defaults to file URI)
    triples: List<Triple>             // RDF triples
    namespaces: Map<String, URI>      // Namespace prefixes
    shapes: List<SHACLShape>          // SHACL validation shapes

STRUCTURE: Triple
    subject: URI                       // RDF subject
    predicate: URI                     // RDF predicate
    object: RDFValue                   // RDF object (URI or Literal)

ENUM: RDFValue
    URI(URI)                          // Resource reference
    Literal(String, Option<Datatype>) // Literal value with optional type
    BlankNode(String)                 // Blank node identifier
```

### TemplateContext Structure
```
STRUCTURE: TemplateContext
    variables: Map<String, Value>      // User-provided variables
    environment: Map<String, String>   // Environment variables
    functions: Map<String, Function>   // Template functions
    rdf_store: OxigraphStore          // RDF graph store
    output_base: PathBuf              // Base output directory
    options: GenerationOptions        // Generation options

STRUCTURE: GenerationOptions
    overwrite_existing: Boolean        // Overwrite files
    create_directories: Boolean        // Auto-create dirs
    validate_rdf: Boolean             // Enable RDF validation
    dry_run: Boolean                  // Don't write files
    incremental: Boolean              // Incremental generation
```

---

## Template Parser Algorithm

### Main Parsing Algorithm

```
ALGORITHM: ParseTemplate
INPUT: template_path (PathBuf), base_context (TemplateContext)
OUTPUT: TemplateNode or Error

BEGIN
    // Phase 1: Read and validate template file
    template_content ← ReadFile(template_path)
    IF template_content is Error THEN
        RETURN Error("Failed to read template: " + template_path)
    END IF

    // Phase 2: Extract frontmatter (YAML/TOML with RDF)
    frontmatter_result ← ExtractFrontmatter(template_content)
    IF frontmatter_result is Error THEN
        RETURN Error("Invalid frontmatter")
    END IF

    frontmatter ← frontmatter_result.data
    template_body ← frontmatter_result.remaining

    // Phase 3: Parse metadata and RDF
    metadata ← ParseRDFMetadata(frontmatter.rdf_section)
    template_config ← ParseTemplateConfig(frontmatter.config_section)

    // Phase 4: Parse template directives
    ast ← ParseTemplateDirectives(template_body, metadata)

    // Phase 5: Build template node tree
    root_node ← BuildTemplateTree(ast, metadata, template_config)

    // Phase 6: Validate template structure
    validation_result ← ValidateTemplate(root_node)
    IF validation_result is Error THEN
        RETURN validation_result
    END IF

    RETURN root_node
END

TIME COMPLEXITY: O(n) where n = template file size
SPACE COMPLEXITY: O(n) for AST storage
```

### Frontmatter Extraction

```
ALGORITHM: ExtractFrontmatter
INPUT: content (String)
OUTPUT: FrontmatterResult or Error

BEGIN
    // Detect frontmatter format (YAML or TOML)
    IF content starts with "---" THEN
        format ← YAML
        delimiter ← "---"
    ELSE IF content starts with "+++" THEN
        format ← TOML
        delimiter ← "+++"
    ELSE
        RETURN {data: {}, remaining: content}  // No frontmatter
    END IF

    // Extract frontmatter block
    lines ← content.split_lines()
    frontmatter_lines ← []
    i ← 1  // Skip first delimiter

    WHILE i < lines.length DO
        IF lines[i] equals delimiter THEN
            BREAK  // Found closing delimiter
        END IF
        frontmatter_lines.append(lines[i])
        i ← i + 1
    END WHILE

    IF i >= lines.length THEN
        RETURN Error("Unclosed frontmatter block")
    END IF

    // Parse frontmatter based on format
    frontmatter_text ← frontmatter_lines.join("\n")

    IF format is YAML THEN
        data ← YAMLParse(frontmatter_text)
    ELSE
        data ← TOMLParse(frontmatter_text)
    END IF

    IF data is Error THEN
        RETURN Error("Invalid frontmatter syntax")
    END IF

    // Remaining content after frontmatter
    remaining ← lines[(i+1)..].join("\n")

    RETURN {data: data, remaining: remaining}
END

TIME COMPLEXITY: O(n) where n = content length
SPACE COMPLEXITY: O(n) for line storage
```

### RDF Metadata Parsing

```
ALGORITHM: ParseRDFMetadata
INPUT: rdf_section (Map<String, Any>)
OUTPUT: RDFMetadata

BEGIN
    metadata ← RDFMetadata.new()

    // Extract subject URI (defaults to template file URI)
    IF rdf_section contains "subject" THEN
        metadata.subject ← ParseURI(rdf_section["subject"])
    END IF

    // Extract namespace prefixes
    IF rdf_section contains "namespaces" THEN
        FOR EACH (prefix, uri) IN rdf_section["namespaces"] DO
            metadata.namespaces[prefix] ← ParseURI(uri)
        END FOR
    END IF

    // Parse RDF triples
    IF rdf_section contains "triples" THEN
        FOR EACH triple_data IN rdf_section["triples"] DO
            triple ← ParseTriple(triple_data, metadata.namespaces)
            metadata.triples.append(triple)
        END FOR
    END IF

    // Parse SHACL shapes for validation
    IF rdf_section contains "shapes" THEN
        FOR EACH shape_data IN rdf_section["shapes"] DO
            shape ← ParseSHACLShape(shape_data, metadata.namespaces)
            metadata.shapes.append(shape)
        END FOR
    END IF

    RETURN metadata
END

TIME COMPLEXITY: O(t) where t = number of triples
SPACE COMPLEXITY: O(t) for triple storage
```

### Template Directive Parsing

```
ALGORITHM: ParseTemplateDirectives
INPUT: template_body (String), metadata (RDFMetadata)
OUTPUT: TemplateAST

BEGIN
    ast ← TemplateAST.new()
    current_node ← ast.root

    // Tokenize template body
    tokens ← Tokenize(template_body)

    // Parse token stream
    i ← 0
    WHILE i < tokens.length DO
        token ← tokens[i]

        CASE token.type OF
            DirectiveStart:
                directive ← ParseDirective(tokens, i)
                i ← directive.end_index

                CASE directive.name OF
                    "file":
                        node ← CreateFileNode(directive)
                        current_node.children.append(node)

                    "directory":
                        node ← CreateDirectoryNode(directive)
                        current_node.children.append(node)
                        current_node ← node  // Enter directory

                    "end":
                        current_node ← current_node.parent  // Exit directory

                    "include":
                        included ← LoadAndParseTemplate(directive.args["path"])
                        current_node.children.append(included)

                    "if":
                        node ← CreateConditionalNode(directive)
                        current_node.children.append(node)
                        current_node ← node

                    "for":
                        node ← CreateLoopNode(directive)
                        current_node.children.append(node)
                        current_node ← node

                    OTHERWISE:
                        RETURN Error("Unknown directive: " + directive.name)
                END CASE

            Content:
                IF current_node.content is None THEN
                    current_node.content ← ContentTemplate.new()
                END IF
                current_node.content.append(token.value)

            Variable:
                var ← ParseVariable(token.value)
                current_node.variables.add(var.name)
                current_node.content.append(VariableRef(var))

            OTHERWISE:
                i ← i + 1
                CONTINUE
        END CASE

        i ← i + 1
    END WHILE

    IF current_node is not ast.root THEN
        RETURN Error("Unclosed directive block")
    END IF

    RETURN ast
END

TIME COMPLEXITY: O(n) where n = template body length
SPACE COMPLEXITY: O(n) for AST nodes
```

---

## File Tree Generator Algorithm

### Main Generation Algorithm

```
ALGORITHM: GenerateFileTree
INPUT: template_node (TemplateNode), context (TemplateContext)
OUTPUT: FileTreeNode or Error

BEGIN
    // Phase 1: Initialize RDF store
    rdf_store ← InitializeRDFStore(context)

    // Phase 2: Validate required variables
    missing_vars ← ValidateRequiredVariables(template_node, context)
    IF missing_vars is not empty THEN
        RETURN Error("Missing required variables: " + missing_vars)
    END IF

    // Phase 3: Generate file tree recursively
    output_root ← GenerateNode(template_node, context, context.output_base)

    // Phase 4: Validate RDF graph
    IF context.options.validate_rdf THEN
        validation_result ← ValidateRDFGraph(rdf_store)
        IF validation_result is Error THEN
            RETURN validation_result
        END IF
    END IF

    // Phase 5: Write files (unless dry run)
    IF NOT context.options.dry_run THEN
        WriteFileTree(output_root, context)
    END IF

    RETURN output_root
END

TIME COMPLEXITY: O(n * m) where n = nodes, m = avg content size
SPACE COMPLEXITY: O(n) for file tree structure
```

### Recursive Node Generation

```
ALGORITHM: GenerateNode
INPUT: template_node (TemplateNode), context (TemplateContext), parent_path (PathBuf)
OUTPUT: FileTreeNode or Error

BEGIN
    // Phase 1: Resolve path with variable substitution
    resolved_path ← ResolvePath(template_node.path, context, parent_path)

    // Phase 2: Check if node should be generated (conditionals)
    IF NOT ShouldGenerate(template_node, context) THEN
        RETURN None  // Skip this node
    END IF

    // Phase 3: Create file tree node
    output_node ← FileTreeNode.new()
    output_node.path ← resolved_path
    output_node.node_type ← template_node.node_type

    // Phase 4: Generate content (for files)
    IF template_node.node_type is File THEN
        content ← GenerateContent(template_node.content, context)
        output_node.content ← content

        // Set file permissions
        IF template_node.directives contains "permissions" THEN
            output_node.permissions ← ParsePermissions(
                template_node.directives["permissions"]
            )
        ELSE
            output_node.permissions ← DefaultPermissions(template_node.node_type)
        END IF
    END IF

    // Phase 5: Process RDF metadata
    rdf_uri ← GenerateRDFMetadata(template_node.metadata, output_node, context)
    output_node.rdf_uri ← rdf_uri

    // Phase 6: Generate children recursively
    IF template_node.node_type is Directory THEN
        FOR EACH child IN template_node.children DO
            // Check for loop directive
            IF child.directives contains "for" THEN
                loop_items ← EvaluateLoop(child.directives["for"], context)

                FOR EACH item IN loop_items DO
                    // Create new context with loop variable
                    loop_context ← context.clone()
                    loop_context.variables[child.directives["for"].var] ← item

                    child_node ← GenerateNode(child, loop_context, resolved_path)
                    IF child_node is not None THEN
                        output_node.children.append(child_node)
                    END IF
                END FOR
            ELSE
                child_node ← GenerateNode(child, context, resolved_path)
                IF child_node is not None THEN
                    output_node.children.append(child_node)
                END IF
            END IF
        END FOR
    END IF

    RETURN output_node
END

TIME COMPLEXITY: O(n) where n = total nodes in template tree
SPACE COMPLEXITY: O(d) where d = tree depth (recursion stack)
```

### Content Generation with Variable Substitution

```
ALGORITHM: GenerateContent
INPUT: content_template (ContentTemplate), context (TemplateContext)
OUTPUT: Bytes

BEGIN
    output ← StringBuffer.new()

    FOR EACH segment IN content_template.segments DO
        CASE segment.type OF
            Literal:
                output.append(segment.value)

            Variable:
                value ← ResolveVariable(segment.variable, context)
                IF value is None THEN
                    RETURN Error("Undefined variable: " + segment.variable)
                END IF
                output.append(FormatValue(value))

            Expression:
                result ← EvaluateExpression(segment.expression, context)
                output.append(FormatValue(result))

            FunctionCall:
                result ← CallTemplateFunction(
                    segment.function_name,
                    segment.arguments,
                    context
                )
                output.append(FormatValue(result))

            Include:
                included_content ← LoadAndGenerateInclude(segment.path, context)
                output.append(included_content)
        END CASE
    END FOR

    RETURN output.to_bytes()
END

TIME COMPLEXITY: O(m) where m = content length
SPACE COMPLEXITY: O(m) for output buffer
```

---

## RDF Metadata Processor

### RDF Graph Generation

```
ALGORITHM: GenerateRDFMetadata
INPUT: metadata (RDFMetadata), file_node (FileTreeNode), context (TemplateContext)
OUTPUT: URI (resource URI)

BEGIN
    // Phase 1: Determine subject URI
    IF metadata.subject is not None THEN
        subject ← metadata.subject
    ELSE
        // Generate URI from file path
        subject ← FilePathToURI(file_node.path, context.output_base)
    END IF

    // Phase 2: Add triples to RDF store
    FOR EACH triple IN metadata.triples DO
        // Resolve subject (handle variable substitution)
        resolved_subject ← ResolveRDFValue(triple.subject, context, subject)

        // Resolve predicate
        resolved_predicate ← ResolveURI(triple.predicate, metadata.namespaces)

        // Resolve object
        resolved_object ← ResolveRDFValue(triple.object, context, subject)

        // Add to Oxigraph store
        context.rdf_store.insert(
            resolved_subject,
            resolved_predicate,
            resolved_object
        )
    END FOR

    // Phase 3: Add automatic metadata triples
    AddAutomaticTriples(subject, file_node, context)

    RETURN subject
END

TIME COMPLEXITY: O(t) where t = number of triples
SPACE COMPLEXITY: O(1) - triples added to existing store
```

### Automatic RDF Triple Generation

```
ALGORITHM: AddAutomaticTriples
INPUT: subject (URI), file_node (FileTreeNode), context (TemplateContext)
OUTPUT: None (modifies RDF store)

BEGIN
    store ← context.rdf_store

    // Add file system metadata
    store.insert(subject, RDF::type, FileSystemResource)
    store.insert(subject, FS::path, Literal(file_node.path.to_string()))
    store.insert(subject, FS::created, Literal(file_node.metadata.generation_time))

    // Add generation provenance
    template_uri ← file_node.metadata.created_from
    store.insert(subject, PROV::wasGeneratedBy, template_uri)
    store.insert(subject, PROV::generatedAtTime, Literal(NOW()))

    // Add variable usage
    FOR EACH (var_name, var_value) IN file_node.metadata.variables_used DO
        var_node ← CreateBlankNode()
        store.insert(subject, TEMPLATE::usedVariable, var_node)
        store.insert(var_node, TEMPLATE::variableName, Literal(var_name))
        store.insert(var_node, TEMPLATE::variableValue, Literal(var_value))
    END FOR

    // Add parent-child relationships
    IF file_node.parent is not None THEN
        parent_uri ← FilePathToURI(file_node.parent.path, context.output_base)
        store.insert(subject, FS::parentDirectory, parent_uri)
        store.insert(parent_uri, FS::contains, subject)
    END IF

    // Add file type metadata
    IF file_node.node_type is File THEN
        file_type ← DetectFileType(file_node.path)
        store.insert(subject, FS::fileType, Literal(file_type))

        IF file_node.content is not None THEN
            content_hash ← SHA256(file_node.content)
            store.insert(subject, FS::contentHash, Literal(content_hash))
            store.insert(subject, FS::contentSize, Literal(file_node.content.length))
        END IF
    END IF
END

TIME COMPLEXITY: O(v + p) where v = variables, p = parent relationships
SPACE COMPLEXITY: O(1) - triples added to existing store
```

### SHACL Validation

```
ALGORITHM: ValidateRDFGraph
INPUT: rdf_store (OxigraphStore)
OUTPUT: ValidationResult or Error

BEGIN
    validation_report ← ValidationReport.new()

    // Load SHACL shapes from templates
    shapes ← rdf_store.query("
        SELECT ?shape WHERE {
            ?shape a sh:NodeShape .
        }
    ")

    FOR EACH shape IN shapes DO
        // Validate each shape
        violations ← ValidateShape(shape, rdf_store)

        IF violations is not empty THEN
            validation_report.violations.extend(violations)
        END IF
    END FOR

    IF validation_report.has_violations THEN
        RETURN Error("RDF validation failed: " + validation_report)
    ELSE
        RETURN Ok(validation_report)
    END IF
END

TIME COMPLEXITY: O(s * r) where s = shapes, r = resources
SPACE COMPLEXITY: O(v) where v = violations
```

---

## Variable Substitution Engine

### Variable Resolution

```
ALGORITHM: ResolveVariable
INPUT: variable_ref (String), context (TemplateContext)
OUTPUT: Value or None

BEGIN
    // Phase 1: Parse variable path (supports nested access)
    path_segments ← variable_ref.split(".")

    // Phase 2: Resolve root variable
    root_name ← path_segments[0]

    IF context.variables contains root_name THEN
        current_value ← context.variables[root_name]
    ELSE IF context.environment contains root_name THEN
        current_value ← context.environment[root_name]
    ELSE
        RETURN None  // Variable not found
    END IF

    // Phase 3: Traverse nested path
    FOR i ← 1 TO path_segments.length - 1 DO
        segment ← path_segments[i]

        IF current_value is Map AND current_value contains segment THEN
            current_value ← current_value[segment]
        ELSE IF current_value is Object AND current_value has_field segment THEN
            current_value ← current_value.get_field(segment)
        ELSE
            RETURN None  // Path segment not found
        END IF
    END FOR

    RETURN current_value
END

TIME COMPLEXITY: O(p) where p = path depth
SPACE COMPLEXITY: O(1)
```

### Expression Evaluation

```
ALGORITHM: EvaluateExpression
INPUT: expression (Expression), context (TemplateContext)
OUTPUT: Value or Error

BEGIN
    CASE expression.type OF
        Literal:
            RETURN expression.value

        Variable:
            value ← ResolveVariable(expression.variable, context)
            IF value is None THEN
                RETURN Error("Undefined variable: " + expression.variable)
            END IF
            RETURN value

        BinaryOperation:
            left ← EvaluateExpression(expression.left, context)
            right ← EvaluateExpression(expression.right, context)

            CASE expression.operator OF
                "+": RETURN left + right
                "-": RETURN left - right
                "*": RETURN left * right
                "/": RETURN left / right
                "==": RETURN left == right
                "!=": RETURN left != right
                ">": RETURN left > right
                "<": RETURN left < right
                ">=": RETURN left >= right
                "<=": RETURN left <= right
                "and": RETURN left AND right
                "or": RETURN left OR right
            END CASE

        UnaryOperation:
            operand ← EvaluateExpression(expression.operand, context)

            CASE expression.operator OF
                "-": RETURN -operand
                "not": RETURN NOT operand
            END CASE

        FunctionCall:
            RETURN CallTemplateFunction(
                expression.function_name,
                expression.arguments,
                context
            )

        Conditional:
            condition ← EvaluateExpression(expression.condition, context)
            IF condition THEN
                RETURN EvaluateExpression(expression.then_branch, context)
            ELSE
                RETURN EvaluateExpression(expression.else_branch, context)
            END IF
    END CASE
END

TIME COMPLEXITY: O(e) where e = expression tree size
SPACE COMPLEXITY: O(d) where d = expression depth (recursion)
```

---

## Template Composition

### Template Inheritance

```
ALGORITHM: ComposeTemplates
INPUT: child_template (TemplateNode), parent_template (TemplateNode)
OUTPUT: TemplateNode (composed)

BEGIN
    composed ← TemplateNode.clone(child_template)

    // Phase 1: Merge metadata
    composed.metadata ← MergeRDFMetadata(
        parent_template.metadata,
        child_template.metadata
    )

    // Phase 2: Merge directives (child overrides parent)
    FOR EACH (key, value) IN parent_template.directives DO
        IF NOT composed.directives contains key THEN
            composed.directives[key] ← value
        END IF
    END FOR

    // Phase 3: Merge children
    parent_children_map ← MapBy(parent_template.children, node => node.path)

    FOR EACH child IN composed.children DO
        IF child.directives contains "override" THEN
            // Child explicitly overrides parent
            CONTINUE
        END IF

        IF parent_children_map contains child.path THEN
            // Recursively compose matching children
            parent_child ← parent_children_map[child.path]
            child ← ComposeTemplates(child, parent_child)
        END IF
    END FOR

    // Phase 4: Add parent children not in child
    FOR EACH parent_child IN parent_template.children DO
        IF NOT composed contains child with path parent_child.path THEN
            IF parent_child.directives does not contain "override-only" THEN
                composed.children.append(parent_child.clone())
            END IF
        END IF
    END FOR

    RETURN composed
END

TIME COMPLEXITY: O(n + m) where n, m = node counts
SPACE COMPLEXITY: O(n) for composed tree
```

### RDF Metadata Merging

```
ALGORITHM: MergeRDFMetadata
INPUT: base (RDFMetadata), overlay (RDFMetadata)
OUTPUT: RDFMetadata (merged)

BEGIN
    merged ← RDFMetadata.new()

    // Merge namespaces (overlay takes precedence)
    merged.namespaces ← base.namespaces.clone()
    FOR EACH (prefix, uri) IN overlay.namespaces DO
        merged.namespaces[prefix] ← uri
    END FOR

    // Use overlay subject if present, otherwise base
    IF overlay.subject is not None THEN
        merged.subject ← overlay.subject
    ELSE
        merged.subject ← base.subject
    END IF

    // Merge triples (remove duplicates)
    triple_set ← Set.new()

    FOR EACH triple IN base.triples DO
        triple_set.insert(triple)
    END FOR

    FOR EACH triple IN overlay.triples DO
        IF triple.predicate ends with "#replace" THEN
            // Remove all triples with this predicate from base
            predicate_to_replace ← RemoveSuffix(triple.predicate, "#replace")
            triple_set.remove_where(t => t.predicate == predicate_to_replace)

            // Add new triple without #replace suffix
            new_triple ← Triple(triple.subject, predicate_to_replace, triple.object)
            triple_set.insert(new_triple)
        ELSE
            triple_set.insert(triple)
        END IF
    END FOR

    merged.triples ← triple_set.to_list()

    // Merge SHACL shapes
    merged.shapes ← base.shapes.clone()
    merged.shapes.extend(overlay.shapes)

    RETURN merged
END

TIME COMPLEXITY: O(t1 + t2) where t1, t2 = triple counts
SPACE COMPLEXITY: O(t1 + t2) for merged triples
```

---

## Validation Algorithm

### Template Validation

```
ALGORITHM: ValidateTemplate
INPUT: template_node (TemplateNode)
OUTPUT: ValidationResult or Error

BEGIN
    errors ← []
    warnings ← []

    // Phase 1: Validate path template
    path_result ← ValidatePathTemplate(template_node.path)
    IF path_result is Error THEN
        errors.append(path_result)
    END IF

    // Phase 2: Validate variable references
    FOR EACH var IN template_node.variables DO
        IF NOT IsValidVariableName(var) THEN
            errors.append("Invalid variable name: " + var)
        END IF
    END FOR

    // Phase 3: Validate RDF metadata
    rdf_result ← ValidateRDFMetadata(template_node.metadata)
    IF rdf_result has errors THEN
        errors.extend(rdf_result.errors)
    END IF

    // Phase 4: Validate directives
    FOR EACH (directive, value) IN template_node.directives DO
        directive_result ← ValidateDirective(directive, value)
        IF directive_result is Error THEN
            errors.append(directive_result)
        END IF
    END FOR

    // Phase 5: Validate children recursively
    IF template_node.node_type is Directory THEN
        child_paths ← Set.new()

        FOR EACH child IN template_node.children DO
            // Check for duplicate paths
            IF child_paths contains child.path THEN
                errors.append("Duplicate child path: " + child.path)
            ELSE
                child_paths.insert(child.path)
            END IF

            // Recursively validate child
            child_result ← ValidateTemplate(child)
            IF child_result has errors THEN
                errors.extend(child_result.errors)
            END IF
            IF child_result has warnings THEN
                warnings.extend(child_result.warnings)
            END IF
        END FOR
    END IF

    // Phase 6: Validate content template
    IF template_node.content is not None THEN
        content_result ← ValidateContentTemplate(template_node.content)
        IF content_result has errors THEN
            errors.extend(content_result.errors)
        END IF
    END IF

    // Return validation result
    IF errors is not empty THEN
        RETURN Error(ValidationReport(errors, warnings))
    ELSE
        RETURN Ok(ValidationReport([], warnings))
    END IF
END

TIME COMPLEXITY: O(n * v) where n = nodes, v = validations per node
SPACE COMPLEXITY: O(n) for validation results
```

### Path Template Validation

```
ALGORITHM: ValidatePathTemplate
INPUT: path_template (PathTemplate)
OUTPUT: ValidationResult or Error

BEGIN
    errors ← []

    FOR EACH segment IN path_template.segments DO
        CASE segment.type OF
            Literal:
                // Check for invalid characters
                IF segment.literal contains InvalidPathChars THEN
                    errors.append("Invalid path characters: " + segment.literal)
                END IF

                // Check for reserved names
                IF segment.literal in [".", "..", ""] THEN
                    errors.append("Reserved path segment: " + segment.literal)
                END IF

            Variable:
                // Validate variable reference
                IF NOT IsValidVariableName(segment.variable) THEN
                    errors.append("Invalid variable: " + segment.variable)
                END IF

            Expression:
                // Validate expression syntax
                expr_result ← ValidateExpression(segment.expression)
                IF expr_result is Error THEN
                    errors.append(expr_result)
                END IF
        END CASE
    END FOR

    // Check for absolute path issues
    IF path_template.is_absolute THEN
        // Warn about absolute paths (may be system-specific)
        RETURN Warning("Absolute path may not be portable")
    END IF

    IF errors is empty THEN
        RETURN Ok()
    ELSE
        RETURN Error(errors)
    END IF
END

TIME COMPLEXITY: O(s) where s = number of segments
SPACE COMPLEXITY: O(1)
```

---

## Complexity Analysis

### Overall System Complexity

**Template Parsing:**
- Time: O(n) where n = template file size
- Space: O(n) for AST storage
- Optimization: Single-pass parsing, no backtracking

**File Tree Generation:**
- Time: O(n * m) where n = nodes, m = avg content size
- Space: O(n) for file tree structure
- Optimization: Streaming generation, lazy evaluation

**RDF Processing:**
- Time: O(t) where t = total triples
- Space: O(t) for RDF store
- Optimization: Indexed store (Oxigraph), incremental insertion

**Variable Substitution:**
- Time: O(v * p) where v = variables, p = avg path depth
- Space: O(1) per variable resolution
- Optimization: Context caching, early variable validation

**Template Composition:**
- Time: O(n + m) where n, m = node counts
- Space: O(n) for composed tree
- Optimization: Hash-based child matching

**Validation:**
- Time: O(n * v) where n = nodes, v = validations per node
- Space: O(n) for validation results
- Optimization: Early termination on errors, parallel validation possible

### Performance Optimizations

**1. Streaming Generation:**
```
OPTIMIZATION: Stream large file content instead of loading into memory
- Use iterator-based content generation
- Write files incrementally
- Memory usage: O(buffer_size) instead of O(total_size)
```

**2. Incremental RDF:**
```
OPTIMIZATION: Build RDF graph incrementally during generation
- Insert triples as files are generated
- Avoid separate RDF processing pass
- Reduces memory pressure
```

**3. Variable Caching:**
```
OPTIMIZATION: Cache resolved variable values
- Memoize variable lookups within generation
- Clear cache between independent generations
- Trade space for time: O(v) space for O(1) lookups
```

**4. Parallel Generation:**
```
OPTIMIZATION: Generate independent file trees in parallel
- Identify independent subtrees
- Use thread pool for parallel generation
- Synchronize RDF store access
- Speedup: Up to O(1/cores) with n independent subtrees
```

### Edge Case Handling

**Circular Template Inclusion:**
- Detect cycles using visited set during inclusion
- Return error on circular dependency
- Cost: O(i) where i = inclusion depth

**Missing Variables:**
- Validate all required variables before generation
- Fail fast with clear error messages
- Cost: O(v) where v = total variables

**RDF Validation Failures:**
- Collect all violations before failing
- Provide detailed SHACL violation reports
- Cost: O(s * r) where s = shapes, r = resources

**File System Conflicts:**
- Check for existing files before writing
- Offer merge/overwrite strategies
- Cost: O(n) stat calls where n = generated files

---

## Implementation Notes

**Recommended Data Flow:**
```
1. Parse template → TemplateNode AST
2. Validate template structure → Early error detection
3. Initialize RDF store → Prepare for metadata
4. Generate file tree → Recursive traversal with RDF insertion
5. Validate RDF graph → SHACL validation
6. Write files → Streaming I/O
```

**Error Handling Strategy:**
- Use Result<T, Error> for all operations
- Collect multiple errors where possible (validation)
- Fail fast for critical errors (parse failures)
- Provide context in error messages

**Testing Strategy:**
- Unit test each algorithm independently
- Property-based testing for parsers
- Integration tests for full pipeline
- Performance benchmarks for large templates

**Future Optimizations:**
- Parallel file tree generation
- Template compilation (pre-parse and cache)
- Lazy RDF insertion (defer until validation)
- Smart template diffing (incremental updates)

---

**END OF PSEUDOCODE SPECIFICATION**
