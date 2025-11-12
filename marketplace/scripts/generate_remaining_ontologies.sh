#!/usr/bin/env bash
# Generate RDF ontologies for remaining 4 packages
set -euo pipefail

PACKAGES=("agent-cli-copilot" "agent-context-crafter" "agent-memory-forge" "agent-reasoning-mcp")
BASE_DIR="marketplace/packages"

for pkg in "${PACKAGES[@]}"; do
    PKG_DIR="$BASE_DIR/$pkg"
    mkdir -p "$PKG_DIR/rdf"

    # Generate ontology based on package type
    case $pkg in
        "agent-cli-copilot")
            cat > "$PKG_DIR/rdf/ontology.ttl" <<'EOFONTOLOGY'
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix dc: <http://purl.org/dc/elements/1.1/> .
@prefix cli: <http://ggen.io/ontology/cli-copilot#> .

# Ontology definition
cli:CLICopilotOntology a owl:Ontology ;
    rdfs:label "CLI Copilot Ontology" ;
    dc:description "RDF ontology for intelligent CLI command assistance" ;
    dc:creator "ggen-team" ;
    owl:versionInfo "1.0.0" .

# Core Classes
cli:Copilot a owl:Class ; rdfs:label "CLI Copilot" ; rdfs:comment "Intelligent CLI assistant" .
cli:Command a owl:Class ; rdfs:label "Command" ; rdfs:comment "CLI command" .
cli:Shell a owl:Class ; rdfs:label "Shell" ; rdfs:comment "Command shell" .
cli:History a owl:Class ; rdfs:label "Command History" ; rdfs:comment "Historical command data" .
cli:SafetyCheck a owl:Class ; rdfs:label "Safety Check" ; rdfs:comment "Command safety validation" .
cli:Suggestion a owl:Class ; rdfs:label "Suggestion" ; rdfs:comment "Command suggestion" .
cli:Translation a owl:Class ; rdfs:label "Translation" ; rdfs:comment "Natural language to command translation" .
cli:Workflow a owl:Class ; rdfs:label "Workflow" ; rdfs:comment "Command workflow automation" .

# Shell instances
cli:Bash a cli:Shell ; rdfs:label "Bash" ; cli:shellPath "/bin/bash" .
cli:Zsh a cli:Shell ; rdfs:label "Zsh" ; cli:shellPath "/bin/zsh" .
cli:Fish a cli:Shell ; rdfs:label "Fish" ; cli:shellPath "/usr/bin/fish" .
cli:PowerShell a cli:Shell ; rdfs:label "PowerShell" ; cli:shellPath "pwsh" .

# Properties
cli:hasShell a owl:ObjectProperty ; rdfs:domain cli:Copilot ; rdfs:range cli:Shell .
cli:executes a owl:ObjectProperty ; rdfs:domain cli:Copilot ; rdfs:range cli:Command .
cli:suggests a owl:ObjectProperty ; rdfs:domain cli:Copilot ; rdfs:range cli:Suggestion .
cli:validates a owl:ObjectProperty ; rdfs:domain cli:Copilot ; rdfs:range cli:SafetyCheck .
cli:commandText a owl:DatatypeProperty ; rdfs:domain cli:Command ; rdfs:range xsd:string .
cli:description a owl:DatatypeProperty ; rdfs:domain cli:Command ; rdfs:range xsd:string .
cli:isDangerous a owl:DatatypeProperty ; rdfs:domain cli:Command ; rdfs:range xsd:boolean .
cli:requiresConfirmation a owl:DatatypeProperty ; rdfs:domain cli:Command ; rdfs:range xsd:boolean .
cli:successRate a owl:DatatypeProperty ; rdfs:domain cli:Command ; rdfs:range xsd:float .
cli:timestamp a owl:DatatypeProperty ; rdfs:domain cli:Command ; rdfs:range xsd:dateTime .
cli:userInput a owl:DatatypeProperty ; rdfs:domain cli:Translation ; rdfs:range xsd:string .
cli:translatedCommand a owl:DatatypeProperty ; rdfs:domain cli:Translation ; rdfs:range xsd:string .
cli:confidence a owl:DatatypeProperty ; rdfs:domain cli:Translation ; rdfs:range xsd:float .

# Safety Checks
cli:PermissionCheck a cli:SafetyCheck ; rdfs:label "Permission Check" ; cli:description "Validate file/directory permissions" .
cli:PathValidation a cli:SafetyCheck ; rdfs:label "Path Validation" ; cli:description "Validate file paths" .
cli:DestructiveOpCheck a cli:SafetyCheck ; rdfs:label "Destructive Operation Check" ; cli:description "Detect destructive operations (rm, dd, etc.)" .
cli:NetworkCheck a cli:SafetyCheck ; rdfs:label "Network Check" ; cli:description "Validate network operations" .
cli:SystemCheck a cli:SafetyCheck ; rdfs:label "System Check" ; cli:description "Validate system-level operations" .

# Command Categories
cli:FileOperation a cli:Command ; rdfs:label "File Operation" ; cli:description "File/directory manipulation" .
cli:GitOperation a cli:Command ; rdfs:label "Git Operation" ; cli:description "Git version control" .
cli:PackageManager a cli:Command ; rdfs:label "Package Manager" ; cli:description "Package management (npm, cargo, pip)" .
cli:DockerCommand a cli:Command ; rdfs:label "Docker Command" ; cli:description "Docker containerization" .
cli:KubernetesCommand a cli:Command ; rdfs:label "Kubernetes Command" ; cli:description "Kubernetes orchestration" .
cli:NetworkCommand a cli:Command ; rdfs:label "Network Command" ; cli:description "Network utilities" .
cli:SystemCommand a cli:Command ; rdfs:label "System Command" ; cli:description "System administration" .

# Workflow Patterns
cli:Pipeline a cli:Workflow ; rdfs:label "Pipeline" ; cli:description "Command pipeline automation" .
cli:Script a cli:Workflow ; rdfs:label "Script" ; cli:description "Shell script generation" .
cli:Alias a cli:Workflow ; rdfs:label "Alias" ; cli:description "Command alias creation" .
cli:Function a cli:Workflow ; rdfs:label "Function" ; cli:description "Shell function generation" .
EOFONTOLOGY
            ;;

        "agent-context-crafter")
            cat > "$PKG_DIR/rdf/ontology.ttl" <<'EOFONTOLOGY'
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix dc: <http://purl.org/dc/elements/1.1/> .
@prefix ctx: <http://ggen.io/ontology/context#> .

# Ontology definition
ctx:ContextOntology a owl:Ontology ;
    rdfs:label "Context Crafter Ontology" ;
    dc:description "RDF ontology for AI agent context and state management" ;
    dc:creator "ggen-team" ;
    owl:versionInfo "1.0.0" .

# Core Classes
ctx:Context a owl:Class ; rdfs:label "Context" ; rdfs:comment "Contextual state information" .
ctx:ContextGraph a owl:Class ; rdfs:label "Context Graph" ; rdfs:comment "Semantic context graph" .
ctx:State a owl:Class ; rdfs:label "State" ; rdfs:comment "Agent state representation" .
ctx:Session a owl:Class ; rdfs:label "Session" ; rdfs:comment "Conversation or interaction session" .
ctx:Snapshot a owl:Class ; rdfs:label "Snapshot" ; rdfs:comment "Context snapshot in time" .
ctx:ContextNode a owl:Class ; rdfs:label "Context Node" ; rdfs:comment "Node in context graph" .
ctx:ContextEdge a owl:Class ; rdfs:label "Context Edge" ; rdfs:comment "Relationship in context graph" .
ctx:Summary a owl:Class ; rdfs:label "Summary" ; rdfs:comment "Context summarization" .
ctx:Filter a owl:Class ; rdfs:label "Filter" ; rdfs:comment "Relevance filter" .
ctx:Version a owl:Class ; rdfs:label "Version" ; rdfs:comment "Context version" .

# Context Types
ctx:ConversationContext a ctx:Context ; rdfs:label "Conversation" ; ctx:description "Dialogue context" .
ctx:CodeContext a ctx:Context ; rdfs:label "Code" ; ctx:description "Code editing context" .
ctx:FileContext a ctx:Context ; rdfs:label "File" ; ctx:description "File system context" .
ctx:TaskContext a ctx:Context ; rdfs:label "Task" ; ctx:description "Task execution context" .
ctx:DecisionContext a ctx:Context ; rdfs:label "Decision" ; ctx:description "Decision-making context" .
ctx:ErrorContext a ctx:Context ; rdfs:label "Error" ; ctx:description "Error handling context" .

# Properties
ctx:hasContext a owl:ObjectProperty ; rdfs:domain owl:Thing ; rdfs:range ctx:Context .
ctx:hasState a owl:ObjectProperty ; rdfs:domain ctx:Context ; rdfs:range ctx:State .
ctx:hasSession a owl:ObjectProperty ; rdfs:domain ctx:Context ; rdfs:range ctx:Session .
ctx:hasSnapshot a owl:ObjectProperty ; rdfs:domain ctx:Session ; rdfs:range ctx:Snapshot .
ctx:hasNode a owl:ObjectProperty ; rdfs:domain ctx:ContextGraph ; rdfs:range ctx:ContextNode .
ctx:hasEdge a owl:ObjectProperty ; rdfs:domain ctx:ContextGraph ; rdfs:range ctx:ContextEdge .
ctx:connectsFrom a owl:ObjectProperty ; rdfs:domain ctx:ContextEdge ; rdfs:range ctx:ContextNode .
ctx:connectsTo a owl:ObjectProperty ; rdfs:domain ctx:ContextEdge ; rdfs:range ctx:ContextNode .
ctx:hasSummary a owl:ObjectProperty ; rdfs:domain ctx:Context ; rdfs:range ctx:Summary .
ctx:hasVersion a owl:ObjectProperty ; rdfs:domain ctx:Context ; rdfs:range ctx:Version .

# Data Properties
ctx:contextId a owl:DatatypeProperty ; rdfs:domain ctx:Context ; rdfs:range xsd:string .
ctx:sessionId a owl:DatatypeProperty ; rdfs:domain ctx:Session ; rdfs:range xsd:string .
ctx:timestamp a owl:DatatypeProperty ; rdfs:domain ctx:Context ; rdfs:range xsd:dateTime .
ctx:relevanceScore a owl:DatatypeProperty ; rdfs:domain ctx:Context ; rdfs:range xsd:float .
ctx:compressed a owl:DatatypeProperty ; rdfs:domain ctx:Context ; rdfs:range xsd:boolean .
ctx:persistent a owl:DatatypeProperty ; rdfs:domain ctx:Context ; rdfs:range xsd:boolean .
ctx:shared a owl:DatatypeProperty ; rdfs:domain ctx:Context ; rdfs:range xsd:boolean .
ctx:encrypted a owl:DatatypeProperty ; rdfs:domain ctx:Context ; rdfs:range xsd:boolean .
ctx:versionNumber a owl:DatatypeProperty ; rdfs:domain ctx:Version ; rdfs:range xsd:integer .
ctx:summaryText a owl:DatatypeProperty ; rdfs:domain ctx:Summary ; rdfs:range xsd:string .
ctx:compressionRatio a owl:DatatypeProperty ; rdfs:domain ctx:Summary ; rdfs:range xsd:float .

# Storage Backends
ctx:RDFStore a owl:Class ; rdfs:label "RDF Store" ; ctx:description "Triple store backend" .
ctx:VectorDB a owl:Class ; rdfs:label "Vector Database" ; ctx:description "Vector similarity backend" .
ctx:SQLite a owl:Class ; rdfs:label "SQLite" ; ctx:description "SQLite backend" .
ctx:Redis a owl:Class ; rdfs:label "Redis" ; ctx:description "Redis cache backend" .

# Operations
ctx:Persist a owl:Class ; rdfs:label "Persist" ; ctx:description "Save context to storage" .
ctx:Load a owl:Class ; rdfs:label "Load" ; ctx:description "Load context from storage" .
ctx:Compress a owl:Class ; rdfs:label "Compress" ; ctx:description "Compress context data" .
ctx:Summarize a owl:Class ; rdfs:label "Summarize" ; ctx:description "Create context summary" .
ctx:Filter a owl:Class ; rdfs:label "Filter" ; ctx:description "Filter by relevance" .
ctx:Merge a owl:Class ; rdfs:label "Merge" ; ctx:description "Merge contexts" .
ctx:Share a owl:Class ; rdfs:label "Share" ; ctx:description "Share context with agents" .
ctx:Rollback a owl:Class ; rdfs:label "Rollback" ; ctx:description "Rollback to previous version" .
EOFONTOLOGY
            ;;

        "agent-memory-forge")
            cat > "$PKG_DIR/rdf/ontology.ttl" <<'EOFONTOLOGY'
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix dc: <http://purl.org/dc/elements/1.1/> .
@prefix mem: <http://ggen.io/ontology/memory#> .

# Ontology definition
mem:MemoryOntology a owl:Ontology ;
    rdfs:label "Memory Forge Ontology" ;
    dc:description "RDF ontology for AI agent memory systems" ;
    dc:creator "ggen-team" ;
    owl:versionInfo "1.0.0" .

# Core Classes
mem:MemorySystem a owl:Class ; rdfs:label "Memory System" ; rdfs:comment "Complete memory system" .
mem:Memory a owl:Class ; rdfs:label "Memory" ; rdfs:comment "Individual memory item" .
mem:EpisodicMemory a owl:Class ; rdfs:label "Episodic Memory" ; rdfs:comment "Event history memory" ; rdfs:subClassOf mem:Memory .
mem:SemanticMemory a owl:Class ; rdfs:label "Semantic Memory" ; rdfs:comment "Knowledge graph memory" ; rdfs:subClassOf mem:Memory .
mem:ProceduralMemory a owl:Class ; rdfs:label "Procedural Memory" ; rdfs:comment "Learned workflows" ; rdfs:subClassOf mem:Memory .
mem:WorkingMemory a owl:Class ; rdfs:label "Working Memory" ; rdfs:comment "Short-term active memory" ; rdfs:subClassOf mem:Memory .
mem:LongTermMemory a owl:Class ; rdfs:label "Long-term Memory" ; rdfs:comment "Consolidated permanent memory" ; rdfs:subClassOf mem:Memory .

# Memory Components
mem:MemoryIndex a owl:Class ; rdfs:label "Memory Index" ; rdfs:comment "Memory indexing structure" .
mem:MemoryRetrieval a owl:Class ; rdfs:label "Memory Retrieval" ; rdfs:comment "Memory retrieval mechanism" .
mem:MemoryConsolidation a owl:Class ; rdfs:label "Memory Consolidation" ; rdfs:comment "Long-term memory consolidation" .
mem:MemoryDecay a owl:Class ; rdfs:label "Memory Decay" ; rdfs:comment "Memory decay function" .
mem:ImportanceScore a owl:Class ; rdfs:label "Importance Score" ; rdfs:comment "Memory importance scoring" .

# Event Types (Episodic)
mem:Event a owl:Class ; rdfs:label "Event" ; rdfs:comment "Episodic event" .
mem:UserAction a mem:Event ; rdfs:label "User Action" ; mem:description "User-initiated action" .
mem:SystemEvent a mem:Event ; rdfs:label "System Event" ; mem:description "System-generated event" .
mem:ErrorEvent a mem:Event ; rdfs:label "Error Event" ; mem:description "Error or exception event" .
mem:SuccessEvent a mem:Event ; rdfs:label "Success Event" ; mem:description "Successful completion event" .

# Knowledge Types (Semantic)
mem:Concept a owl:Class ; rdfs:label "Concept" ; rdfs:comment "Knowledge concept" .
mem:Fact a owl:Class ; rdfs:label "Fact" ; rdfs:comment "Factual knowledge" .
mem:Relationship a owl:Class ; rdfs:label "Relationship" ; rdfs:comment "Semantic relationship" .
mem:Entity a owl:Class ; rdfs:label "Entity" ; rdfs:comment "Knowledge entity" .
mem:Attribute a owl:Class ; rdfs:label "Attribute" ; rdfs:comment "Entity attribute" .

# Workflow Types (Procedural)
mem:Procedure a owl:Class ; rdfs:label "Procedure" ; rdfs:comment "Learned procedure" .
mem:Pattern a owl:Class ; rdfs:label "Pattern" ; rdfs:comment "Behavioral pattern" .
mem:Strategy a owl:Class ; rdfs:label "Strategy" ; rdfs:comment "Problem-solving strategy" .
mem:Heuristic a owl:Class ; rdfs:label "Heuristic" ; rdfs:comment "Decision heuristic" .

# Properties
mem:hasMemory a owl:ObjectProperty ; rdfs:domain mem:MemorySystem ; rdfs:range mem:Memory .
mem:hasEvent a owl:ObjectProperty ; rdfs:domain mem:EpisodicMemory ; rdfs:range mem:Event .
mem:hasConcept a owl:ObjectProperty ; rdfs:domain mem:SemanticMemory ; rdfs:range mem:Concept .
mem:hasProcedure a owl:ObjectProperty ; rdfs:domain mem:ProceduralMemory ; rdfs:range mem:Procedure .
mem:relatesTo a owl:ObjectProperty ; rdfs:domain mem:Memory ; rdfs:range mem:Memory .
mem:strengthenedBy a owl:ObjectProperty ; rdfs:domain mem:Memory ; rdfs:range mem:Memory .

# Data Properties
mem:memoryId a owl:DatatypeProperty ; rdfs:domain mem:Memory ; rdfs:range xsd:string .
mem:createdAt a owl:DatatypeProperty ; rdfs:domain mem:Memory ; rdfs:range xsd:dateTime .
mem:lastAccessedAt a owl:DatatypeProperty ; rdfs:domain mem:Memory ; rdfs:range xsd:dateTime .
mem:accessCount a owl:DatatypeProperty ; rdfs:domain mem:Memory ; rdfs:range xsd:integer .
mem:importance a owl:DatatypeProperty ; rdfs:domain mem:Memory ; rdfs:range xsd:float .
mem:confidence a owl:DatatypeProperty ; rdfs:domain mem:Memory ; rdfs:range xsd:float .
mem:decayRate a owl:DatatypeProperty ; rdfs:domain mem:Memory ; rdfs:range xsd:float .
mem:strength a owl:DatatypeProperty ; rdfs:domain mem:Memory ; rdfs:range xsd:float .
mem:content a owl:DatatypeProperty ; rdfs:domain mem:Memory ; rdfs:range xsd:string .
mem:temporal a owl:DatatypeProperty ; rdfs:domain mem:Event ; rdfs:range xsd:dateTime .
mem:spatial a owl:DatatypeProperty ; rdfs:domain mem:Event ; rdfs:range xsd:string .
mem:contextual a owl:DatatypeProperty ; rdfs:domain mem:Event ; rdfs:range xsd:string .

# Storage Engines
mem:RDFTripleStore a owl:Class ; rdfs:label "RDF Triple Store" ; mem:description "RDF/SPARQL storage" .
mem:VectorDatabase a owl:Class ; rdfs:label "Vector Database" ; mem:description "Embedding-based storage" .
mem:RelationalDB a owl:Class ; rdfs:label "Relational Database" ; mem:description "SQL-based storage" .

# Operations
mem:Store a owl:Class ; rdfs:label "Store" ; mem:description "Store memory" .
mem:Retrieve a owl:Class ; rdfs:label "Retrieve" ; mem:description "Retrieve memory by query" .
mem:Update a owl:Class ; rdfs:label "Update" ; mem:description "Update existing memory" .
mem:Delete a owl:Class ; rdfs:label "Delete" ; mem:description "Delete memory" .
mem:Consolidate a owl:Class ; rdfs:label "Consolidate" ; mem:description "Consolidate to long-term" .
mem:Prune a owl:Class ; rdfs:label "Prune" ; mem:description "Prune low-importance memories" .
mem:Refresh a owl:Class ; rdfs:label "Refresh" ; mem:description "Refresh memory strength" .
mem:Search a owl:Class ; rdfs:label "Search" ; mem:description "Similarity-based search" .
EOFONTOLOGY
            ;;

        "agent-reasoning-mcp")
            cat > "$PKG_DIR/rdf/ontology.ttl" <<'EOFONTOLOGY'
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix dc: <http://purl.org/dc/elements/1.1/> .
@prefix reason: <http://ggen.io/ontology/reasoning#> .

# Ontology definition
reason:ReasoningOntology a owl:Ontology ;
    rdfs:label "Reasoning MCP Ontology" ;
    dc:description "RDF ontology for AI agent reasoning and inference" ;
    dc:creator "ggen-team" ;
    owl:versionInfo "1.0.0" .

# Core Classes
reason:ReasoningEngine a owl:Class ; rdfs:label "Reasoning Engine" ; rdfs:comment "AI reasoning engine" .
reason:Reasoning a owl:Class ; rdfs:label "Reasoning" ; rdfs:comment "Reasoning process" .
reason:Inference a owl:Class ; rdfs:label "Inference" ; rdfs:comment "Logical inference" .
reason:Rule a owl:Class ; rdfs:label "Rule" ; rdfs:comment "Inference rule" .
reason:Fact a owl:Class ; rdfs:label "Fact" ; rdfs:comment "Known fact" .
reason:Conclusion a owl:Class ; rdfs:label "Conclusion" ; rdfs:comment "Inferred conclusion" .
reason:Proof a owl:Class ; rdfs:label "Proof" ; rdfs:comment "Reasoning proof" .
reason:Explanation a owl:Class ; rdfs:label "Explanation" ; rdfs:comment "Explanation of reasoning" .

# Reasoning Types
reason:DeductiveReasoning a reason:Reasoning ; rdfs:label "Deductive" ; reason:description "Deductive reasoning" .
reason:InductiveReasoning a reason:Reasoning ; rdfs:label "Inductive" ; reason:description "Inductive reasoning" .
reason:AbductiveReasoning a reason:Reasoning ; rdfs:label "Abductive" ; reason:description "Abductive reasoning" .
reason:AnalogicalReasoning a reason:Reasoning ; rdfs:label "Analogical" ; reason:description "Analogical reasoning" .
reason:CausalReasoning a reason:Reasoning ; rdfs:label "Causal" ; reason:description "Causal reasoning" .
reason:ProbabilisticReasoning a reason:Reasoning ; rdfs:label "Probabilistic" ; reason:description "Probabilistic reasoning" .
reason:TemporalReasoning a reason:Reasoning ; rdfs:label "Temporal" ; reason:description "Temporal reasoning" .
reason:SpatialReasoning a reason:Reasoning ; rdfs:label "Spatial" ; reason:description "Spatial reasoning" .

# Inference Engines
reason:ForwardChaining a owl:Class ; rdfs:label "Forward Chaining" ; reason:description "Forward chaining inference" .
reason:BackwardChaining a owl:Class ; rdfs:label "Backward Chaining" ; reason:description "Backward chaining inference" .
reason:SPARQLInference a owl:Class ; rdfs:label "SPARQL Inference" ; reason:description "SPARQL-based inference" .
reason:RDFEntailment a owl:Class ; rdfs:label "RDF Entailment" ; reason:description "RDF entailment" .
reason:OWLReasoner a owl:Class ; rdfs:label "OWL Reasoner" ; reason:description "OWL ontology reasoning" .

# Planning
reason:Plan a owl:Class ; rdfs:label "Plan" ; rdfs:comment "Execution plan" .
reason:Goal a owl:Class ; rdfs:label "Goal" ; rdfs:comment "Reasoning goal" .
reason:Subgoal a owl:Class ; rdfs:label "Subgoal" ; rdfs:comment "Decomposed subgoal" .
reason:Action a owl:Class ; rdfs:label "Action" ; rdfs:comment "Planned action" .
reason:Precondition a owl:Class ; rdfs:label "Precondition" ; rdfs:comment "Action precondition" .
reason:Postcondition a owl:Class ; rdfs:label "Postcondition" ; rdfs:comment "Action effect" .
reason:Constraint a owl:Class ; rdfs:label "Constraint" ; rdfs:comment "Planning constraint" .

# MCP Integration
reason:MCPTool a owl:Class ; rdfs:label "MCP Tool" ; rdfs:comment "MCP protocol tool" .
reason:MCPAgent a owl:Class ; rdfs:label "MCP Agent" ; rdfs:comment "MCP-enabled agent" .
reason:MCPMessage a owl:Class ; rdfs:label "MCP Message" ; rdfs:comment "MCP protocol message" .
reason:MCPTransport a owl:Class ; rdfs:label "MCP Transport" ; rdfs:comment "MCP transport layer" .

# Properties
reason:hasReasoning a owl:ObjectProperty ; rdfs:domain reason:ReasoningEngine ; rdfs:range reason:Reasoning .
reason:hasInference a owl:ObjectProperty ; rdfs:domain reason:Reasoning ; rdfs:range reason:Inference .
reason:hasRule a owl:ObjectProperty ; rdfs:domain reason:Inference ; rdfs:range reason:Rule .
reason:hasFact a owl:ObjectProperty ; rdfs:domain reason:Reasoning ; rdfs:range reason:Fact .
reason:hasConclusion a owl:ObjectProperty ; rdfs:domain reason:Reasoning ; rdfs:range reason:Conclusion .
reason:hasProof a owl:ObjectProperty ; rdfs:domain reason:Conclusion ; rdfs:range reason:Proof .
reason:hasExplanation a owl:ObjectProperty ; rdfs:domain reason:Reasoning ; rdfs:range reason:Explanation .
reason:hasGoal a owl:ObjectProperty ; rdfs:domain reason:Plan ; rdfs:range reason:Goal .
reason:hasSubgoal a owl:ObjectProperty ; rdfs:domain reason:Goal ; rdfs:range reason:Subgoal .
reason:hasAction a owl:ObjectProperty ; rdfs:domain reason:Plan ; rdfs:range reason:Action .
reason:hasPrecondition a owl:ObjectProperty ; rdfs:domain reason:Action ; rdfs:range reason:Precondition .
reason:hasPostcondition a owl:ObjectProperty ; rdfs:domain reason:Action ; rdfs:range reason:Postcondition .
reason:hasConstraint a owl:ObjectProperty ; rdfs:domain reason:Plan ; rdfs:range reason:Constraint .
reason:usesMCP a owl:ObjectProperty ; rdfs:domain reason:ReasoningEngine ; rdfs:range reason:MCPTool .

# Data Properties
reason:reasoningId a owl:DatatypeProperty ; rdfs:domain reason:Reasoning ; rdfs:range xsd:string .
reason:confidence a owl:DatatypeProperty ; rdfs:domain reason:Conclusion ; rdfs:range xsd:float .
reason:probability a owl:DatatypeProperty ; rdfs:domain reason:Conclusion ; rdfs:range xsd:float .
reason:truthValue a owl:DatatypeProperty ; rdfs:domain reason:Fact ; rdfs:range xsd:boolean .
reason:timestamp a owl:DatatypeProperty ; rdfs:domain reason:Reasoning ; rdfs:range xsd:dateTime .
reason:hops a owl:DatatypeProperty ; rdfs:domain reason:Inference ; rdfs:range xsd:integer ; rdfs:comment "Number of inference hops" .
reason:complexity a owl:DatatypeProperty ; rdfs:domain reason:Reasoning ; rdfs:range xsd:integer .
reason:cost a owl:DatatypeProperty ; rdfs:domain reason:Action ; rdfs:range xsd:float .
reason:priority a owl:DatatypeProperty ; rdfs:domain reason:Goal ; rdfs:range xsd:integer .

# Contradiction Detection
reason:Contradiction a owl:Class ; rdfs:label "Contradiction" ; reason:description "Detected contradiction" .
reason:Resolution a owl:Class ; rdfs:label "Resolution" ; reason:description "Contradiction resolution" .
reason:detectsContradiction a owl:ObjectProperty ; rdfs:domain reason:Reasoning ; rdfs:range reason:Contradiction .
reason:resolvesBy a owl:ObjectProperty ; rdfs:domain reason:Contradiction ; rdfs:range reason:Resolution .

# Explanation Types
reason:ProofTrace a reason:Explanation ; rdfs:label "Proof Trace" ; reason:description "Step-by-step proof" .
reason:NaturalLanguage a reason:Explanation ; rdfs:label "Natural Language" ; reason:description "Human-readable explanation" .
reason:Visualization a reason:Explanation ; rdfs:label "Visualization" ; reason:description "Visual explanation" .
reason:ConfidenceScore a reason:Explanation ; rdfs:label "Confidence Score" ; reason:description "Confidence-based explanation" .
EOFONTOLOGY
            ;;
    esac

    LINES=$(wc -l < "$PKG_DIR/rdf/ontology.ttl")
    echo "Generated $pkg: $LINES lines"
done

echo "✅ All RDF ontologies generated"
