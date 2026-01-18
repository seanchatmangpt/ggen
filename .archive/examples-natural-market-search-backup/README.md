# Natural Language Market Search

A standalone CLI tool for natural language search in the ggen marketplace using AI interpretation.

## Features

- **Natural Language Queries** - Search using conversational language
- **AI Interpretation** - Understands user intent and converts to search parameters
- **Smart Suggestions** - Provides related search suggestions
- **Multiple Output Formats** - Human-readable and JSON output
- **Confidence Scoring** - Shows AI confidence in interpretation
- **Detailed Reasoning** - Explains why packages were recommended

## Installation

```bash
cd examples/natural-market-search
cargo build --release
```

## Usage

### Basic Search

```bash
# Search for authentication packages
cargo run -- search --query "I need a user authentication system"

# Search for web frameworks
cargo run -- search --query "Find me web frameworks for APIs"

# Search for database tools
cargo run -- search --query "What packages help with database operations?"
```

### Advanced Options

```bash
# Show detailed output with AI reasoning
cargo run -- search --query "authentication system" --detailed --explain

# Output as JSON for programmatic use
cargo run -- search --query "web API" --json

# Limit number of results
cargo run -- search --query "CLI tools" --limit 5

# Use specific AI model
cargo run -- search --query "database ORM" --model "qwen3-coder:30b"
```

### Examples and Testing

```bash
# Show example queries
cargo run -- examples

# Test AI interpretation
cargo run -- test --query "I need OAuth2 integration"
```

## Example Queries

### Authentication & Security
- "I need a user authentication system"
- "Find me OAuth2 integration packages"
- "What's available for JWT tokens?"
- "I want role-based access control"

### Web Development
- "Find me web frameworks for APIs"
- "I need REST API tools"
- "Show me GraphQL servers"
- "What packages help with web middleware?"

### Database Operations
- "What packages help with database operations?"
- "I need an ORM for my project"
- "Find database migration tools"
- "Show me query builders"

### CLI Development
- "I want to build a CLI application"
- "Find command-line argument parsers"
- "What tools help with terminal interfaces?"

### Templates & Boilerplate
- "Show me templates for new projects"
- "I need boilerplate code generators"
- "Find project scaffolding tools"

## Output Formats

### Human-Readable Output

```
🤖 Natural language search using AI model: qwen3-coder:30b
Query: "I need a user authentication system"

🎯 AI Interpretation:
User is looking for authentication and authorization packages. This includes login systems, JWT tokens, OAuth2, and user management solutions.

🔍 Search Parameters:
  keywords: ["authentication"]
  search_terms: "I need a user authentication system"
  category: "security"
  sort: "relevance"

📦 Found 3 relevant packages (confidence: 60.0%)

1. 📦 User Authentication System (⭐ 95%)
   ID: io.ggen.auth.user
   Description: Complete user authentication with email/password, JWT tokens, and session management
   Category: security
   Tags: authentication, jwt, user
   AI Reasoning: Perfect match for authentication needs with comprehensive features

💡 Try these related searches:
   • Try: 'user management system'
   • Try: 'JWT token authentication'
   • Try: 'OAuth2 social login'

🔧 To use a package:
   ggen market add "io.ggen.auth.user"
```

### JSON Output

```json
{
  "query": "Find me web frameworks for APIs",
  "interpretation": "User is searching for web frameworks and API development tools...",
  "search_params": {
    "keywords": ["web", "api"],
    "category": "web",
    "sort": "relevance"
  },
  "results": [
    {
      "id": "io.ggen.web.rest",
      "name": "REST API Framework",
      "description": "Complete REST API framework...",
      "relevance_score": 0.92,
      "ai_reasoning": "Excellent choice for building REST APIs..."
    }
  ],
  "suggestions": [
    "Try: 'REST API framework'",
    "Try: 'GraphQL server'"
  ],
  "confidence": 0.8
}
```

## AI Interpretation

The tool uses AI to understand natural language queries and convert them into structured search parameters:

### Query Analysis
- **Intent Recognition** - Understands what the user is looking for
- **Keyword Extraction** - Identifies relevant technical terms
- **Category Mapping** - Maps queries to appropriate categories
- **Context Understanding** - Considers the broader context

### Confidence Scoring
- **Base Confidence** - Starting point (50%)
- **Technical Terms** - Increases confidence for specific terms
- **Query Specificity** - More specific queries get higher confidence
- **Maximum Cap** - Confidence capped at 95%

### Search Parameter Generation
- **Keywords** - Extracted from the query
- **Category** - Mapped from query intent
- **Sort Order** - Relevance-based sorting
- **Search Terms** - Original query preserved

## Integration with ggen

The natural language search tool is designed to work alongside the existing ggen marketplace:

```bash
# Use natural language search to find packages
natural-market-search search --query "I need authentication" --json

# Then use ggen to add the package
ggen market add "io.ggen.auth.user"

# Or use traditional search
ggen market search "authentication" --category security
```

## Mock Implementation

This is currently a mock implementation that demonstrates the concept. In a real implementation, it would:

1. **Use Ollama** - Connect to Ollama with qwen3-coder:30b model
2. **Real Marketplace** - Query actual marketplace data
3. **Advanced AI** - Use more sophisticated NLP models
4. **Learning** - Improve based on user feedback

## Dependencies

- `clap` - Command-line argument parsing
- `serde_json` - JSON serialization/deserialization
- `serde` - Serialization framework
- `tokio` - Async runtime

## Future Enhancements

- **Real AI Integration** - Connect to Ollama for actual AI processing
- **Marketplace Integration** - Query real marketplace data
- **Learning System** - Improve based on user interactions
- **Fuzzy Matching** - Handle typos and similar terms
- **Multi-language Support** - Support queries in different languages
- **Voice Input** - Support voice-based queries
- **Visual Results** - Rich terminal output with colors and formatting

## License

This project follows the same license as the main ggen project.

