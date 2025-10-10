<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [GGen Cookbook Examples](#ggen-cookbook-examples)
  - [Organization](#organization)
  - [Running Examples](#running-examples)
    - [Prerequisites](#prerequisites)
    - [Basic Usage](#basic-usage)
    - [Advanced Usage](#advanced-usage)
  - [Example Categories](#example-categories)
    - [Chapter Examples](#chapter-examples)
    - [Pattern Examples](#pattern-examples)
    - [Recipe Examples](#recipe-examples)
  - [Contributing Examples](#contributing-examples)
  - [Getting Help](#getting-help)
  - [License](#license)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# GGen Cookbook Examples

This directory contains runnable examples that demonstrate GGen's pattern language approach to code generation. All examples are tested, documented, and ready to use.

## Organization

Examples are organized by chapter and pattern:

- **`chapter-*/`**: Examples relevant to specific chapters
- **`patterns/`**: Examples demonstrating specific patterns
- **`recipes/`**: Complete, runnable recipes for common tasks

## Running Examples

### Prerequisites

1. Install GGen:
   ```bash
   cargo install ggen
   ```

2. Verify installation:
   ```bash
   ggen --version
   ```

### Basic Usage

1. Navigate to an example directory:
   ```bash
   cd examples/chapter-1
   ```

2. Run the example:
   ```bash
   ggen project gen pattern-language-example.tmpl
   ```

3. View generated output:
   ```bash
   cat generated_output.rs
   ```

### Advanced Usage

1. Use with custom variables:
   ```bash
   ggen project gen pattern-language-example.tmpl \
     --var user_name="Bob" \
     --var user_email="bob@example.com"
   ```

2. Use with knowledge graphs:
   ```bash
   ggen graph load domain-model.ttl
   ggen project gen knowledge-first-example.tmpl
   ```

3. Use with recipes:
   ```bash
   ggen project gen quick-start-api.tmpl \
     --var api_name="MyAPI" \
     --var resource_name="Product"
   ```

## Example Categories

### Chapter Examples

- **Chapter 1**: Pattern language fundamentals
- **Chapter 2**: Philosophy and vision
- **Chapter 3**: Engine architecture
- **Chapter 4**: CLI usage
- **Chapter 5**: Template anatomy
- **Chapter 6**: Core patterns
- **Chapter 7**: Advanced patterns
- **Chapter 8**: Data-driven generation
- **Chapter 9**: Best practices
- **Chapter 10**: Pattern catalog
- **Chapter 11**: Self-configuration
- **Chapter 12**: Self-optimization
- **Chapter 13**: Self-healing
- **Chapter 14**: Marketplace
- **Chapter 15**: Integration
- **Chapter 16**: Enterprise patterns
- **Chapter 17**: Extending GGen

### Pattern Examples

- **Pattern 001**: Knowledge-First Projection
- **Pattern 002**: Deterministic Engine
- **Pattern 003**: Graph-Template Binding
- **Pattern 004**: Noun-Verb CLI
- **Pattern 005**: Multi-Language Projection
- **Pattern 006**: Lockfile Versioning
- **Pattern 007**: Snapshot Testing
- **Pattern 008**: Incremental Generation
- **Pattern 009**: Template Inheritance
- **Pattern 010**: Conditional Generation

### Recipe Examples

- **Quick Start API**: Complete REST API generation
- **Database Schema**: Database-to-code generation
- **API Client**: Client generation from OpenAPI specs
- **Testing Suite**: Comprehensive test generation
- **Documentation**: Automated documentation generation
- **Deployment**: CI/CD pipeline generation
- **Monitoring**: Observability stack generation
- **Security**: Security scanning and compliance

## Contributing Examples

We welcome contributions of new examples:

1. Follow the existing structure and naming conventions
2. Include comprehensive documentation
3. Test examples thoroughly
4. Use the pattern language approach
5. Submit pull requests for review

## Getting Help

- **Documentation**: See the main cookbook for detailed explanations
- **Community**: Join the GGen community for support
- **Issues**: Report problems via GitHub issues
- **Discussions**: Use GitHub discussions for questions

## License

Examples are licensed under the same terms as GGen itself.
