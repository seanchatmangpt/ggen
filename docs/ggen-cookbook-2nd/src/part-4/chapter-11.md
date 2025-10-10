<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Chapter 11: Self-Configuration](#chapter-11-self-configuration)
  - [Overview](#overview)
  - [What You'll Learn](#what-youll-learn)
  - [Chapter Structure](#chapter-structure)
  - [Key Concepts](#key-concepts)
    - [**Self-Configuration**](#self-configuration)
    - [**Template Discovery**](#template-discovery)
    - [**Context Detection**](#context-detection)
    - [**Intelligent Defaults**](#intelligent-defaults)
  - [The Self-Configuration Loop](#the-self-configuration-loop)
  - [Context Detection](#context-detection-1)
    - [**Project Type Detection**](#project-type-detection)
    - [**Environment Detection**](#environment-detection)
    - [**Dependency Detection**](#dependency-detection)
  - [Template Discovery](#template-discovery-1)
    - [**Local Discovery**](#local-discovery)
    - [**Registry Discovery**](#registry-discovery)
    - [**Intelligent Selection**](#intelligent-selection)
  - [Intelligent Defaults](#intelligent-defaults-1)
    - [**Language-Specific Defaults**](#language-specific-defaults)
    - [**Framework-Specific Defaults**](#framework-specific-defaults)
    - [**Environment-Specific Defaults**](#environment-specific-defaults)
  - [Configuration Inheritance](#configuration-inheritance)
    - [**Global Configuration**](#global-configuration)
    - [**Project Configuration**](#project-configuration)
    - [**Inheritance Rules**](#inheritance-rules)
  - [Example: Automatic Web API Generation](#example-automatic-web-api-generation)
    - [**1. Context Detection**](#1-context-detection)
    - [**2. Template Discovery**](#2-template-discovery)
    - [**3. Default Application**](#3-default-application)
    - [**4. Configuration Validation**](#4-configuration-validation)
  - [Benefits of Self-Configuration](#benefits-of-self-configuration)
    - [**1. Reduced Cognitive Load**](#1-reduced-cognitive-load)
    - [**2. Consistency**](#2-consistency)
    - [**3. Best Practices**](#3-best-practices)
    - [**4. Adaptability**](#4-adaptability)
    - [**5. Error Prevention**](#5-error-prevention)
  - [Customization and Overrides](#customization-and-overrides)
    - [**Configuration Files**](#configuration-files)
    - [**Environment Variables**](#environment-variables)
    - [**Command Line Overrides**](#command-line-overrides)
  - [Best Practices](#best-practices)
    - [**1. Trust the Defaults**](#1-trust-the-defaults)
    - [**2. Document Overrides**](#2-document-overrides)
    - [**3. Use Hierarchical Configuration**](#3-use-hierarchical-configuration)
    - [**4. Validate Changes**](#4-validate-changes)
    - [**5. Monitor Configuration**](#5-monitor-configuration)
  - [Next Steps](#next-steps)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Chapter 11: Self-Configuration

## Overview

This chapter explores GGen's self-configuration capabilities - how the system automatically detects context, discovers templates, applies intelligent defaults, and manages configuration inheritance. These autonomic properties reduce the need for manual configuration and make GGen easier to use.

## What You'll Learn

- How GGen automatically discovers templates and context
- How intelligent defaults are applied based on detected context
- How configuration inheritance works across projects and templates
- How to customize and override automatic configuration

## Chapter Structure

- [11.1 Template Discovery](./chapter-11-1.md) - Automatic template finding and selection
- [11.2 Auto-Detection of Context](./chapter-11-2.md) - Detecting project type and requirements
- [11.3 Intelligent Defaults](./chapter-11-3.md) - Smart default selection and application
- [11.4 Configuration Inheritance](./chapter-11-4.md) - Hierarchical configuration management

## Key Concepts

### **Self-Configuration**
The ability of GGen to automatically configure itself based on detected context and available resources.

### **Template Discovery**
The process of automatically finding and selecting appropriate templates for a given context.

### **Context Detection**
The automatic identification of project type, requirements, and constraints.

### **Intelligent Defaults**
Smart default values that are chosen based on context and best practices.

## The Self-Configuration Loop

GGen's self-configuration follows a continuous loop:

```
┌─────────────────────────────────────────────────────────────┐
│                Self-Configuration Loop                      │
└─────────────────────────────────────────────────────────────┘

    ┌─────────────┐
    │   Detect    │  ← Analyze environment and context
    │  Context    │
    └──────┬──────┘
           │
           ▼
    ┌─────────────┐
    │  Discover   │  ← Find available templates and resources
    │ Templates   │
    └──────┬──────┘
           │
           ▼
    ┌─────────────┐
    │   Apply     │  ← Apply intelligent defaults and settings
    │  Defaults   │
    └──────┬──────┘
           │
           ▼
    ┌─────────────┐
    │  Validate   │  ← Ensure configuration is correct and complete
    │    Config   │
    └──────┬──────┘
           │
           └──────────┐
                      │
               (iterate as needed)
```

## Context Detection

GGen automatically detects various aspects of the current context:

### **Project Type Detection**
- **Language Detection**: Identifies primary programming language
- **Framework Detection**: Recognizes frameworks and libraries
- **Build System Detection**: Finds build tools and configurations
- **Test Framework Detection**: Identifies testing frameworks

### **Environment Detection**
- **Operating System**: Detects OS and architecture
- **Development Environment**: Identifies IDE, editor, and tools
- **CI/CD Environment**: Recognizes build and deployment systems
- **Cloud Environment**: Detects cloud platforms and services

### **Dependency Detection**
- **Package Managers**: Identifies package managers and lock files
- **Version Constraints**: Detects version requirements and constraints
- **Dependency Graphs**: Analyzes dependency relationships
- **Security Scans**: Identifies known vulnerabilities

## Template Discovery

GGen automatically discovers templates through multiple mechanisms:

### **Local Discovery**
- **File System Scanning**: Searches for `.tmpl` files in project directories
- **Pattern Matching**: Matches templates to detected context
- **Dependency Resolution**: Resolves template dependencies automatically
- **Version Selection**: Chooses appropriate template versions

### **Registry Discovery**
- **Remote Registry**: Searches GitHub and other registries
- **Semantic Search**: Finds templates based on functionality and context
- **Community Recommendations**: Uses community ratings and usage patterns
- **Dependency Analysis**: Analyzes template compatibility and dependencies

### **Intelligent Selection**
- **Context Matching**: Selects templates that match detected context
- **Quality Scoring**: Ranks templates by quality and reliability
- **Usage Patterns**: Considers community usage and success rates
- **Compatibility Checking**: Ensures templates work with current environment

## Intelligent Defaults

GGen applies intelligent defaults based on detected context:

### **Language-Specific Defaults**
- **Rust**: Uses `cargo` for builds, `rustfmt` for formatting, `clippy` for linting
- **Python**: Uses `pip` for packages, `black` for formatting, `pytest` for testing
- **TypeScript**: Uses `npm` for packages, `prettier` for formatting, `jest` for testing
- **Java**: Uses `maven` or `gradle` for builds, `checkstyle` for linting

### **Framework-Specific Defaults**
- **Web Frameworks**: Applies appropriate routing, middleware, and structure patterns
- **Database Frameworks**: Configures ORM settings, connection pools, and migrations
- **Testing Frameworks**: Sets up test structure, fixtures, and assertions
- **Documentation Frameworks**: Configures documentation generation and hosting

### **Environment-Specific Defaults**
- **Development**: Enables debugging, hot reloading, and development tools
- **Testing**: Configures test databases, mock services, and test data
- **Staging**: Sets up staging-specific configurations and monitoring
- **Production**: Applies security, performance, and reliability settings

## Configuration Inheritance

GGen uses hierarchical configuration inheritance:

### **Global Configuration**
- **User Settings**: Personal preferences and defaults
- **System Settings**: System-wide configurations and policies
- **Environment Variables**: Runtime environment settings
- **Command Line**: Explicit command-line overrides

### **Project Configuration**
- **Project Root**: Project-specific settings and templates
- **Subdirectories**: Directory-specific configurations
- **Template Configuration**: Template-specific settings and defaults
- **Generation Context**: Context-specific overrides

### **Inheritance Rules**
- **Override Precedence**: Command line > Project > Global
- **Merging Strategy**: Deep merge for complex configurations
- **Validation**: Ensures inherited configurations are valid
- **Conflict Resolution**: Resolves conflicting settings automatically

## Example: Automatic Web API Generation

Consider generating a REST API for a new project:

### **1. Context Detection**
```bash
# GGen automatically detects:
# - Language: Rust (from Cargo.toml)
# - Framework: Actix Web (from dependencies)
# - Database: PostgreSQL (from environment)
# - Testing: Cargo test (from project structure)
```

### **2. Template Discovery**
```bash
# GGen finds templates:
# - rust-actix-api.tmpl (matches language + framework)
# - postgres-integration.tmpl (matches database)
# - api-testing.tmpl (matches testing framework)
```

### **3. Default Application**
```bash
# GGen applies defaults:
# - Port: 8080 (standard for web APIs)
# - Database URL: postgres://localhost:5432/dbname
# - Logging: info level with structured output
# - CORS: enabled for development
```

### **4. Configuration Validation**
```bash
# GGen validates:
# - All required templates are available
# - Dependencies are compatible
# - Configuration is complete and valid
# - Generated code will compile and run
```

## Benefits of Self-Configuration

### **1. Reduced Cognitive Load**
Developers don't need to remember configuration details or make manual choices for common scenarios.

### **2. Consistency**
Automatic configuration ensures consistent settings across projects and teams.

### **3. Best Practices**
Intelligent defaults encode industry best practices and community knowledge.

### **4. Adaptability**
Configuration adapts automatically to changing context and requirements.

### **5. Error Prevention**
Automatic validation prevents common configuration errors and inconsistencies.

## Customization and Overrides

While GGen provides intelligent defaults, you can customize and override them:

### **Configuration Files**
```toml
# ggen.toml
[project]
name = "my-api"
language = "rust"
framework = "actix-web"

[defaults]
port = 3000
database_url = "postgres://localhost:5432/myapp"
log_level = "debug"
```

### **Environment Variables**
```bash
export GGEN_PORT=3000
export GGEN_DATABASE_URL="postgres://localhost:5432/myapp"
export GGEN_LOG_LEVEL="debug"
```

### **Command Line Overrides**
```bash
ggen project gen --port 3000 --database-url "postgres://localhost:5432/myapp"
```

## Best Practices

### **1. Trust the Defaults**
Start with GGen's intelligent defaults and only override when necessary.

### **2. Document Overrides**
Document any configuration overrides and the rationale for them.

### **3. Use Hierarchical Configuration**
Leverage configuration inheritance to manage settings at appropriate levels.

### **4. Validate Changes**
Always validate configuration changes before applying them to production.

### **5. Monitor Configuration**
Track configuration changes and their impact on system behavior.

## Next Steps

Start with [11.1: Template Discovery](./chapter-11-1.md) to understand how GGen automatically finds and selects templates.
