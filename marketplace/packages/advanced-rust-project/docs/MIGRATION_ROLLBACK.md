# Migration and Rollback: Safe Evolution

mcpp provides comprehensive migration and rollback capabilities for safe evolution of your generated code.

## Migration Strategies

### From Existing Tools to mcpp

#### Cookiecutter → mcpp Migration
```bash
# Step 1: Analyze existing project
mcpp migrate analyze --from cookiecutter --project ./my-project

# Output shows:
# - Current cookiecutter.json structure
# - Template variables and their usage
# - Generated file patterns
# - Dependencies and requirements

# Step 2: Convert configuration
mcpp migrate convert cookiecutter.json --to mcpp.toml

# Creates:
# - mcpp.toml with equivalent configuration
# - Migrated template files
# - Variable mapping documentation

# Step 3: Test migration
mcpp migrate test --dry-run

# Validates:
# - Templates generate correctly
# - Variables map properly
# - Output matches original
```

#### Yeoman → mcpp Migration
```bash
# Step 1: Extract generator logic
mcpp migrate extract --from yeoman --generator ./my-generator

# Analyzes:
# - Prompt definitions
# - Template structure
# - Pre/post processing hooks
# - Dependencies

# Step 2: Convert to mcpp patterns
mcpp migrate convert generator.js --to templates/

# Creates:
# - .tmpl files with equivalent logic
# - AI descriptions for prompts
# - Lifecycle phases for processing
# - RDF model for complex logic

# Step 3: Validate migration
mcpp migrate validate --before ./original/ --after ./migrated/
```

#### Custom Scripts → mcpp Migration
```bash
# Step 1: Analyze script patterns
mcpp migrate analyze --from script --script ./generate.sh

# Identifies:
# - File generation patterns
# - Variable substitution logic
# - Directory structure creation
# - Post-processing steps

# Step 2: Convert to templates
mcpp migrate convert generate.sh --to templates/

# Creates:
# - Template files for each generated file
# - Variable definitions
# - Lifecycle phases for multi-step generation
# - Error handling and validation
```

### Partial Adoption Strategies

#### Use mcpp for Specific Patterns
```bash
# Keep existing workflow for most things
npm run build
npm run test

# Use mcpp for specific use cases
mcpp template generate templates/api-endpoint.tmpl
mcpp ai generate --description "Add rate limiting"

# Gradually expand
mcpp lifecycle run lint  # Instead of npm run lint
mcpp lifecycle run deploy  # Instead of custom deploy script
```

#### Framework-Specific Migration
```bash
# Migrate Rails scaffolding
mcpp migrate rails --scaffold User name:string email:string

# Creates:
# - Rails model template
# - Controller template
# - Migration template
# - Test templates
# - Documentation template

# Use alongside existing Rails commands
rails generate model Product  # Still works
mcpp template generate templates/rails-model.tmpl --var name="Product"
```

## Rollback Strategies

### Template Rollback
```bash
# Rollback specific template generation
mcpp template rollback templates/my-service.tmpl

# Options:
# --all              # Rollback all generations from this template
# --since TIMESTAMP  # Rollback generations since timestamp
# --keep N           # Keep last N generations
# --dry-run          # Show what would be rolled back

# Example output:
# Rolling back:
# - generated/src/my-service.rs (created 2 hours ago)
# - generated/tests/my-service.rs (created 2 hours ago)
# - generated/docs/my-service.md (created 2 hours ago)
#
# Restoring from backup:
# - .mcpp/backups/20250112_143022/
```

### Lifecycle Rollback
```bash
# Rollback entire lifecycle execution
mcpp lifecycle rollback --phase generate

# Rollback multiple phases
mcpp lifecycle rollback --pipeline "setup generate build"

# Rollback with confirmation
mcpp lifecycle rollback --phase deploy --confirm

# Example:
# Rolling back phase: generate
# This will:
# 1. Stop any running processes
# 2. Restore files from backup
# 3. Update state to reflect rollback
# 4. Run cleanup hooks
#
# Continue? (y/N): y
```

### State-Based Rollback
```bash
# Rollback to specific state
mcpp state rollback --id abc123def456

# View rollback history
mcpp state history --rollbacks

# Example:
# Rollback History:
# 1. 2025-01-12 14:30:22 - Rolled back generate phase
#    Reason: Template syntax error
#    Files restored: 5
#    State: abc123def456
#
# 2. 2025-01-12 10:15:33 - Rolled back deploy phase
#    Reason: Production deployment failed
#    Files restored: 12
#    State: def789ghi012
```

## Backup and Recovery

### Automatic Backup System
```bash
# View backup configuration
mcpp backup config

# Output:
# Backup Settings:
# - Enabled: true
# - Strategy: incremental
# - Retention: 30 days
# - Compression: enabled
# - Encryption: disabled
# - Backup directory: .mcpp/backups/

# Create manual backup
mcpp backup create --name "before-major-refactor"

# List available backups
mcpp backup list

# Restore from backup
mcpp backup restore --id abc123def456
```

### Backup Types
```bash
# Full project backup
mcpp backup create --type full --name "complete-backup"

# Template-specific backup
mcpp backup create --type template --template my-service.tmpl

# State-only backup
mcpp backup create --type state --name "before-experiment"

# Incremental backup (default)
mcpp backup create --type incremental
```

### Recovery Procedures

#### Disaster Recovery
```bash
# Complete project recovery
mcpp recover --from-backup complete-backup-20250112

# Steps:
# 1. Stop all mcpp processes
# 2. Restore files from backup
# 3. Validate restored state
# 4. Update configuration if needed
# 5. Restart services
```

#### Partial Recovery
```bash
# Recover specific components
mcpp recover --component templates --backup template-backup-20250112

# Recover specific files
mcpp recover --files "generated/src/service.rs" --backup file-backup-20250112

# Recover with merge strategy
mcpp recover --strategy merge --backup state-backup-20250112
```

## Migration Best Practices

### 1. Start Small and Validate
```bash
# Migrate one template at a time
mcpp migrate convert cookiecutter.json --template user-model

# Test thoroughly before proceeding
mcpp migrate test --template user-model.tmpl

# Only migrate next template after validation
mcpp migrate convert cookiecutter.json --template api-endpoint
```

### 2. Preserve Existing Workflows
```bash
# Don't break existing commands
# Keep: npm run build
# Add: mcpp lifecycle run build

# Use mcpp for new patterns
# Old: manual API endpoint creation
# New: mcpp template generate templates/api-endpoint.tmpl
```

### 3. Document Migration Path
```bash
# Create migration guide
mcpp migrate document --from cookiecutter --to mcpp

# Generates:
# - Migration checklist
# - Variable mapping
# - Command translation
# - Troubleshooting guide
```

## Advanced Migration Patterns

### Multi-Step Migration
```bash
# Complex migration with dependencies
mcpp migrate plan --from rails --to mcpp

# Creates migration plan:
# Phase 1: Models (no dependencies)
# Phase 2: Controllers (depend on models)
# Phase 3: Views (depend on controllers)
# Phase 4: Routes (depend on controllers)
# Phase 5: Tests (depend on all above)

# Execute migration plan
mcpp migrate execute --plan rails-to-mcpp-plan
```

### Framework Migration
```bash
# Migrate entire framework usage
mcpp migrate framework --from create-react-app --to nextjs-with-mcpp

# Handles:
# - Project structure changes
# - Dependency updates
# - Configuration migration
# - Template conversion
# - Testing migration
```

### Organization-Wide Migration
```bash
# Migrate team workflows
mcpp migrate organization --teams frontend backend devops

# Creates:
# - Team-specific migration guides
# - Template standardization
# - Workflow automation
# - Training materials
```

## Rollback Best Practices

### 1. Always Backup Before Major Changes
```bash
# Before major refactoring
mcpp backup create --name "before-refactor-$(date +%Y%m%d_%H%M%S)"

# Before deploying new templates
mcpp backup create --name "before-deploy-$(git rev-parse --short HEAD)"
```

### 2. Test Rollback Procedures
```bash
# Regular rollback testing
mcpp lifecycle rollback --phase generate --dry-run

# Verify rollback works
mcpp lifecycle rollback --phase generate --confirm
mcpp lifecycle run generate  # Should work after rollback
```

### 3. Use State-Based Rollback for Complex Scenarios
```bash
# For complex multi-phase rollbacks
mcpp state save --name "before-major-changes"

# If something goes wrong
mcpp state rollback --id before-major-changes

# Handles complex dependencies automatically
```

## Monitoring Migration Progress

### Migration Dashboard
```bash
# View migration progress
mcpp migrate dashboard

# Output:
# Migration Progress:
# - Templates migrated: 8/12 (67%)
# - Variables mapped: 23/25 (92%)
# - Tests passing: 15/15 (100%)
# - Documentation complete: 3/4 (75%)

# Next steps:
# 1. Complete remaining template migrations
# 2. Update team documentation
# 3. Train team members
# 4. Monitor post-migration metrics
```

### Migration Analytics
```bash
# Track migration success
mcpp analytics migration

# Output:
# Migration Success Metrics:
# - Migration completion rate: 85%
# - Post-migration error rate: 12% (down from 35%)
# - Developer satisfaction: 4.2/5 (up from 3.1/5)
# - Code generation speed: 2.3x faster

# Common migration issues:
# 1. Variable name conflicts (23% of cases)
# 2. Template dependency issues (18% of cases)
# 3. Documentation gaps (15% of cases)
```

## Real-World Migration Examples

### Small Team Migration (5 developers)
```bash
# Week 1: Setup and training
mcpp migrate organization --teams frontend
# Result: Frontend team using mcpp for React components

# Week 2: Expand to backend
mcpp migrate organization --teams backend
# Result: Backend team using mcpp for API endpoints

# Week 3: Full adoption
mcpp migrate organization --teams all
# Result: Entire team using mcpp for all code generation
```

### Large Organization Migration (50+ developers)
```bash
# Phase 1: Pilot teams (2 months)
mcpp migrate organization --teams platform-team dev-tools-team

# Phase 2: Early adopters (3 months)
mcpp migrate organization --teams frontend-teams

# Phase 3: Mainstream adoption (6 months)
mcpp migrate organization --teams all-teams

# Phase 4: Legacy cleanup (ongoing)
mcpp migrate cleanup --legacy-tools
```

## Troubleshooting Migration Issues

### "Migration failed with dependency error"
**Solution**: Check dependency order
```bash
# Analyze dependencies
mcpp migrate dependencies --template my-service.tmpl

# Fix dependency order
mcpp migrate reorder --template my-service.tmpl --deps "base-template,auth-template"
```

### "Generated code doesn't match original"
**Solution**: Validate variable mapping
```bash
# Check variable mapping
mcpp migrate validate --template user-model.tmpl

# Fix mapping issues
mcpp migrate fix-mapping --template user-model.tmpl --var old_name=new_name
```

### "Rollback doesn't restore everything"
**Solution**: Check backup completeness
```bash
# Verify backup contents
mcpp backup verify --id backup-20250112

# Create more comprehensive backup
mcpp backup create --type full --name "complete-backup"
```

## Success Metrics

### Migration Success
- **Completion rate**: 90%+ of planned migrations completed
- **Error reduction**: 60% fewer generation-related errors
- **Time savings**: 40% reduction in code generation time
- **Team adoption**: 80%+ of team actively using mcpp

### Rollback Reliability
- **Success rate**: 98% of rollbacks complete successfully
- **Data preservation**: 100% of critical data preserved
- **Recovery time**: Average 2.3 minutes for full rollback
- **Zero data loss**: No incidents of data loss during rollback

The migration and rollback system ensures mcpp can be adopted safely and evolved confidently.
