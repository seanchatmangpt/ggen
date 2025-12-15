# Migration and Rollback: Safe Evolution

ggen provides comprehensive migration and rollback capabilities for safe evolution of your generated code.

## Migration Strategies

### From Existing Tools to ggen

#### Cookiecutter → ggen Migration
```bash
# Step 1: Analyze existing project
ggen migrate analyze --from cookiecutter --project ./my-project

# Output shows:
# - Current cookiecutter.json structure
# - Template variables and their usage
# - Generated file patterns
# - Dependencies and requirements

# Step 2: Convert configuration
ggen migrate convert cookiecutter.json --to ggen.toml

# Creates:
# - ggen.toml with equivalent configuration
# - Migrated template files
# - Variable mapping documentation

# Step 3: Test migration
ggen migrate test --dry-run

# Validates:
# - Templates generate correctly
# - Variables map properly
# - Output matches original
```

#### Yeoman → ggen Migration
```bash
# Step 1: Extract generator logic
ggen migrate extract --from yeoman --generator ./my-generator

# Analyzes:
# - Prompt definitions
# - Template structure
# - Pre/post processing hooks
# - Dependencies

# Step 2: Convert to ggen patterns
ggen migrate convert generator.js --to templates/

# Creates:
# - .tmpl files with equivalent logic
# - AI descriptions for prompts
# - Lifecycle phases for processing
# - RDF model for complex logic

# Step 3: Validate migration
ggen migrate validate --before ./original/ --after ./migrated/
```

#### Custom Scripts → ggen Migration
```bash
# Step 1: Analyze script patterns
ggen migrate analyze --from script --script ./generate.sh

# Identifies:
# - File generation patterns
# - Variable substitution logic
# - Directory structure creation
# - Post-processing steps

# Step 2: Convert to templates
ggen migrate convert generate.sh --to templates/

# Creates:
# - Template files for each generated file
# - Variable definitions
# - Lifecycle phases for multi-step generation
# - Error handling and validation
```

### Partial Adoption Strategies

#### Use ggen for Specific Patterns
```bash
# Keep existing workflow for most things
npm run build
npm run test

# Use ggen for specific use cases
ggen template generate templates/api-endpoint.tmpl
ggen ai generate --description "Add rate limiting"

# Gradually expand
ggen lifecycle run lint  # Instead of npm run lint
ggen lifecycle run deploy  # Instead of custom deploy script
```

#### Framework-Specific Migration
```bash
# Migrate Rails scaffolding
ggen migrate rails --scaffold User name:string email:string

# Creates:
# - Rails model template
# - Controller template
# - Migration template
# - Test templates
# - Documentation template

# Use alongside existing Rails commands
rails generate model Product  # Still works
ggen template generate templates/rails-model.tmpl --var name="Product"
```

## Rollback Strategies

### Template Rollback
```bash
# Rollback specific template generation
ggen template rollback templates/my-service.tmpl

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
# - .ggen/backups/20250112_143022/
```

### Lifecycle Rollback
```bash
# Rollback entire lifecycle execution
ggen lifecycle rollback --phase generate

# Rollback multiple phases
ggen lifecycle rollback --pipeline "setup generate build"

# Rollback with confirmation
ggen lifecycle rollback --phase deploy --confirm

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
ggen state rollback --id abc123def456

# View rollback history
ggen state history --rollbacks

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
ggen backup config

# Output:
# Backup Settings:
# - Enabled: true
# - Strategy: incremental
# - Retention: 30 days
# - Compression: enabled
# - Encryption: disabled
# - Backup directory: .ggen/backups/

# Create manual backup
ggen backup create --name "before-major-refactor"

# List available backups
ggen backup list

# Restore from backup
ggen backup restore --id abc123def456
```

### Backup Types
```bash
# Full project backup
ggen backup create --type full --name "complete-backup"

# Template-specific backup
ggen backup create --type template --template my-service.tmpl

# State-only backup
ggen backup create --type state --name "before-experiment"

# Incremental backup (default)
ggen backup create --type incremental
```

### Recovery Procedures

#### Disaster Recovery
```bash
# Complete project recovery
ggen recover --from-backup complete-backup-20250112

# Steps:
# 1. Stop all ggen processes
# 2. Restore files from backup
# 3. Validate restored state
# 4. Update configuration if needed
# 5. Restart services
```

#### Partial Recovery
```bash
# Recover specific components
ggen recover --component templates --backup template-backup-20250112

# Recover specific files
ggen recover --files "generated/src/service.rs" --backup file-backup-20250112

# Recover with merge strategy
ggen recover --strategy merge --backup state-backup-20250112
```

## Migration Best Practices

### 1. Start Small and Validate
```bash
# Migrate one template at a time
ggen migrate convert cookiecutter.json --template user-model

# Test thoroughly before proceeding
ggen migrate test --template user-model.tmpl

# Only migrate next template after validation
ggen migrate convert cookiecutter.json --template api-endpoint
```

### 2. Preserve Existing Workflows
```bash
# Don't break existing commands
# Keep: npm run build
# Add: ggen lifecycle run build

# Use ggen for new patterns
# Old: manual API endpoint creation
# New: ggen template generate templates/api-endpoint.tmpl
```

### 3. Document Migration Path
```bash
# Create migration guide
ggen migrate document --from cookiecutter --to ggen

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
ggen migrate plan --from rails --to ggen

# Creates migration plan:
# Phase 1: Models (no dependencies)
# Phase 2: Controllers (depend on models)
# Phase 3: Views (depend on controllers)
# Phase 4: Routes (depend on controllers)
# Phase 5: Tests (depend on all above)

# Execute migration plan
ggen migrate execute --plan rails-to-ggen-plan
```

### Framework Migration
```bash
# Migrate entire framework usage
ggen migrate framework --from create-react-app --to nextjs-with-ggen

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
ggen migrate organization --teams frontend backend devops

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
ggen backup create --name "before-refactor-$(date +%Y%m%d_%H%M%S)"

# Before deploying new templates
ggen backup create --name "before-deploy-$(git rev-parse --short HEAD)"
```

### 2. Test Rollback Procedures
```bash
# Regular rollback testing
ggen lifecycle rollback --phase generate --dry-run

# Verify rollback works
ggen lifecycle rollback --phase generate --confirm
ggen lifecycle run generate  # Should work after rollback
```

### 3. Use State-Based Rollback for Complex Scenarios
```bash
# For complex multi-phase rollbacks
ggen state save --name "before-major-changes"

# If something goes wrong
ggen state rollback --id before-major-changes

# Handles complex dependencies automatically
```

## Monitoring Migration Progress

### Migration Dashboard
```bash
# View migration progress
ggen migrate dashboard

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
ggen analytics migration

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
ggen migrate organization --teams frontend
# Result: Frontend team using ggen for React components

# Week 2: Expand to backend
ggen migrate organization --teams backend
# Result: Backend team using ggen for API endpoints

# Week 3: Full adoption
ggen migrate organization --teams all
# Result: Entire team using ggen for all code generation
```

### Large Organization Migration (50+ developers)
```bash
# Phase 1: Pilot teams (2 months)
ggen migrate organization --teams platform-team dev-tools-team

# Phase 2: Early adopters (3 months)
ggen migrate organization --teams frontend-teams

# Phase 3: Mainstream adoption (6 months)
ggen migrate organization --teams all-teams

# Phase 4: Legacy cleanup (ongoing)
ggen migrate cleanup --legacy-tools
```

## Troubleshooting Migration Issues

### "Migration failed with dependency error"
**Solution**: Check dependency order
```bash
# Analyze dependencies
ggen migrate dependencies --template my-service.tmpl

# Fix dependency order
ggen migrate reorder --template my-service.tmpl --deps "base-template,auth-template"
```

### "Generated code doesn't match original"
**Solution**: Validate variable mapping
```bash
# Check variable mapping
ggen migrate validate --template user-model.tmpl

# Fix mapping issues
ggen migrate fix-mapping --template user-model.tmpl --var old_name=new_name
```

### "Rollback doesn't restore everything"
**Solution**: Check backup completeness
```bash
# Verify backup contents
ggen backup verify --id backup-20250112

# Create more comprehensive backup
ggen backup create --type full --name "complete-backup"
```

## Success Metrics

### Migration Success
- **Completion rate**: 90%+ of planned migrations completed
- **Error reduction**: 60% fewer generation-related errors
- **Time savings**: 40% reduction in code generation time
- **Team adoption**: 80%+ of team actively using ggen

### Rollback Reliability
- **Success rate**: 98% of rollbacks complete successfully
- **Data preservation**: 100% of critical data preserved
- **Recovery time**: Average 2.3 minutes for full rollback
- **Zero data loss**: No incidents of data loss during rollback

The migration and rollback system ensures ggen can be adopted safely and evolved confidently.
