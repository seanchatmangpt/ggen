# OpenAPI Workflow Integration with Bree Semantic Scheduler

Run the complete **ZERO_TO_OPENAPI** workflow through the Bree semantic job scheduler.

## Architecture

Instead of running OpenAPI generation steps manually, define them as RDF jobs and execute through:

1. **RDF Specification** → Job definitions in Turtle
2. **Validation** → SHACL shapes ensure completeness
3. **Generation** → ggen creates executable jobs
4. **Execution** → Citty CLI or Bree scheduler runs jobs
5. **Monitoring** → SLA tracking, audit logs, metrics

## Workflow Jobs

### Job 1: Validate OpenAPI Specification

```turtle
jobs:validateOpenAPI
  a bree:Job ;
  rdfs:label "Validate OpenAPI Specification" ;
  bree:jobName "validate-openapi" ;
  bree:jobPath "jobs/validate-openapi.js" ;
  bree:runOnStart false ;
  bree:hasTimeout jobs:timeout_10s ;
  bree:hasCittyCommand jobs:cmd_validate ;
  rdfs:comment "Validates OpenAPI.yaml against JSON Schema" .

jobs:timeout_10s
  a bree:MSInterval ;
  bree:milliseconds 10000 .

jobs:cmd_validate
  a citty:CliCommand ;
  citty:commandName "validate-openapi" ;
  citty:commandDescription "Validate OpenAPI specification" ;
  citty:hasArgument jobs:arg_openapi_file .

jobs:arg_openapi_file
  a citty:Argument ;
  citty:argumentName "file" ;
  citty:argumentType "string" ;
  rdfs:comment "Path to OpenAPI.yaml file" .
```

### Job 2: Generate RDF Schema from OpenAPI

```turtle
jobs:generateRDFSchema
  a bree:Job ;
  rdfs:label "Generate RDF Schema" ;
  bree:jobName "generate-rdf-schema" ;
  bree:jobPath "jobs/generate-rdf-schema.js" ;
  bree:runOnStart false ;
  bree:hasTimeout jobs:timeout_30s ;
  bree:hasCittyCommand jobs:cmd_generate_rdf ;
  rdfs:comment "Transforms OpenAPI spec into RDF ontology" .

jobs:timeout_30s
  a bree:MSInterval ;
  bree:milliseconds 30000 .

jobs:cmd_generate_rdf
  a citty:CliCommand ;
  citty:commandName "generate-rdf-schema" ;
  citty:commandDescription "Generate RDF schema from OpenAPI" ;
  citty:hasArgument jobs:arg_openapi_file ;
  citty:hasArgument jobs:arg_output_ttl .

jobs:arg_output_ttl
  a citty:Argument ;
  citty:argumentName "output" ;
  citty:argumentType "string" ;
  rdfs:comment "Output RDF file (.ttl)" .
```

### Job 3: Generate JavaScript Code via ggen

```turtle
jobs:generateCode
  a bree:Job ;
  rdfs:label "Generate Code via ggen" ;
  bree:jobName "generate-code" ;
  bree:jobPath "jobs/generate-code.js" ;
  bree:runOnStart false ;
  bree:hasTimeout jobs:timeout_60s ;
  bree:hasCittyCommand jobs:cmd_gen ;
  rdfs:comment "Runs ggen sync to produce JavaScript from RDF spec" .

jobs:timeout_60s
  a bree:MSInterval ;
  bree:milliseconds 60000 .

jobs:cmd_gen
  a citty:CliCommand ;
  citty:commandName "generate-code" ;
  citty:commandDescription "Generate JavaScript code" ;
  citty:hasArgument jobs:arg_config_file ;
  citty:hasArgument jobs:arg_output_dir .

jobs:arg_config_file
  a citty:Argument ;
  citty:argumentName "config" ;
  citty:argumentType "string" ;
  rdfs:comment "ggen config file path" .

jobs:arg_output_dir
  a citty:Argument ;
  citty:argumentName "output" ;
  citty:argumentType "string" ;
  rdfs:comment "Output directory for generated code" .
```

### Job 4: Type Check with TypeScript

```turtle
jobs:typeCheck
  a bree:Job ;
  rdfs:label "Type Check Generated Code" ;
  bree:jobName "type-check" ;
  bree:jobPath "jobs/type-check.js" ;
  bree:runOnStart false ;
  bree:hasTimeout jobs:timeout_45s ;
  bree:hasCittyCommand jobs:cmd_typecheck ;
  rdfs:comment "Validates generated code with TypeScript compiler" .

jobs:timeout_45s
  a bree:MSInterval ;
  bree:milliseconds 45000 .

jobs:cmd_typecheck
  a citty:CliCommand ;
  citty:commandName "type-check" ;
  citty:commandDescription "Run TypeScript type checking" ;
  citty:hasArgument jobs:arg_tsconfig .

jobs:arg_tsconfig
  a citty:Argument ;
  citty:argumentName "tsconfig" ;
  citty:argumentType "string" ;
  rdfs:comment "Path to tsconfig.json" .
```

### Job 5: Run Tests

```turtle
jobs:runTests
  a bree:Job ;
  rdfs:label "Run Generated Code Tests" ;
  bree:jobName "run-tests" ;
  bree:jobPath "jobs/run-tests.js" ;
  bree:runOnStart false ;
  bree:hasTimeout jobs:timeout_120s ;
  bree:hasCittyCommand jobs:cmd_tests ;
  rdfs:comment "Executes test suite on generated code" .

jobs:timeout_120s
  a bree:MSInterval ;
  bree:milliseconds 120000 .

jobs:cmd_tests
  a citty:CliCommand ;
  citty:commandName "run-tests" ;
  citty:commandDescription "Run test suite" ;
  citty:hasArgument jobs:arg_test_pattern .

jobs:arg_test_pattern
  a citty:Argument ;
  citty:argumentName "pattern" ;
  citty:argumentType "string" ;
  rdfs:comment "Jest test pattern (optional)" .
```

### Job 6: Lint & Format

```turtle
jobs:lintAndFormat
  a bree:Job ;
  rdfs:label "Lint and Format Code" ;
  bree:jobName "lint-and-format" ;
  bree:jobPath "jobs/lint-and-format.js" ;
  bree:runOnStart false ;
  bree:hasTimeout jobs:timeout_30s ;
  bree:hasCittyCommand jobs:cmd_lint ;
  rdfs:comment "Runs ESLint and Prettier on generated code" .

jobs:cmd_lint
  a citty:CliCommand ;
  citty:commandName "lint-and-format" ;
  citty:commandDescription "Lint and format code" ;
  citty:hasArgument jobs:arg_fix .

jobs:arg_fix
  a citty:Argument ;
  citty:argumentName "fix" ;
  citty:argumentType "boolean" ;
  rdfs:comment "Auto-fix formatting issues" .
```

### Job 7: Build Distribution Package

```turtle
jobs:buildPackage
  a bree:Job ;
  rdfs:label "Build Distribution Package" ;
  bree:jobName "build-package" ;
  bree:jobPath "jobs/build-package.js" ;
  bree:runOnStart false ;
  bree:hasTimeout jobs:timeout_60s ;
  bree:hasCittyCommand jobs:cmd_build ;
  rdfs:comment "Bundles and minifies code for distribution" .

jobs:cmd_build
  a citty:CliCommand ;
  citty:commandName "build-package" ;
  citty:commandDescription "Build distribution package" ;
  citty:hasArgument jobs:arg_minify ;
  citty:hasArgument jobs:arg_sourcemaps .

jobs:arg_minify
  a citty:Argument ;
  citty:argumentName "minify" ;
  citty:argumentType "boolean" ;
  rdfs:comment "Minify output" .

jobs:arg_sourcemaps
  a citty:Argument ;
  citty:argumentName "sourcemaps" ;
  citty:argumentType "boolean" ;
  rdfs:comment "Generate source maps" .
```

### Job 8: Deploy Generated Code

```turtle
jobs:deployCode
  a bree:Job ;
  rdfs:label "Deploy Generated Code" ;
  bree:jobName "deploy" ;
  bree:jobPath "jobs/deploy.js" ;
  bree:runOnStart false ;
  bree:hasTimeout jobs:timeout_120s ;
  bree:hasCittyCommand jobs:cmd_deploy ;
  rdfs:comment "Deploys generated code to production" .

jobs:cmd_deploy
  a citty:CliCommand ;
  citty:commandName "deploy" ;
  citty:commandDescription "Deploy generated code" ;
  citty:hasArgument jobs:arg_environment ;
  citty:hasArgument jobs:arg_dry_run .

jobs:arg_environment
  a citty:Argument ;
  citty:argumentName "env" ;
  citty:argumentType "string" ;
  rdfs:comment "Deployment environment (staging|production)" .

jobs:arg_dry_run
  a citty:Argument ;
  citty:argumentName "dry-run" ;
  citty:argumentType "boolean" ;
  rdfs:comment "Preview changes without deploying" .
```

## Workflow Pipeline

Define a pipeline that runs jobs sequentially:

```turtle
jobs:openapi_pipeline
  a bree:Job ;
  rdfs:label "OpenAPI Complete Pipeline" ;
  bree:jobName "openapi-pipeline" ;
  bree:jobPath "jobs/openapi-pipeline.js" ;
  bree:runOnStart false ;
  bree:hasInterval jobs:on_demand ;
  bree:hasCittyCommand jobs:cmd_pipeline ;
  rdfs:comment "Runs full OpenAPI workflow: validate → generate → test → deploy" ;

  # Pipeline dependencies (conceptual - would be handled in job logic)
  rdfs:comment "Dependencies: 1→2→3→4→5→6→7→8" .

jobs:on_demand
  a bree:HumanInterval ;
  bree:humanExpression "on-demand" ;
  bree:milliseconds 0 .

jobs:cmd_pipeline
  a citty:CliCommand ;
  citty:commandName "openapi-pipeline" ;
  citty:commandDescription "Run complete OpenAPI generation pipeline" ;
  citty:hasArgument jobs:arg_spec_file ;
  citty:hasArgument jobs:arg_step .

jobs:arg_spec_file
  a citty:Argument ;
  citty:argumentName "spec" ;
  citty:argumentType "string" ;
  rdfs:comment "OpenAPI specification file" .

jobs:arg_step
  a citty:Argument ;
  citty:argumentName "step" ;
  citty:argumentType "string" ;
  rdfs:comment "Run specific step: validate|generate-rdf|generate-code|typecheck|test|lint|build|deploy|all" .
```

## Usage

### Run Individual Steps

```bash
# Validate OpenAPI spec
bree-scheduler run validate-openapi --file openapi.yaml

# Generate RDF schema
bree-scheduler run generate-rdf-schema --file openapi.yaml --output schema.ttl

# Generate code
bree-scheduler run generate-code --config ggen.toml --output generated/

# Type check
bree-scheduler run type-check --tsconfig tsconfig.json

# Run tests
bree-scheduler run run-tests

# Lint and format
bree-scheduler run lint-and-format --fix

# Build package
bree-scheduler run build-package --minify --sourcemaps

# Deploy
bree-scheduler run deploy --env production
```

### Run Complete Pipeline

```bash
# Run all steps sequentially
bree-scheduler run openapi-pipeline --spec openapi.yaml --step all

# Run pipeline starting from specific step
bree-scheduler run openapi-pipeline --spec openapi.yaml --step generate-code
```

### Schedule Pipeline

```turtle
jobs:scheduled_pipeline
  a bree:Job ;
  rdfs:label "Scheduled OpenAPI Pipeline" ;
  bree:jobName "scheduled-openapi" ;
  bree:jobPath "jobs/openapi-pipeline.js" ;

  # Run every night at 2:00 AM
  bree:hasCron jobs:nightly_cron .

jobs:nightly_cron
  a bree:CronExpression ;
  bree:cronExpression "0 2 * * *" ;
  rdfs:comment "Daily at 2:00 AM" .
```

## Monitoring

### Track Pipeline Execution

```bash
# Show execution history
bree-scheduler history --job-name "openapi-pipeline" --limit 20

# Get SLA metrics
bree-scheduler metrics --job-name "generate-code"

# Check circuit breaker status
bree-scheduler metrics --format json | jq '.circuitBreakers'
```

### Audit Trail

All job execution is tracked:

```bash
# View audit logs
tail -f logs/audit.jsonl

# Query specific pipeline run
jq 'select(.jobName == "openapi-pipeline")' logs/audit.jsonl
```

## Implementation Example

Job file: `jobs/openapi-pipeline.js`

```javascript
import { parentPort } from 'worker_threads';
import { execSync } from 'child_process';

const steps = {
  'validate': () => execSync('npm run validate'),
  'generate-rdf': () => execSync('npm run generate-rdf'),
  'generate-code': () => execSync('npm run generate'),
  'typecheck': () => execSync('npm run typecheck'),
  'test': () => execSync('npm test'),
  'lint': () => execSync('npm run lint -- --fix'),
  'build': () => execSync('npm run build'),
  'deploy': () => execSync('npm run deploy'),
};

async function run(args) {
  const step = args.step || 'all';
  const spec = args.spec || 'openapi.yaml';

  try {
    if (step === 'all') {
      // Run all steps sequentially
      for (const [name, fn] of Object.entries(steps)) {
        console.log(`Running: ${name}`);
        fn();
        console.log(`✓ ${name} completed`);
      }
    } else if (steps[step]) {
      console.log(`Running: ${step}`);
      steps[step]();
      console.log(`✓ ${step} completed`);
    } else {
      throw new Error(`Unknown step: ${step}`);
    }

    parentPort.postMessage({ type: 'complete', result: 'success' });
    process.exit(0);
  } catch (error) {
    console.error(`✗ ${error.message}`);
    process.exit(1);
  }
}

run(process.env.WORKER_DATA);
```

## Benefits

✅ **Specification-Driven** - Entire workflow defined in RDF
✅ **Auditable** - Every step logged with tracing IDs
✅ **Monitorable** - SLA tracking, metrics, health checks
✅ **Resilient** - Circuit breakers, graceful degradation
✅ **Schedulable** - Run on-demand, cron, or triggered
✅ **Compliant** - Audit trail for SOC2/HIPAA/GDPR

## Next Steps

1. Add job definitions to `bree-jobs-openapi.ttl`
2. Implement job files in `jobs/`
3. Configure in `ggen-bree-config.toml`
4. Generate code with `ggen sync`
5. Deploy and monitor through Citty CLI
