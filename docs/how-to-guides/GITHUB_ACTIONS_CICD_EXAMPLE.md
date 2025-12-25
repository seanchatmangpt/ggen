# GitHub Actions CI/CD Integration Example

> Automate code generation, testing, and deployment with GitHub Actions

## Overview

This guide shows how to integrate ggen into a GitHub Actions CI/CD pipeline to:

- Automatically regenerate code when ontologies change
- Run tests on generated code
- Deploy to multiple environments
- Validate generated schemas
- Create release artifacts

## Prerequisites

- GitHub repository
- ggen CLI installed and versioned
- Node.js/Python/Go tooling as needed
- Deployment credentials (Docker, AWS, etc.)

---

## Workflow 1: Auto-Regenerate and Test

Create `.github/workflows/generate-and-test.yml`:

```yaml
name: Generate Code & Run Tests

on:
  push:
    branches: [main, develop]
    paths:
      - "ontology/**"
      - "ggen.toml"
      - "templates/**"
      - ".github/workflows/generate-and-test.yml"

concurrency:
  group: ${{ github.workflow }}-${{ github.ref }}
  cancel-in-progress: true

jobs:
  generate:
    name: Generate Code from Ontology
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v4
        with:
          fetch-depth: 0  # Full history for commits

      - name: Install ggen CLI
        run: |
          curl -sSL https://ggen.example.com/install.sh | bash
          ggen-cli --version

      - name: Generate code
        run: |
          ggen-cli render
          ls -la generated/

      - name: Check for changes
        id: changes
        run: |
          if git diff --quiet generated/; then
            echo "changed=false" >> $GITHUB_OUTPUT
          else
            echo "changed=true" >> $GITHUB_OUTPUT
            git diff --name-only generated/
          fi

      - name: Create Pull Request (if changes)
        if: steps.changes.outputs.changed == 'true'
        uses: peter-evans/create-pull-request@v5
        with:
          commit-message: "chore(codegen): Regenerate code from ontology changes"
          title: "Codegen: Regenerated from ontology"
          body: |
            ## Summary
            Automatic code regeneration from RDF ontology changes.

            - **Ontology**: `${{ github.event.head_commit.message }}`
            - **Generated Files**: See diff below

            ### Generated Files
            ```
            ${{ steps.changes.outputs.files }}
            ```

            ### Next Steps
            1. Review the generated changes
            2. Run tests locally: `npm test` or `pytest`
            3. Merge if tests pass in CI

          branch: auto/codegen-${{ github.run_number }}
          delete-branch: true
          labels: "codegen,automated"

  test:
    name: Run Tests
    needs: generate
    runs-on: ubuntu-latest

    strategy:
      matrix:
        node-version: [18.x, 20.x]
        python-version: ["3.10", "3.11"]

    steps:
      - uses: actions/checkout@v4

      - name: Setup Node.js ${{ matrix.node-version }}
        uses: actions/setup-node@v4
        with:
          node-version: ${{ matrix.node-version }}
          cache: "npm"

      - name: Setup Python ${{ matrix.python-version }}
        uses: actions/setup-python@v4
        with:
          python-version: ${{ matrix.python-version }}
          cache: "pip"

      - name: Install dependencies
        run: |
          npm ci
          pip install -r requirements.txt

      - name: Run linter
        run: |
          npm run lint
          python -m pylint generated/

      - name: Run type checking
        run: |
          npm run typecheck
          python -m mypy generated/

      - name: Run tests
        run: |
          npm test -- --coverage
          pytest --cov=generated/ --cov-report=xml

      - name: Upload coverage
        uses: codecov/codecov-action@v3
        with:
          files: ./coverage/lcov.info, ./coverage.xml
          fail_ci_if_error: true

      - name: Archive test results
        if: failure()
        uses: actions/upload-artifact@v3
        with:
          name: test-results-${{ matrix.node-version }}-${{ matrix.python-version }}
          path: |
            coverage/
            .nyc_output/
            htmlcov/

  validate:
    name: Validate Generated Code
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v4

      - name: Install ggen CLI
        run: |
          curl -sSL https://ggen.example.com/install.sh | bash

      - name: Validate GraphQL Schema
        if: hashFiles('generated/schema.graphql') != ''
        run: |
          npm install -g @apollo/rover
          rover graph introspect generated/schema.graphql

      - name: Validate SPARQL Queries
        run: |
          ggen-cli validate --schema ontology/ generated/

      - name: Check schema compliance
        run: |
          ggen-cli validate --shacl shapes.ttl generated/

      - name: Build artifacts
        run: |
          mkdir -p dist
          npm run build
          cp -r generated/ dist/

      - name: Upload artifacts
        uses: actions/upload-artifact@v3
        with:
          name: generated-code
          path: dist/
          retention-days: 30
```

---

## Workflow 2: Deploy Generated API

Create `.github/workflows/deploy-generated-api.yml`:

```yaml
name: Deploy Generated API

on:
  push:
    branches: [main]
    paths:
      - "generated/**"
      - "src/**"
      - "Dockerfile"
      - ".github/workflows/deploy-*.yml"
  workflow_dispatch:

env:
  REGISTRY: ghcr.io
  IMAGE_NAME: ${{ github.repository }}

jobs:
  build-and-push:
    runs-on: ubuntu-latest
    permissions:
      contents: read
      packages: write

    steps:
      - uses: actions/checkout@v4

      - name: Set up Docker Buildx
        uses: docker/setup-buildx-action@v2

      - name: Log in to Container Registry
        uses: docker/login-action@v2
        with:
          registry: ${{ env.REGISTRY }}
          username: ${{ github.actor }}
          password: ${{ secrets.GITHUB_TOKEN }}

      - name: Extract metadata
        id: meta
        uses: docker/metadata-action@v4
        with:
          images: ${{ env.REGISTRY }}/${{ env.IMAGE_NAME }}
          tags: |
            type=ref,event=branch
            type=sha,prefix={{branch}}-
            type=semver,pattern={{version}}

      - name: Build and push Docker image
        uses: docker/build-push-action@v4
        with:
          context: .
          push: true
          tags: ${{ steps.meta.outputs.tags }}
          labels: ${{ steps.meta.outputs.labels }}
          cache-from: type=registry,ref=${{ env.REGISTRY }}/${{ env.IMAGE_NAME }}:buildcache
          cache-to: type=registry,ref=${{ env.REGISTRY }}/${{ env.IMAGE_NAME }}:buildcache,mode=max

  deploy-staging:
    needs: build-and-push
    runs-on: ubuntu-latest
    if: github.event_name == 'push' && github.ref == 'refs/heads/develop'

    steps:
      - uses: actions/checkout@v4

      - name: Deploy to Staging
        env:
          DEPLOY_KEY: ${{ secrets.STAGING_DEPLOY_KEY }}
        run: |
          mkdir -p ~/.ssh
          echo "$DEPLOY_KEY" > ~/.ssh/deploy_key
          chmod 600 ~/.ssh/deploy_key
          ssh-keyscan -H ${{ secrets.STAGING_HOST }} >> ~/.ssh/known_hosts

          ssh -i ~/.ssh/deploy_key ${{ secrets.STAGING_USER }}@${{ secrets.STAGING_HOST }} \
            "cd /app && docker-compose pull && docker-compose up -d"

      - name: Run smoke tests
        run: |
          npx jest tests/smoke/ --runInBand
          pytest tests/smoke/

      - name: Notify Slack
        if: always()
        uses: slackapi/slack-github-action@v1.24.0
        with:
          webhook-url: ${{ secrets.SLACK_WEBHOOK }}
          payload: |
            {
              "text": "Staging deployment ${{ job.status }}",
              "blocks": [
                {
                  "type": "section",
                  "text": {
                    "type": "mrkdwn",
                    "text": "*Generated API Staging Deployment*\nStatus: ${{ job.status }}\nBranch: ${{ github.ref_name }}\nCommit: ${{ github.sha }}"
                  }
                }
              ]
            }

  deploy-production:
    needs: build-and-push
    runs-on: ubuntu-latest
    if: github.event_name == 'push' && github.ref == 'refs/heads/main'

    environment:
      name: production
      url: https://api.example.com

    steps:
      - uses: actions/checkout@v4

      - name: Create deployment
        id: deployment
        uses: actions/github-script@v6
        with:
          script: |
            const deployment = await github.rest.repos.createDeployment({
              owner: context.repo.owner,
              repo: context.repo.repo,
              ref: context.ref,
              environment: 'production',
              required_contexts: [],
              auto_merge: false
            });
            core.setOutput('deployment_id', deployment.data.id);

      - name: Deploy to Production
        env:
          DEPLOY_KEY: ${{ secrets.PROD_DEPLOY_KEY }}
        run: |
          mkdir -p ~/.ssh
          echo "$DEPLOY_KEY" > ~/.ssh/deploy_key
          chmod 600 ~/.ssh/deploy_key
          ssh-keyscan -H ${{ secrets.PROD_HOST }} >> ~/.ssh/known_hosts

          ssh -i ~/.ssh/deploy_key ${{ secrets.PROD_USER }}@${{ secrets.PROD_HOST }} \
            "cd /app && docker-compose pull && docker-compose up -d"

      - name: Verify deployment
        run: |
          for i in {1..30}; do
            if curl -f https://api.example.com/health; then
              echo "✓ API healthy"
              exit 0
            fi
            echo "Waiting for API to be ready... ($i/30)"
            sleep 2
          done
          exit 1

      - name: Update deployment status
        if: success()
        uses: actions/github-script@v6
        with:
          script: |
            await github.rest.repos.createDeploymentStatus({
              owner: context.repo.owner,
              repo: context.repo.repo,
              deployment_id: ${{ steps.deployment.outputs.deployment_id }},
              state: 'success',
              description: 'Successfully deployed to production',
              environment_url: 'https://api.example.com'
            });

      - name: Create release
        uses: actions/create-release@v1
        env:
          GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
        with:
          tag_name: v${{ github.run_number }}
          release_name: Release ${{ github.run_number }}
          body: |
            ## Generated API Release
            - Generated from ontology commit: ${{ github.sha }}
            - Build: ${{ github.run_number }}
            - API: https://api.example.com

      - name: Slack notification
        uses: slackapi/slack-github-action@v1.24.0
        with:
          webhook-url: ${{ secrets.SLACK_WEBHOOK }}
          payload: |
            {
              "text": "✅ Production deployment successful!",
              "blocks": [
                {
                  "type": "section",
                  "text": {
                    "type": "mrkdwn",
                    "text": "*Generated API Production Deployment*\n✅ Status: Success\n🏷️ Version: v${{ github.run_number }}\n🔗 URL: https://api.example.com"
                  }
                }
              ]
            }
```

---

## Workflow 3: Validate Ontology Changes

Create `.github/workflows/validate-ontology.yml`:

```yaml
name: Validate Ontology

on:
  pull_request:
    paths:
      - "ontology/**"
      - ".specify/**"

jobs:
  validate:
    runs-on: ubuntu-latest

    steps:
      - uses: actions/checkout@v4

      - name: Install ggen
        run: |
          curl -sSL https://ggen.example.com/install.sh | bash

      - name: Validate TTL syntax
        run: |
          for file in ontology/**/*.ttl; do
            echo "Validating $file..."
            ggen-cli validate-ttl "$file"
          done

      - name: Check SHACL compliance
        run: |
          ggen-cli validate --shacl shapes.ttl ontology/

      - name: Lint RDF
        run: |
          ggen-cli lint ontology/ \
            --check-prefixes \
            --check-unused-entities \
            --check-circular-refs

      - name: Dry-run generation
        run: |
          ggen-cli render --dry-run
          echo "Generation would succeed"

      - name: Verify determinism
        run: |
          # Generate twice and compare
          ggen-cli render -o dist1/
          ggen-cli render -o dist2/
          diff -r dist1/ dist2/ || exit 1
          echo "✓ Generation is deterministic"

      - name: Check for breaking changes
        if: github.event_name == 'pull_request'
        run: |
          git fetch origin ${{ github.base_ref }}
          ggen-cli compare \
            origin/${{ github.base_ref }}:ontology/ \
            HEAD:ontology/ \
            --check-breaking-changes
```

---

## Configuration Files

### Dockerfile

Create `Dockerfile` for API generation:

```dockerfile
FROM node:20-alpine AS builder

WORKDIR /app

# Install ggen
RUN npm install -g ggen-cli

# Copy ontology and config
COPY ontology/ ./ontology/
COPY templates/ ./templates/
COPY ggen.toml .

# Generate code
RUN ggen-cli render

# Build stage
FROM node:20-alpine

WORKDIR /app

COPY --from=builder /app/generated ./generated
COPY package*.json ./
COPY src ./src
COPY tsconfig.json ./

RUN npm ci --only=production

EXPOSE 3000

CMD ["node", "src/app.js"]
```

### docker-compose.yml

Create `docker-compose.yml` for local testing:

```yaml
version: '3.8'

services:
  api:
    build: .
    ports:
      - "3000:3000"
    environment:
      NODE_ENV: development
      DATABASE_URL: postgresql://user:pass@postgres:5432/api
    depends_on:
      - postgres
    volumes:
      - ./generated:/app/generated
      - ./src:/app/src

  postgres:
    image: postgres:15
    environment:
      POSTGRES_USER: user
      POSTGRES_PASSWORD: pass
      POSTGRES_DB: api
    ports:
      - "5432:5432"
    volumes:
      - postgres_data:/var/lib/postgresql/data

  redis:
    image: redis:7-alpine
    ports:
      - "6379:6379"

volumes:
  postgres_data:
```

---

## Best Practices

### 1. Protect Main Branch

Add branch protection rules:

```yaml
# In GitHub settings → Branches → Add rule

Pattern: main
- Require pull request reviews (1+)
- Require status checks to pass
  - All workflows in `.github/workflows/`
  - CodeQL analysis
  - Code coverage ≥80%
```

### 2. Cache Generated Artifacts

```yaml
- name: Cache generated code
  uses: actions/cache@v3
  with:
    path: |
      generated/
      dist/
    key: codegen-${{ hashFiles('ontology/**', 'ggen.toml') }}
```

### 3. Run Parallel Jobs

```yaml
matrix:
  job:
    - test-unit
    - test-integration
    - lint
    - build

jobs:
  ${{ matrix.job }}:
    runs-on: ubuntu-latest
    steps:
      - run: npm run ${{ matrix.job }}
```

### 4. Monitor Generation Time

```yaml
- name: Measure generation time
  run: |
    time ggen-cli render > /tmp/gen.log 2>&1
    DURATION=$(grep real /tmp/gen.log | awk '{print $2}')
    echo "DURATION=$DURATION" >> $GITHUB_ENV

- name: Warn if slow
  if: env.DURATION > 30
  uses: actions/github-script@v6
  with:
    script: |
      github.rest.issues.createComment({
        issue_number: context.issue.number,
        owner: context.repo.owner,
        repo: context.repo.repo,
        body: '⚠️ Code generation took {{ env.DURATION }} - consider optimizing'
      })
```

---

## See Also

- [PostgreSQL Integration](./POSTGRESQL_INTEGRATION_EXAMPLE.md) - Database CI/CD
- [JavaScript Express Example](./JAVASCRIPT_EXPRESS_EXAMPLE.md) - API testing
- [Performance Tuning](./PERFORMANCE_TUNING_GUIDE.md) - Optimize generation speed
