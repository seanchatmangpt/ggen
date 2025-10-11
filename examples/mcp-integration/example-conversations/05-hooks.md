# Example 5: Lifecycle Hooks and Automation

This example demonstrates using ggen-mcp hooks to automate tasks during template generation and project lifecycle.

## Understanding Lifecycle Hooks

Hooks are automation scripts that run at specific points during project generation:

- **pre_generate** - Before template processing starts
- **post_generate** - After all files are created
- **pre_file** - Before each individual file is created
- **post_file** - After each file is created
- **pre_validate** - Before template validation
- **post_validate** - After validation completes

## Registering a Simple Hook

### User:
```
Set up a hook to automatically format generated code
```

### Claude:
I'll register a post-generation hook to format code after project generation.

**Tool Call:** `hook_register`
```json
{
  "event": "post_generate",
  "command": "prettier --write .",
  "name": "format_code",
  "description": "Format JavaScript/TypeScript code with Prettier",
  "enabled": true
}
```

**Response:**
```json
{
  "status": "success",
  "hook": {
    "id": "hook-1704902400",
    "name": "format_code",
    "event": "post_generate",
    "command": "prettier --write .",
    "description": "Format JavaScript/TypeScript code with Prettier",
    "enabled": true,
    "created_at": "2024-01-10T16:40:00Z"
  },
  "message": "Hook registered successfully. Will run after project generation."
}
```

✅ **Hook registered!**

**Details:**
- 🎯 Event: post_generate
- 📝 Name: format_code
- 🔧 Command: `prettier --write .`
- ✅ Status: Enabled

This hook will automatically format all JavaScript/TypeScript files after generating any project.

**Test it:**
```bash
ggen project gen react-typescript-starter --vars project_name=my-app
# After generation, Prettier will auto-format all code
```

## Multi-Step Hook Automation

### User:
```
I want to automate: code formatting, dependency installation, git init, and initial commit
```

### Claude:
I'll create a comprehensive post-generation automation workflow with multiple hooks.

**Tool Call 1:** `hook_register` (Format Code)
```json
{
  "event": "post_generate",
  "command": "prettier --write . && eslint --fix .",
  "name": "format_and_lint",
  "description": "Format code and fix linting issues",
  "priority": 1,
  "enabled": true
}
```

**Tool Call 2:** `hook_register` (Install Dependencies)
```json
{
  "event": "post_generate",
  "command": "npm install",
  "name": "install_deps",
  "description": "Install NPM dependencies",
  "priority": 2,
  "enabled": true,
  "conditions": {
    "file_exists": "package.json"
  }
}
```

**Tool Call 3:** `hook_register` (Git Init)
```json
{
  "event": "post_generate",
  "command": "git init && git add . && git commit -m 'Initial commit from ggen template'",
  "name": "git_init",
  "description": "Initialize git repository and create initial commit",
  "priority": 3,
  "enabled": true
}
```

**Response (Combined):**
```json
{
  "status": "success",
  "hooks_registered": 3,
  "execution_order": [
    {
      "priority": 1,
      "name": "format_and_lint",
      "command": "prettier --write . && eslint --fix ."
    },
    {
      "priority": 2,
      "name": "install_deps",
      "command": "npm install",
      "condition": "package.json exists"
    },
    {
      "priority": 3,
      "name": "git_init",
      "command": "git init && git add . && git commit -m 'Initial commit...'"
    }
  ],
  "workflow_name": "post_generate_automation"
}
```

✅ **Automation workflow created!**

**Execution Order:**
1. 🎨 **Format & Lint** → Clean up code style
2. 📦 **Install Dependencies** → Run npm install (if package.json exists)
3. 🔄 **Git Init** → Initialize repo and commit

**When you generate a project:**
```bash
ggen project gen react-typescript-starter --vars project_name=my-app
```

**Automatic workflow:**
```bash
[1/4] 📄 Generating files...
[2/4] 🎨 Formatting and linting...
[3/4] 📦 Installing dependencies...
[4/4] 🔄 Initializing git repository...
✅ Project ready!
```

Your project will be fully set up and ready to use!

## Conditional Hooks

### User:
```
Run different build commands based on the project type
```

### Claude:
I'll set up conditional hooks that detect the project type and run appropriate commands.

**Tool Call 1:** `hook_register` (Rust Projects)
```json
{
  "event": "post_generate",
  "command": "cargo build && cargo test",
  "name": "rust_build",
  "description": "Build and test Rust project",
  "priority": 10,
  "enabled": true,
  "conditions": {
    "file_exists": "Cargo.toml",
    "template_type": "rust"
  }
}
```

**Tool Call 2:** `hook_register` (Python Projects)
```json
{
  "event": "post_generate",
  "command": "pip install -r requirements.txt && pytest",
  "name": "python_setup",
  "description": "Install dependencies and run tests for Python",
  "priority": 10,
  "enabled": true,
  "conditions": {
    "file_exists": "requirements.txt",
    "template_type": "python"
  }
}
```

**Tool Call 3:** `hook_register` (Node.js Projects)
```json
{
  "event": "post_generate",
  "command": "npm install && npm test",
  "name": "node_setup",
  "description": "Install dependencies and run tests for Node.js",
  "priority": 10,
  "enabled": true,
  "conditions": {
    "file_exists": "package.json",
    "template_type": "nodejs"
  }
}
```

**Response:**
```json
{
  "status": "success",
  "conditional_hooks": [
    {
      "name": "rust_build",
      "triggers_when": "Cargo.toml exists AND template_type=rust",
      "command": "cargo build && cargo test"
    },
    {
      "name": "python_setup",
      "triggers_when": "requirements.txt exists AND template_type=python",
      "command": "pip install -r requirements.txt && pytest"
    },
    {
      "name": "node_setup",
      "triggers_when": "package.json exists AND template_type=nodejs",
      "command": "npm install && npm test"
    }
  ],
  "message": "Conditional hooks registered. Only matching hook will execute."
}
```

✅ **Smart conditional hooks configured!**

**How it works:**

For **Rust projects:**
```bash
ggen project gen rust-cli-boilerplate
# Triggers: cargo build && cargo test
```

For **Python projects:**
```bash
ggen project gen fastapi-microservice
# Triggers: pip install -r requirements.txt && pytest
```

For **Node.js projects:**
```bash
ggen project gen express-api
# Triggers: npm install && npm test
```

Only the relevant hook runs based on project type detection!

## Docker Container Hooks

### User:
```
Automatically build Docker images and start containers for projects with Docker support
```

### Claude:
I'll create hooks to automate Docker workflows.

**Tool Call 1:** `hook_register` (Docker Build)
```json
{
  "event": "post_generate",
  "command": "docker build -t {{project_name}} .",
  "name": "docker_build",
  "description": "Build Docker image from Dockerfile",
  "priority": 20,
  "enabled": true,
  "conditions": {
    "file_exists": "Dockerfile"
  }
}
```

**Tool Call 2:** `hook_register` (Docker Compose)
```json
{
  "event": "post_generate",
  "command": "docker-compose up -d",
  "name": "docker_compose_up",
  "description": "Start services with docker-compose",
  "priority": 21,
  "enabled": true,
  "conditions": {
    "file_exists": "docker-compose.yml"
  },
  "variables": {
    "detached": true
  }
}
```

**Tool Call 3:** `hook_register` (Health Check)
```json
{
  "event": "post_generate",
  "command": "sleep 5 && curl -f http://localhost:{{port}}/health || echo 'Service starting...'",
  "name": "health_check",
  "description": "Check if service is healthy",
  "priority": 22,
  "enabled": true,
  "conditions": {
    "file_exists": "docker-compose.yml",
    "variable_exists": "port"
  }
}
```

**Response:**
```json
{
  "status": "success",
  "docker_workflow": {
    "hooks": [
      {
        "step": 1,
        "name": "docker_build",
        "action": "Build Docker image"
      },
      {
        "step": 2,
        "name": "docker_compose_up",
        "action": "Start containers in detached mode"
      },
      {
        "step": 3,
        "name": "health_check",
        "action": "Verify service health"
      }
    ],
    "triggers_when": "Dockerfile or docker-compose.yml present"
  }
}
```

✅ **Docker automation workflow configured!**

**Workflow:**
1. 🐳 Build Docker image with project name
2. 🚀 Start containers via docker-compose
3. 💚 Health check on configured port

**Example generation:**
```bash
ggen project gen fastapi-microservice --vars project_name=api --vars port=8000
```

**Automatic Docker setup:**
```bash
[1/5] 📄 Generating files...
[2/5] 🐳 Building Docker image 'api'...
[3/5] 🚀 Starting containers...
[4/5] 💚 Health checking http://localhost:8000/health...
[5/5] ✅ Service is running!

Your service is live at: http://localhost:8000
API docs: http://localhost:8000/docs
```

## Pre-Generation Validation Hooks

### User:
```
Before generating, check if required tools are installed
```

### Claude:
I'll create pre-generation hooks to validate the environment.

**Tool Call:** `hook_register`
```json
{
  "event": "pre_generate",
  "command": "command -v node && command -v npm && command -v git || (echo 'Missing required tools: node, npm, or git' && exit 1)",
  "name": "check_tools",
  "description": "Verify required development tools are installed",
  "priority": 1,
  "enabled": true,
  "fail_on_error": true
}
```

**Response:**
```json
{
  "status": "success",
  "hook": {
    "name": "check_tools",
    "event": "pre_generate",
    "validation": true,
    "fail_on_error": true,
    "checks": [
      "node",
      "npm",
      "git"
    ]
  },
  "message": "Pre-generation validation hook registered. Generation will abort if tools are missing."
}
```

✅ **Environment validation configured!**

**When generating:**
```bash
ggen project gen react-typescript-starter
```

**Validation runs first:**
```bash
[1/1] 🔍 Checking required tools...
  ✅ node found (v18.17.0)
  ✅ npm found (v9.6.7)
  ✅ git found (v2.40.0)

[2/5] 📄 Generating files...
```

**If a tool is missing:**
```bash
[1/1] 🔍 Checking required tools...
  ✅ node found
  ❌ npm not found
  ✅ git found

❌ Missing required tools: npm
Please install missing tools and try again.
Generation aborted.
```

## File-Level Hooks

### User:
```
Automatically make shell scripts executable when they're created
```

### Claude:
I'll create a post-file hook that runs after each file is generated.

**Tool Call:** `hook_register`
```json
{
  "event": "post_file",
  "command": "chmod +x {{file_path}}",
  "name": "make_scripts_executable",
  "description": "Make shell scripts executable",
  "enabled": true,
  "conditions": {
    "file_pattern": "*.sh",
    "file_type": "script"
  }
}
```

**Response:**
```json
{
  "status": "success",
  "hook": {
    "name": "make_scripts_executable",
    "event": "post_file",
    "scope": "per_file",
    "matches": "*.sh files",
    "command": "chmod +x {{file_path}}"
  },
  "message": "File-level hook registered. Runs for each matching file."
}
```

✅ **File-level automation configured!**

**During generation:**
```bash
[1/10] 📄 Creating scripts/deploy.sh...
        🔧 Hook: make_scripts_executable
        ✅ Made executable

[2/10] 📄 Creating scripts/test.sh...
        🔧 Hook: make_scripts_executable
        ✅ Made executable

[3/10] 📄 Creating README.md...
        (no matching hooks)
```

All `.sh` files are automatically made executable!

## Template-Specific Hooks

### User:
```
Create a hook that only runs for FastAPI projects to set up the database
```

### Claude:
I'll create a template-specific hook for FastAPI projects.

**Tool Call:** `hook_register`
```json
{
  "event": "post_generate",
  "command": "alembic upgrade head && python -m app.init_db",
  "name": "fastapi_db_setup",
  "description": "Run migrations and initialize database for FastAPI",
  "priority": 15,
  "enabled": true,
  "conditions": {
    "template_name": "fastapi-*",
    "file_exists": "alembic.ini"
  },
  "environment": {
    "DATABASE_URL": "postgresql://localhost/{{database_name}}"
  }
}
```

**Response:**
```json
{
  "status": "success",
  "hook": {
    "name": "fastapi_db_setup",
    "event": "post_generate",
    "applies_to": "fastapi-* templates",
    "requires": "alembic.ini",
    "steps": [
      "Run Alembic migrations (upgrade head)",
      "Initialize database with seed data"
    ],
    "environment_vars": {
      "DATABASE_URL": "postgresql://localhost/{{database_name}}"
    }
  }
}
```

✅ **Template-specific hook configured!**

**Only runs for FastAPI templates:**
```bash
# This triggers the hook
ggen project gen fastapi-microservice --vars database_name=myapp_db

[1/6] 📄 Generating files...
[2/6] 📦 Installing dependencies...
[3/6] 🗄️  Running database migrations...
       alembic upgrade head
[4/6] 🌱 Initializing database...
       python -m app.init_db
[5/6] ✅ Database ready!
```

**Does NOT run for other templates:**
```bash
# Hook skipped (not a FastAPI template)
ggen project gen express-api
```

## Listing and Managing Hooks

### User:
```
Show me all my registered hooks
```

### Claude:
Let me query your registered hooks from the system.

```bash
# Via CLI
ggen hook list
```

**Output:**
```json
{
  "hooks": [
    {
      "id": "hook-1",
      "name": "format_and_lint",
      "event": "post_generate",
      "priority": 1,
      "enabled": true,
      "runs_count": 23
    },
    {
      "id": "hook-2",
      "name": "install_deps",
      "event": "post_generate",
      "priority": 2,
      "enabled": true,
      "conditions": ["package.json exists"],
      "runs_count": 18
    },
    {
      "id": "hook-3",
      "name": "git_init",
      "event": "post_generate",
      "priority": 3,
      "enabled": true,
      "runs_count": 23
    },
    {
      "id": "hook-4",
      "name": "docker_build",
      "event": "post_generate",
      "priority": 20,
      "enabled": true,
      "conditions": ["Dockerfile exists"],
      "runs_count": 7
    },
    {
      "id": "hook-5",
      "name": "check_tools",
      "event": "pre_generate",
      "priority": 1,
      "enabled": true,
      "validation": true,
      "runs_count": 23
    }
  ],
  "total_hooks": 5,
  "total_executions": 94
}
```

**📋 Registered Hooks:**

**Pre-Generation:**
1. ✅ **check_tools** (validation) - Runs: 23

**Post-Generation:**
1. ✅ **format_and_lint** (priority 1) - Runs: 23
2. ✅ **install_deps** (priority 2, conditional) - Runs: 18
3. ✅ **git_init** (priority 3) - Runs: 23
4. ✅ **docker_build** (priority 20, conditional) - Runs: 7

**Statistics:**
- Total hooks: 5
- Total executions: 94
- Most used: format_and_lint (23 times)

## Disabling/Enabling Hooks

### User:
```
Temporarily disable the git_init hook
```

### Claude:
```bash
# Via CLI
ggen hook disable git_init
```

**Output:**
```
✅ Hook 'git_init' disabled
It will not run until re-enabled with: ggen hook enable git_init
```

**To re-enable:**
```bash
ggen hook enable git_init
```

## Hook Execution Logs

### User:
```
Show me the logs from the last hook execution
```

### Claude:
```bash
# Via CLI
ggen hook logs --last
```

**Output:**
```
Hook Execution Log - 2024-01-10 16:45:23
Project: my-api (fastapi-microservice)

[1/5] pre_generate/check_tools
  Command: command -v node && command -v npm && command -v git
  Status: ✅ Success (0.12s)
  Output:
    /usr/local/bin/node
    /usr/local/bin/npm
    /usr/bin/git

[2/5] post_generate/format_and_lint
  Command: prettier --write . && eslint --fix .
  Status: ✅ Success (2.34s)
  Output:
    Formatted 12 files
    Fixed 3 linting issues

[3/5] post_generate/install_deps
  Command: npm install
  Status: ✅ Success (18.92s)
  Output:
    added 247 packages in 18s

[4/5] post_generate/git_init
  Command: git init && git add . && git commit -m 'Initial commit'
  Status: ✅ Success (0.45s)
  Output:
    Initialized empty Git repository
    [main (root-commit) a1b2c3d] Initial commit
    23 files changed, 1234 insertions(+)

[5/5] post_generate/docker_build
  Command: docker build -t my-api .
  Status: ✅ Success (45.23s)
  Output:
    Successfully built abc123def456
    Successfully tagged my-api:latest

Total duration: 67.06s
All hooks completed successfully
```

## Best Practices for Hooks

1. **Use priority ordering** - Control execution order with priority numbers
2. **Add conditions** - Use conditions to run hooks only when needed
3. **Handle errors** - Set `fail_on_error` for critical hooks
4. **Log everything** - Hooks are logged for debugging
5. **Keep hooks fast** - Long-running hooks slow down generation
6. **Use variables** - Reference template variables with `{{var}}`
7. **Test thoroughly** - Verify hooks work across different projects

## Key Takeaways

1. **Register:** Use `hook_register` to create automation
2. **Events:** Choose from pre/post_generate, pre/post_file, pre/post_validate
3. **Conditions:** Add file_exists, template_type, or custom conditions
4. **Priority:** Control execution order with priority numbers
5. **Variables:** Use `{{variable}}` to access template variables
6. **Management:** Enable/disable hooks as needed

## Related Tools

- `project_gen` - Triggers hooks during generation
- `template_validate` - Can trigger validation hooks
- `market_install` - Some packages include pre-configured hooks
- `ai_generate_template` - Can suggest relevant hooks
