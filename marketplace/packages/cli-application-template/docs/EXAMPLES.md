# CLI Application Examples

Real-world examples of CLI applications you can generate with this template.

## 1. File Converter CLI

Convert between different file formats.

```turtle
@prefix cli: <http://ggen.dev/ontology/cli#> .

:Converter a cli:CLIApplication ;
    cli:commandName "convert" ;
    cli:commandAbout "Convert files between formats" ;
    cli:hasCommand :ConvertCmd .

:ConvertCmd a cli:Command ;
    cli:commandName "file" ;
    cli:hasArgument :InputFile, :OutputFile ;
    cli:hasOption :FormatOpt, :CompressionOpt .

:InputFile a cli:Argument ;
    cli:argumentName "input" ;
    cli:argumentType cli:PathType ;
    cli:argumentRequired true ;
    cli:argumentHelp "Input file path" .

:OutputFile a cli:Argument ;
    cli:argumentName "output" ;
    cli:argumentType cli:PathType ;
    cli:argumentRequired true ;
    cli:argumentHelp "Output file path" .

:FormatOpt a cli:Option ;
    cli:optionLong "format" ;
    cli:optionShort "f" ;
    cli:optionType cli:EnumType ;
    cli:optionEnumValues "json,yaml,toml,xml" ;
    cli:optionRequired true .

:CompressionOpt a cli:Option ;
    cli:optionLong "compress" ;
    cli:optionType cli:BooleanType ;
    cli:optionDefault "false" .
```

**Usage:**
```bash
convert file input.json output.yaml --format yaml
convert file data.xml result.json --format json --compress
```

## 2. Database Migration CLI

Manage database migrations.

```turtle
:DBMigrate a cli:CLIApplication ;
    cli:commandName "dbmigrate" ;
    cli:hasCommand :Create, :Up, :Down, :Status ;
    cli:hasConfig [
        a cli:ConfigFile ;
        cli:configFormat "yaml" ;
        cli:configPath "./migrations.yaml"
    ] .

:Create a cli:Command ;
    cli:commandName "create" ;
    cli:commandAbout "Create a new migration" ;
    cli:hasArgument [
        cli:argumentName "name" ;
        cli:argumentRequired true ;
        cli:argumentHelp "Migration name"
    ] .

:Up a cli:Command ;
    cli:commandName "up" ;
    cli:commandAbout "Run pending migrations" ;
    cli:hasOption [
        cli:optionLong "steps" ;
        cli:optionType cli:IntegerType ;
        cli:optionHelp "Number of migrations to run"
    ] .

:Down a cli:Command ;
    cli:commandName "down" ;
    cli:commandAbout "Rollback migrations" ;
    cli:hasOption [
        cli:optionLong "steps" ;
        cli:optionType cli:IntegerType ;
        cli:optionDefault "1"
    ] .

:Status a cli:Command ;
    cli:commandName "status" ;
    cli:commandAbout "Show migration status" .
```

**Usage:**
```bash
dbmigrate create add_users_table
dbmigrate up
dbmigrate down --steps 2
dbmigrate status
```

## 3. API Testing CLI

Test REST APIs.

```turtle
:APITest a cli:CLIApplication ;
    cli:commandName "apitest" ;
    cli:hasCommand :Get, :Post, :Put, :Delete .

:Get a cli:Command ;
    cli:commandName "get" ;
    cli:hasArgument [
        cli:argumentName "url" ;
        cli:argumentRequired true ;
        cli:hasValidator [
            cli:validatorType "regex" ;
            cli:validatorPattern "^https?://.+" ;
            cli:validatorErrorMessage "Invalid URL"
        ]
    ] ;
    cli:hasOption [
        cli:optionLong "header" ;
        cli:optionShort "H" ;
        cli:optionMultiple true
    ] , [
        cli:optionLong "auth" ;
        cli:optionShort "a" ;
        cli:optionHelp "Bearer token"
    ] .

:Post a cli:Command ;
    cli:commandName "post" ;
    cli:hasArgument [
        cli:argumentName "url" ;
        cli:argumentRequired true
    ] ;
    cli:hasOption [
        cli:optionLong "data" ;
        cli:optionShort "d" ;
        cli:optionHelp "Request body (JSON)"
    ] , [
        cli:optionLong "file" ;
        cli:optionShort "f" ;
        cli:optionType cli:PathType
    ] .
```

**Usage:**
```bash
apitest get https://api.example.com/users -H "Content-Type: application/json"
apitest post https://api.example.com/users -d '{"name": "John"}'
apitest post https://api.example.com/upload -f ./data.json
```

## 4. Code Generator CLI

Generate boilerplate code.

```turtle
:CodeGen a cli:CLIApplication ;
    cli:commandName "codegen" ;
    cli:hasCommand :New, :Scaffold, :Component .

:New a cli:Command ;
    cli:commandName "new" ;
    cli:hasArgument [
        cli:argumentName "name" ;
        cli:argumentRequired true
    ] ;
    cli:hasPrompt [
        cli:promptType "select" ;
        cli:promptMessage "Choose template:" ;
        cli:promptChoices "react,vue,angular,svelte"
    ] , [
        cli:promptType "confirm" ;
        cli:promptMessage "Include TypeScript?" ;
        cli:promptName "typescript"
    ] .

:Scaffold a cli:Command ;
    cli:commandName "scaffold" ;
    cli:hasArgument [
        cli:argumentName "type" ;
        cli:argumentType cli:EnumType ;
        cli:optionEnumValues "model,controller,service,component"
    ] ;
    cli:hasArgument [
        cli:argumentName "name" ;
        cli:argumentRequired true
    ] .

:Component a cli:Command ;
    cli:commandName "component" ;
    cli:hasArgument [
        cli:argumentName "name" ;
        cli:argumentRequired true
    ] ;
    cli:hasOption [
        cli:optionLong "style" ;
        cli:optionType cli:EnumType ;
        cli:optionEnumValues "css,scss,less,styled"
    ] .
```

**Usage:**
```bash
codegen new my-app
codegen scaffold model User
codegen component Button --style scss
```

## 5. Docker Manager CLI

Manage Docker containers and images.

```turtle
:DockerMgr a cli:CLIApplication ;
    cli:commandName "dmgr" ;
    cli:hasCommand :Container, :Image, :Network .

:Container a cli:Command ;
    cli:commandName "container" ;
    cli:hasSubcommand :List, :Start, :Stop, :Logs .

:List a cli:Subcommand ;
    cli:commandName "list" ;
    cli:hasOption [
        cli:optionLong "all" ;
        cli:optionShort "a" ;
        cli:optionType cli:BooleanType
    ] , [
        cli:optionLong "filter" ;
        cli:optionMultiple true
    ] .

:Start a cli:Subcommand ;
    cli:commandName "start" ;
    cli:hasArgument [
        cli:argumentName "container_id" ;
        cli:argumentRequired true
    ] .

:Logs a cli:Subcommand ;
    cli:commandName "logs" ;
    cli:hasArgument [
        cli:argumentName "container_id" ;
        cli:argumentRequired true
    ] ;
    cli:hasOption [
        cli:optionLong "follow" ;
        cli:optionShort "f" ;
        cli:optionType cli:BooleanType
    ] , [
        cli:optionLong "tail" ;
        cli:optionType cli:IntegerType ;
        cli:optionDefault "100"
    ] .
```

**Usage:**
```bash
dmgr container list --all
dmgr container start abc123
dmgr container logs abc123 --follow --tail 50
```

## 6. Git Workflow CLI

Custom Git workflow automation.

```turtle
:GitFlow a cli:CLIApplication ;
    cli:commandName "gflow" ;
    cli:hasCommand :Feature, :Release, :Hotfix .

:Feature a cli:Command ;
    cli:commandName "feature" ;
    cli:hasSubcommand :FeatureStart, :FeatureFinish .

:FeatureStart a cli:Subcommand ;
    cli:commandName "start" ;
    cli:hasArgument [
        cli:argumentName "name" ;
        cli:argumentRequired true ;
        cli:hasValidator [
            cli:validatorType "regex" ;
            cli:validatorPattern "^[a-z0-9-]+$" ;
            cli:validatorErrorMessage "Feature name must be lowercase with hyphens"
        ]
    ] .

:FeatureFinish a cli:Subcommand ;
    cli:commandName "finish" ;
    cli:hasOption [
        cli:optionLong "no-merge" ;
        cli:optionType cli:BooleanType
    ] , [
        cli:optionLong "delete-branch" ;
        cli:optionType cli:BooleanType ;
        cli:optionDefault "true"
    ] .

:Release a cli:Command ;
    cli:commandName "release" ;
    cli:hasArgument [
        cli:argumentName "version" ;
        cli:argumentRequired true ;
        cli:hasValidator [
            cli:validatorType "regex" ;
            cli:validatorPattern "^\\d+\\.\\d+\\.\\d+$" ;
            cli:validatorErrorMessage "Version must be semver (e.g., 1.2.3)"
        ]
    ] .
```

**Usage:**
```bash
gflow feature start user-authentication
gflow feature finish --no-merge
gflow release 1.2.0
```

## 7. Server Deployment CLI

Deploy applications to servers.

```turtle
:Deploy a cli:CLIApplication ;
    cli:commandName "deploy" ;
    cli:hasCommand :App, :Database, :Rollback ;
    cli:hasConfig [
        a cli:ConfigFile ;
        cli:configFormat "toml" ;
        cli:configPath "./deploy.toml"
    ] , [
        a cli:EnvVar ;
        cli:envVarPrefix "DEPLOY_"
    ] .

:App a cli:Command ;
    cli:commandName "app" ;
    cli:hasArgument [
        cli:argumentName "environment" ;
        cli:argumentType cli:EnumType ;
        cli:optionEnumValues "dev,staging,prod"
    ] ;
    cli:hasOption [
        cli:optionLong "tag" ;
        cli:optionHelp "Docker image tag"
    ] , [
        cli:optionLong "migrate" ;
        cli:optionType cli:BooleanType ;
        cli:optionDefault "true"
    ] .

:Database a cli:Command ;
    cli:commandName "database" ;
    cli:hasArgument [
        cli:argumentName "environment" ;
        cli:argumentType cli:EnumType ;
        cli:optionEnumValues "dev,staging,prod"
    ] ;
    cli:hasOption [
        cli:optionLong "backup" ;
        cli:optionType cli:BooleanType ;
        cli:optionDefault "true"
    ] .

:Rollback a cli:Command ;
    cli:commandName "rollback" ;
    cli:hasArgument [
        cli:argumentName "environment"
    ] ;
    cli:hasOption [
        cli:optionLong "version" ;
        cli:optionRequired true
    ] .
```

**Usage:**
```bash
deploy app staging --tag v1.2.0
deploy database prod --backup
deploy rollback prod --version v1.1.0
```

## 8. Package Manager CLI

Manage project dependencies.

```turtle
:PkgMgr a cli:CLIApplication ;
    cli:commandName "pkgmgr" ;
    cli:hasCommand :Install, :Update, :Remove, :List, :Search .

:Install a cli:Command ;
    cli:commandName "install" ;
    cli:hasArgument [
        cli:argumentName "packages" ;
        cli:argumentRequired false ;
        cli:argumentMultiple true
    ] ;
    cli:hasOption [
        cli:optionLong "save-dev" ;
        cli:optionShort "D" ;
        cli:optionType cli:BooleanType
    ] , [
        cli:optionLong "global" ;
        cli:optionShort "g" ;
        cli:optionType cli:BooleanType
    ] .

:Update a cli:Command ;
    cli:commandName "update" ;
    cli:hasArgument [
        cli:argumentName "packages" ;
        cli:argumentMultiple true
    ] .

:Search a cli:Command ;
    cli:commandName "search" ;
    cli:hasArgument [
        cli:argumentName "query" ;
        cli:argumentRequired true
    ] ;
    cli:hasOption [
        cli:optionLong "limit" ;
        cli:optionType cli:IntegerType ;
        cli:optionDefault "10"
    ] .
```

**Usage:**
```bash
pkgmgr install
pkgmgr install lodash axios --save-dev
pkgmgr update lodash
pkgmgr search react
```

## 9. Test Runner CLI

Run and manage tests.

```turtle
:TestRunner a cli:CLIApplication ;
    cli:commandName "testrun" ;
    cli:hasCommand :Run, :Watch, :Coverage .

:Run a cli:Command ;
    cli:commandName "run" ;
    cli:hasArgument [
        cli:argumentName "pattern" ;
        cli:argumentHelp "Test file pattern"
    ] ;
    cli:hasOption [
        cli:optionLong "parallel" ;
        cli:optionShort "p" ;
        cli:optionType cli:BooleanType
    ] , [
        cli:optionLong "workers" ;
        cli:optionType cli:IntegerType ;
        cli:optionDefault "4" ;
        cli:hasValidator [
            cli:validatorType "range" ;
            cli:validatorMin 1 ;
            cli:validatorMax 32
        ]
    ] , [
        cli:optionLong "timeout" ;
        cli:optionType cli:IntegerType ;
        cli:optionDefault "5000"
    ] .

:Watch a cli:Command ;
    cli:commandName "watch" ;
    cli:hasOption [
        cli:optionLong "include" ;
        cli:optionMultiple true
    ] .

:Coverage a cli:Command ;
    cli:commandName "coverage" ;
    cli:hasOption [
        cli:optionLong "threshold" ;
        cli:optionType cli:IntegerType ;
        cli:optionDefault "80" ;
        cli:hasValidator [
            cli:validatorType "range" ;
            cli:validatorMin 0 ;
            cli:validatorMax 100
        ]
    ] .
```

**Usage:**
```bash
testrun run "**/*.test.js" --parallel --workers 8
testrun watch --include src/**
testrun coverage --threshold 90
```

## 10. File Backup CLI

Backup and restore files.

```turtle
:Backup a cli:CLIApplication ;
    cli:commandName "backup" ;
    cli:hasCommand :Create, :Restore, :List, :Delete .

:Create a cli:Command ;
    cli:commandName "create" ;
    cli:hasArgument [
        cli:argumentName "source" ;
        cli:argumentType cli:PathType ;
        cli:argumentRequired true
    ] ;
    cli:hasOption [
        cli:optionLong "name" ;
        cli:optionHelp "Backup name"
    ] , [
        cli:optionLong "compress" ;
        cli:optionType cli:BooleanType ;
        cli:optionDefault "true"
    ] , [
        cli:optionLong "exclude" ;
        cli:optionMultiple true
    ] .

:Restore a cli:Command ;
    cli:commandName "restore" ;
    cli:hasArgument [
        cli:argumentName "backup_id" ;
        cli:argumentRequired true
    ] ;
    cli:hasArgument [
        cli:argumentName "destination" ;
        cli:argumentType cli:PathType ;
        cli:argumentRequired true
    ] ;
    cli:hasOption [
        cli:optionLong "overwrite" ;
        cli:optionType cli:BooleanType
    ] .

:List a cli:Command ;
    cli:commandName "list" ;
    cli:hasOption [
        cli:optionLong "sort" ;
        cli:optionType cli:EnumType ;
        cli:optionEnumValues "date,size,name"
    ] .
```

**Usage:**
```bash
backup create /home/user/docs --name daily-backup --exclude "*.tmp"
backup list --sort date
backup restore backup-123 /restore/path --overwrite
```

## 11. Log Analyzer CLI

Analyze log files.

```turtle
:LogAnalyzer a cli:CLIApplication ;
    cli:commandName "logz" ;
    cli:hasCommand :Parse, :Filter, :Stats .

:Parse a cli:Command ;
    cli:commandName "parse" ;
    cli:hasArgument [
        cli:argumentName "logfile" ;
        cli:argumentType cli:PathType ;
        cli:argumentRequired true
    ] ;
    cli:hasOption [
        cli:optionLong "format" ;
        cli:optionType cli:EnumType ;
        cli:optionEnumValues "json,csv,table"
    ] .

:Filter a cli:Command ;
    cli:commandName "filter" ;
    cli:hasArgument [
        cli:argumentName "logfile" ;
        cli:argumentType cli:PathType
    ] ;
    cli:hasOption [
        cli:optionLong "level" ;
        cli:optionType cli:EnumType ;
        cli:optionEnumValues "DEBUG,INFO,WARN,ERROR"
    ] , [
        cli:optionLong "after" ;
        cli:optionHelp "Start date (YYYY-MM-DD)"
    ] , [
        cli:optionLong "grep" ;
        cli:optionHelp "Search pattern"
    ] .

:Stats a cli:Command ;
    cli:commandName "stats" ;
    cli:hasArgument [
        cli:argumentName "logfile" ;
        cli:argumentType cli:PathType
    ] .
```

**Usage:**
```bash
logz parse app.log --format json
logz filter app.log --level ERROR --after 2024-01-01
logz stats app.log
```

## 12. Cloud Resource Manager

Manage cloud infrastructure.

```turtle
:CloudMgr a cli:CLIApplication ;
    cli:commandName "cloudmgr" ;
    cli:hasCommand :Instance, :Storage, :Network ;
    cli:hasConfig [
        a cli:EnvVar ;
        cli:envVarPrefix "CLOUD_"
    ] .

:Instance a cli:Command ;
    cli:commandName "instance" ;
    cli:hasSubcommand :InstanceCreate, :InstanceList, :InstanceStop .

:InstanceCreate a cli:Subcommand ;
    cli:commandName "create" ;
    cli:hasOption [
        cli:optionLong "type" ;
        cli:optionRequired true ;
        cli:optionType cli:EnumType ;
        cli:optionEnumValues "t2.micro,t2.small,t2.medium"
    ] , [
        cli:optionLong "region" ;
        cli:optionRequired true
    ] , [
        cli:optionLong "count" ;
        cli:optionType cli:IntegerType ;
        cli:optionDefault "1"
    ] .

:InstanceList a cli:Subcommand ;
    cli:commandName "list" ;
    cli:hasOption [
        cli:optionLong "region" ;
        cli:optionMultiple true
    ] .
```

**Usage:**
```bash
export CLOUD_API_KEY=xxx
cloudmgr instance create --type t2.micro --region us-east-1
cloudmgr instance list --region us-east-1 --region us-west-2
```

## 13. Email CLI

Send emails from command line.

```turtle
:EmailCLI a cli:CLIApplication ;
    cli:commandName "mail" ;
    cli:hasCommand :Send, :Template .

:Send a cli:Command ;
    cli:commandName "send" ;
    cli:hasOption [
        cli:optionLong "to" ;
        cli:optionRequired true ;
        cli:hasValidator [
            cli:validatorType "regex" ;
            cli:validatorPattern "^[^@]+@[^@]+\\.[^@]+$"
        ]
    ] , [
        cli:optionLong "subject" ;
        cli:optionRequired true
    ] , [
        cli:optionLong "body" ;
        cli:optionHelp "Email body"
    ] , [
        cli:optionLong "file" ;
        cli:optionType cli:PathType ;
        cli:optionHelp "Body from file"
    ] , [
        cli:optionLong "attachment" ;
        cli:optionMultiple true
    ] .
```

**Usage:**
```bash
mail send --to user@example.com --subject "Test" --body "Hello!"
mail send --to user@example.com --subject "Report" --file ./body.html --attachment report.pdf
```

## 14. Crypto Wallet CLI

Manage cryptocurrency wallets.

```turtle
:Wallet a cli:CLIApplication ;
    cli:commandName "wallet" ;
    cli:hasCommand :Create, :Balance, :Send, :Receive .

:Create a cli:Command ;
    cli:commandName "create" ;
    cli:hasPrompt [
        cli:promptType "password" ;
        cli:promptMessage "Enter passphrase:"
    ] .

:Balance a cli:Command ;
    cli:commandName "balance" ;
    cli:hasOption [
        cli:optionLong "address" ;
        cli:optionRequired true
    ] .

:Send a cli:Command ;
    cli:commandName "send" ;
    cli:hasOption [
        cli:optionLong "to" ;
        cli:optionRequired true
    ] , [
        cli:optionLong "amount" ;
        cli:optionType cli:FloatType ;
        cli:optionRequired true ;
        cli:hasValidator [
            cli:validatorType "range" ;
            cli:validatorMin 0.001 ;
            cli:validatorErrorMessage "Minimum amount is 0.001"
        ]
    ] .
```

**Usage:**
```bash
wallet create
wallet balance --address 0xabc123
wallet send --to 0xdef456 --amount 1.5
```

## 15. Kubernetes CLI Wrapper

Simplified Kubernetes management.

```turtle
:K8s a cli:CLIApplication ;
    cli:commandName "k8s" ;
    cli:hasCommand :Deploy, :Scale, :Logs, :Exec .

:Deploy a cli:Command ;
    cli:commandName "deploy" ;
    cli:hasArgument [
        cli:argumentName "manifest" ;
        cli:argumentType cli:PathType ;
        cli:argumentRequired true
    ] ;
    cli:hasOption [
        cli:optionLong "namespace" ;
        cli:optionShort "n" ;
        cli:optionDefault "default"
    ] .

:Scale a cli:Command ;
    cli:commandName "scale" ;
    cli:hasArgument [
        cli:argumentName "deployment"
    ] ;
    cli:hasOption [
        cli:optionLong "replicas" ;
        cli:optionType cli:IntegerType ;
        cli:optionRequired true ;
        cli:hasValidator [
            cli:validatorType "range" ;
            cli:validatorMin 0 ;
            cli:validatorMax 100
        ]
    ] .

:Logs a cli:Command ;
    cli:commandName "logs" ;
    cli:hasArgument [
        cli:argumentName "pod"
    ] ;
    cli:hasOption [
        cli:optionLong "follow" ;
        cli:optionShort "f" ;
        cli:optionType cli:BooleanType
    ] .
```

**Usage:**
```bash
k8s deploy app.yaml --namespace production
k8s scale myapp --replicas 5
k8s logs mypod-123 --follow
```

These examples demonstrate the full power and flexibility of the CLI Application Template for generating production-ready command-line tools across multiple domains.
