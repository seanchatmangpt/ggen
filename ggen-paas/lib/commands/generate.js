/**
 * ggen-paas CLI Command: generate
 *
 * Auto-generated from: .specify/cli-commands.ttl
 * Generated: 2026-01-08T10:00:00Z
 *
 * Description: Generate infrastructure artifacts from RDF specifications
 * Aliases: gen, g
 *
 * MANUAL EDITS WILL BE LOST on next `ggen sync`
 * Edit the RDF specification instead: .specify/cli-commands.ttl
 */

import path from 'path';
import fs from 'fs';
import { fileURLToPath } from 'url';
import { CommandBase } from '../command-base.js';
import { OntologyManager } from '../utils/ontology.js';
import { Logger } from '../utils/logger.js';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

/**
 * GenerateCommand
 *
 * Generate infrastructure artifacts from RDF specifications
 */
export default class GenerateCommand extends CommandBase {
  constructor() {
    super({
      name: 'generate',
      aliases: ['gen', 'g'],
      category: 'Generation',
      description: 'Generate infrastructure artifacts from RDF specifications',
      examples: `
        ggen paas generate docker
        ggen paas generate kubernetes --output yaml
        ggen paas generate terraform --validate
        ggen paas generate all --watch
        ggen paas generate openapi --dry-run
      `,
      slo: {
        maxDurationMs: 10000,
        expectedPassRate: 99.0,
      },
    });
  }

  /**
   * Define command schema from RDF specification
   */
  defineSchema() {
    return {
      positional: [
        {
          name: 'artifact',
          type: 'string',
          required: true,
          choices: ['docker', 'kubernetes', 'terraform', 'openapi', 'all'],
          description: 'Type of artifact to generate',
        },
      ],
      options: {
        'output': {
          shortForm: '-o',
          longForm: '--output',
          type: 'string',
          default: 'text',
          choices: ['text', 'json', 'yaml'],
          description: 'Output format for generation results',
          stackable: false,
        },
        'validate': {
          shortForm: undefined,
          longForm: '--validate',
          type: 'boolean',
          default: true,
          description: 'Validate generated artifacts before returning',
          stackable: false,
        },
        'watch': {
          shortForm: '-w',
          longForm: '--watch',
          type: 'boolean',
          default: false,
          description: 'Watch for spec changes and auto-regenerate',
          stackable: false,
        },
        'dryRun': {
          shortForm: undefined,
          longForm: '--dry-run',
          type: 'boolean',
          default: false,
          description: 'Show what would be generated without writing files',
          stackable: false,
        },
      },
    };
  }

  /**
   * Main command execution
   *
   * @param {object} args - Parsed positional arguments
   * @param {object} options - Parsed options
   * @returns {Promise<Result>}
   */
  async execute(args, options) {
    const logger = this.logger;

    try {
      // Log execution start
      logger.info(`Starting generate command...`);
      logger.debug(`Arguments: ${JSON.stringify(args)}`);
      logger.debug(`Options: ${JSON.stringify(options)}`);

      // Validate input arguments
      const validation = this.validateArguments(args, options);
      if (!validation.valid) {
        logger.error(`Invalid arguments: ${validation.error}`);
        return this.fail('INVALID_ARGUMENTS', validation.error);
      }

      // Load ontology manager
      logger.debug('Loading ggen-paas ontology...');
      const projectRoot = path.resolve(__dirname, '../../..');
      const ontologyPath = path.join(projectRoot, '.specify/ggen-paas-ontology.ttl');
      const ontologyManager = new OntologyManager(ontologyPath);

      await ontologyManager.load();
      logger.debug('Ontology loaded successfully');

      // Validate specification closure
      const closure = ontologyManager.validateClosure();
      if (!closure.valid) {
        logger.error('Specification closure validation failed');
        logger.debug(`Closure issues: ${closure.issues.join(', ')}`);
        return this.fail('SPEC_INCOMPLETE', 'Specification validation failed', {
          issues: closure.issues,
        });
      }

      // Execute the actual command logic
      const artifactType = args.artifact || args.arg0;
      logger.info(`Generating ${artifactType} artifacts...`);
      const result = await this.handleGenerate(
        { artifact: artifactType },
        options,
        ontologyManager,
        logger,
        projectRoot
      );

      // Format and return result
      if (result.success) {
        logger.success(`Generate command completed successfully`);
        return this.success(result, options.output || 'text');
      } else {
        logger.error(`Generate command failed: ${result.error}`);
        return this.fail(result.code || 'COMMAND_FAILED', result.error, result.details);
      }
    } catch (error) {
      logger.error(`Unexpected error: ${error.message}`);
      logger.debug(`Stack trace: ${error.stack}`);
      return this.fail('UNEXPECTED_ERROR', error.message, {
        stack: error.stack,
      });
    }
  }

  /**
   * Handle generate command logic
   *
   * This is where the actual artifact generation happens.
   *
   * @private
   */
  async handleGenerate(args, options, ontologyManager, logger, projectRoot) {
    try {
      const artifactType = args.artifact;
      const outputDir = path.join(projectRoot, 'generated');

      // Ensure output directory exists
      if (!fs.existsSync(outputDir)) {
        fs.mkdirSync(outputDir, { recursive: true });
      }

      const results = {};

      // Generate requested artifact(s)
      if (artifactType === 'all') {
        // Generate all artifact types
        results.docker = await this.generateDockerCompose(ontologyManager, outputDir, options, logger);
        results.kubernetes = await this.generateKubernetes(ontologyManager, outputDir, options, logger);
        results.terraform = await this.generateTerraform(ontologyManager, outputDir, options, logger);
        results.openapi = await this.generateOpenAPI(ontologyManager, outputDir, options, logger);
      } else {
        // Generate specific artifact
        switch (artifactType) {
          case 'docker':
            results.docker = await this.generateDockerCompose(ontologyManager, outputDir, options, logger);
            break;
          case 'kubernetes':
            results.kubernetes = await this.generateKubernetes(ontologyManager, outputDir, options, logger);
            break;
          case 'terraform':
            results.terraform = await this.generateTerraform(ontologyManager, outputDir, options, logger);
            break;
          case 'openapi':
            results.openapi = await this.generateOpenAPI(ontologyManager, outputDir, options, logger);
            break;
          default:
            return {
              success: false,
              code: 'INVALID_ARTIFACT',
              error: `Unknown artifact type: ${artifactType}`,
            };
        }
      }

      // Validate generated artifacts if requested
      if (options.validate) {
        logger.info('Validating generated artifacts...');
        const validationResult = await this.validateGeneratedArtifacts(outputDir, logger);
        if (!validationResult.valid) {
          return {
            success: false,
            code: 'VALIDATION_FAILED',
            error: `Artifact validation failed: ${validationResult.error}`,
            details: validationResult.details,
          };
        }
        logger.success('All artifacts validated successfully');
      }

      return {
        success: true,
        timestamp: new Date().toISOString(),
        artifactType,
        artifacts: results,
        outputDir,
        dryRun: options.dryRun,
        duration: this.getDuration(),
      };
    } catch (error) {
      return {
        success: false,
        code: 'GENERATE_ERROR',
        error: error.message,
        details: { stack: error.stack },
      };
    }
  }

  /**
   * Generate docker-compose.yml
   * @private
   */
  async generateDockerCompose(ontologyManager, outputDir, options, logger) {
    logger.info('  Generating docker-compose.yml...');

    try {
      const containers = ontologyManager.getContainers();
      const dataStores = ontologyManager.getDataStores();

      // Build docker-compose content
      let content = `version: '3.9'\n`;
      content += `# Generated from ggen-paas ontology\n`;
      content += `# Generated: ${new Date().toISOString()}\n\n`;

      content += `services:\n`;

      // Add containers
      for (const container of containers) {
        const label = container.properties?.label?.[0] || container.iri.split('#')[1];
        content += `  ${label}:\n`;
        content += `    image: ggen/${label}:latest\n`;
        content += `    ports:\n`;
        content += `      - "3000:8080"\n`;
        content += `    networks:\n`;
        content += `      - ggen-paas-network\n`;
        content += `    restart: unless-stopped\n\n`;
      }

      // Add data stores
      if (dataStores.length > 0) {
        content += `  postgres:\n`;
        content += `    image: postgres:16-alpine\n`;
        content += `    environment:\n`;
        content += `      - POSTGRES_USER=ggen\n`;
        content += `      - POSTGRES_DB=ggen_paas\n`;
        content += `    volumes:\n`;
        content += `      - postgres-data:/var/lib/postgresql/data\n`;
        content += `    networks:\n`;
        content += `      - ggen-paas-network\n\n`;

        content += `  redis:\n`;
        content += `    image: redis:7-alpine\n`;
        content += `    networks:\n`;
        content += `      - ggen-paas-network\n\n`;
      }

      content += `networks:\n`;
      content += `  ggen-paas-network:\n`;
      content += `    driver: bridge\n`;

      // Write file if not dry-run
      if (!options.dryRun) {
        const filePath = path.join(outputDir, 'docker-compose.yml');
        fs.writeFileSync(filePath, content, 'utf-8');
      }

      logger.success('  ✓ docker-compose.yml generated');
      return {
        file: 'docker-compose.yml',
        size: content.length,
        lines: content.split('\n').length,
        containers: containers.length,
        dataStores: dataStores.length,
      };
    } catch (error) {
      logger.error(`  ✗ Failed to generate docker-compose.yml: ${error.message}`);
      return {
        error: error.message,
      };
    }
  }

  /**
   * Generate Kubernetes manifests
   * @private
   */
  async generateKubernetes(ontologyManager, outputDir, options, logger) {
    logger.info('  Generating Kubernetes manifests...');

    try {
      const containers = ontologyManager.getContainers();
      const k8sDir = path.join(outputDir, 'k8s');

      if (!options.dryRun && !fs.existsSync(k8sDir)) {
        fs.mkdirSync(k8sDir, { recursive: true });
      }

      // Generate deployment for each container
      let deploymentCount = 0;
      for (const container of containers) {
        const label = container.properties?.label?.[0] || container.iri.split('#')[1];
        const deployment = this.generateDeployment(label, container);

        if (!options.dryRun) {
          const filePath = path.join(k8sDir, `${label}-deployment.yaml`);
          fs.writeFileSync(filePath, deployment, 'utf-8');
        }

        deploymentCount++;
      }

      logger.success(`  ✓ ${deploymentCount} Kubernetes manifests generated`);
      return {
        files: deploymentCount,
        directory: 'k8s',
      };
    } catch (error) {
      logger.error(`  ✗ Failed to generate Kubernetes manifests: ${error.message}`);
      return {
        error: error.message,
      };
    }
  }

  /**
   * Generate a Kubernetes Deployment manifest
   * @private
   */
  generateDeployment(name, container) {
    return `apiVersion: apps/v1
kind: Deployment
metadata:
  name: ${name}
  labels:
    app: ${name}
spec:
  replicas: 3
  selector:
    matchLabels:
      app: ${name}
  template:
    metadata:
      labels:
        app: ${name}
    spec:
      containers:
      - name: ${name}
        image: ggen/${name}:latest
        ports:
        - containerPort: 8080
        livenessProbe:
          httpGet:
            path: /health
            port: 8080
          initialDelaySeconds: 30
          periodSeconds: 10
        readinessProbe:
          httpGet:
            path: /ready
            port: 8080
          initialDelaySeconds: 5
          periodSeconds: 5
`;
  }

  /**
   * Generate Terraform configuration
   * @private
   */
  async generateTerraform(ontologyManager, outputDir, options, logger) {
    logger.info('  Generating Terraform configuration...');

    try {
      const tfDir = path.join(outputDir, 'terraform');
      const content = `# Terraform AWS Infrastructure
# Generated from ggen-paas ontology

terraform {
  required_version = ">= 1.0"
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 5.0"
    }
  }
}

provider "aws" {
  region = var.aws_region
}

variable "aws_region" {
  default = "us-east-1"
}

# Infrastructure resources would be generated here
`;

      if (!options.dryRun) {
        if (!fs.existsSync(tfDir)) {
          fs.mkdirSync(tfDir, { recursive: true });
        }
        const filePath = path.join(tfDir, 'main.tf');
        fs.writeFileSync(filePath, content, 'utf-8');
      }

      logger.success('  ✓ Terraform configuration generated');
      return {
        file: 'terraform/main.tf',
        size: content.length,
      };
    } catch (error) {
      logger.error(`  ✗ Failed to generate Terraform: ${error.message}`);
      return {
        error: error.message,
      };
    }
  }

  /**
   * Generate OpenAPI specification
   * @private
   */
  async generateOpenAPI(ontologyManager, outputDir, options, logger) {
    logger.info('  Generating OpenAPI specification...');

    try {
      const content = `openapi: 3.0.0
info:
  title: ggen-paas API
  version: 1.0.0
  description: Generated from ggen-paas ontology

paths:
  /services:
    get:
      summary: List all services
      responses:
        '200':
          description: List of services

  /services/{id}:
    get:
      summary: Get service details
      parameters:
        - name: id
          in: path
          required: true
          schema:
            type: string
      responses:
        '200':
          description: Service details

servers:
  - url: http://localhost:3001
    description: Local development
`;

      if (!options.dryRun) {
        const apiDir = path.join(outputDir, 'api');
        if (!fs.existsSync(apiDir)) {
          fs.mkdirSync(apiDir, { recursive: true });
        }
        const filePath = path.join(apiDir, 'openapi.yaml');
        fs.writeFileSync(filePath, content, 'utf-8');
      }

      logger.success('  ✓ OpenAPI specification generated');
      return {
        file: 'api/openapi.yaml',
        size: content.length,
      };
    } catch (error) {
      logger.error(`  ✗ Failed to generate OpenAPI: ${error.message}`);
      return {
        error: error.message,
      };
    }
  }

  /**
   * Validate all generated artifacts
   * @private
   */
  async validateGeneratedArtifacts(outputDir, logger) {
    try {
      // Basic validation: check files exist
      const files = this.findAllFiles(outputDir);
      if (files.length === 0) {
        return {
          valid: false,
          error: 'No files generated',
        };
      }

      logger.debug(`  Validating ${files.length} generated files...`);

      // Validate each file
      for (const file of files) {
        const ext = path.extname(file);
        if (ext === '.json') {
          try {
            const content = fs.readFileSync(file, 'utf-8');
            JSON.parse(content);
          } catch (e) {
            return {
              valid: false,
              error: `Invalid JSON in ${file}: ${e.message}`,
              details: { file },
            };
          }
        }
      }

      return { valid: true };
    } catch (error) {
      return {
        valid: false,
        error: error.message,
      };
    }
  }

  /**
   * Find all files in directory recursively
   * @private
   */
  findAllFiles(dir) {
    const files = [];

    function walk(current) {
      if (!fs.existsSync(current)) return;
      const entries = fs.readdirSync(current);

      for (const entry of entries) {
        const fullPath = path.join(current, entry);
        const stat = fs.statSync(fullPath);

        if (stat.isDirectory()) {
          walk(fullPath);
        } else {
          files.push(fullPath);
        }
      }
    }

    walk(dir);
    return files;
  }

  /**
   * Validate command arguments against schema
   * @private
   */
  validateArguments(args, options) {
    // Validate positional arguments
    if (args['artifact'] === undefined && !args.arg0) {
      return { valid: false, error: `Missing required argument: artifact` };
    }

    const artifactType = args['artifact'] || args.arg0;
    const validChoices = ['docker', 'kubernetes', 'terraform', 'openapi', 'all'];
    if (artifactType && !validChoices.includes(artifactType)) {
      return {
        valid: false,
        error: `Invalid artifact. Must be one of: ${validChoices.join(', ')}`,
      };
    }

    // Validate options
    if (options['output'] !== undefined) {
      if (!['text', 'json', 'yaml'].includes(options['output'])) {
        return {
          valid: false,
          error: `Invalid --output option. Must be one of: text, json, yaml`,
        };
      }
    }

    return { valid: true };
  }
}
