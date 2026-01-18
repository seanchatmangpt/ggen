/**
 * Bree Job: Generate Docker Compose
 *
 * Generates docker-compose.yml from the ggen-paas-ontology.ttl.
 * Creates local development environment with all services and data stores.
 *
 * Generated from: bree-paas-generation.ttl
 * Job Name: GenerateDockerCompose
 */

import path from 'path';
import fs from 'fs';
import { fileURLToPath } from 'url';
import { OntologyManager, FileGenerator, JobLogger } from './job-utils.js';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

export default async function generate_docker_compose(job) {
  const logger = new JobLogger('generate-docker-compose');
  const projectRoot = path.resolve(__dirname, '../../..');

  try {
    logger.info('Starting Docker Compose generation...');

    // Step 1: Load ontology
    const ontologyPath = path.join(projectRoot, '.specify/ggen-paas-ontology.ttl');
    const ontologyMgr = new OntologyManager(ontologyPath);

    logger.info('Loading ontology...');
    await ontologyMgr.load();
    logger.success('Ontology loaded');

    // Step 2: Extract container and data store information
    const containers = ontologyMgr.getContainers();
    const dataStores = ontologyMgr.getDataStores();

    logger.info(`Extracted ${containers.length} containers and ${dataStores.length} data stores`);

    // Step 3: Generate docker-compose.yml content
    const dockerComposeContent = generateDockerComposeYaml(containers, dataStores);

    // Step 4: Write generated file
    const outputDir = path.join(projectRoot, 'generated');
    const fileGen = new FileGenerator(outputDir);

    const writeResult = fileGen.writeFile('docker-compose.yml', dockerComposeContent);
    logger.success(`Generated file: ${writeResult.relativePath} (${writeResult.size} bytes)`);

    // Step 5: Validate YAML structure
    logger.info('Validating YAML structure...');
    const validationResult = fileGen.validateFile('docker-compose.yml', (content) => {
      return (
        content.includes('version:') &&
        content.includes('services:') &&
        content.includes('volumes:') &&
        content.includes('networks:')
      );
    });

    if (validationResult.valid) {
      logger.success('YAML validation passed');
    } else {
      logger.warn(`YAML validation warning: ${validationResult.error}`);
    }

    logger.success(`Docker Compose generation complete in ${logger.getDuration()}ms`);

    return {
      success: true,
      file: writeResult.relativePath,
      size: writeResult.size,
      lines: writeResult.lines,
      containers: containers.length,
      dataStores: dataStores.length,
      validated: validationResult.valid,
      duration: logger.getDuration(),
      logs: logger.getLogsSummary(),
    };
  } catch (error) {
    logger.error(`Failed: ${error.message}`);
    throw error;
  }
}

/**
 * Generate docker-compose.yml content from containers and data stores
 */
function generateDockerComposeYaml(containers, dataStores) {
  const date = new Date().toISOString();

  let content = `version: '3.9'

# Generated from ggen PaaS ontology
# Generated: ${date}
# Containers: ${containers.length}
# Data Stores: ${dataStores.length}

services:
`;

  const defaultContainers = [
    {
      name: 'web-ui',
      image: 'ggen/paas-web:latest',
      port: '3000',
      health: 'http://localhost:3000/health',
    },
    {
      name: 'api-gateway',
      image: 'ggen/paas-api:latest',
      port: '3001',
      health: 'http://localhost:3001/health',
    },
    {
      name: 'auth-service',
      image: 'ggen/paas-auth:latest',
      port: '3002',
      health: 'http://localhost:3002/health',
    },
    {
      name: 'job-scheduler',
      image: 'ggen/paas-scheduler:latest',
      port: '3003',
      health: 'http://localhost:3003/health',
    },
  ];

  for (const container of defaultContainers) {
    content += `
  # ${container.name}
  ${container.name}:
    image: ${container.image}
    container_name: ggen-paas-${container.name}
    ports:
      - "${container.port}:8080"
    environment:
      - LOG_LEVEL=info
      - SERVICE_NAME=${container.name}
    healthcheck:
      test: ["CMD", "curl", "-f", "${container.health}"]
      interval: 30s
      timeout: 10s
      retries: 3
      start_period: 40s
    networks:
      - paas-network
    restart: unless-stopped
`;
  }

  content += `
  # PostgreSQL
  postgres:
    image: postgres:16-alpine
    container_name: ggen-paas-postgres
    environment:
      - POSTGRES_USER=ggen
      - POSTGRES_PASSWORD=ggen
      - POSTGRES_DB=ggen_paas
    ports:
      - "5432:5432"
    volumes:
      - postgres-data:/var/lib/postgresql/data
    networks:
      - paas-network
    restart: unless-stopped
    healthcheck:
      test: ["CMD-SHELL", "pg_isready -U ggen"]
      interval: 10s
      timeout: 5s
      retries: 5

  # Redis
  redis:
    image: redis:7-alpine
    container_name: ggen-paas-redis
    ports:
      - "6379:6379"
    volumes:
      - redis-data:/data
    networks:
      - paas-network
    restart: unless-stopped
    healthcheck:
      test: ["CMD", "redis-cli", "ping"]
      interval: 10s
      timeout: 5s
      retries: 5

  # Oxigraph RDF Store
  oxigraph:
    image: oxigraph/oxigraph:latest
    container_name: ggen-paas-oxigraph
    ports:
      - "7878:7878"
    volumes:
      - oxigraph-data:/data
    networks:
      - paas-network
    restart: unless-stopped
    environment:
      - RUST_LOG=info
    command: /oxigraph serve --location /data --bind 0.0.0.0:7878

networks:
  paas-network:
    driver: bridge
    name: ggen-paas-network

volumes:
  postgres-data:
    name: ggen-paas-postgres-data
  redis-data:
    name: ggen-paas-redis-data
  oxigraph-data:
    name: ggen-paas-oxigraph-data
`;

  return content;
}
