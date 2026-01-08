/**
 * Bree Job: Generate Docker Compose
 *
 * Executes ggen sync to generate docker-compose.yml from the ggen PaaS ontology.
 * This job is part of the semantic scheduler pipeline that uses RDF specifications
 * to drive infrastructure code generation.
 *
 * Generated from: bree-paas-generation.ttl
 * Job Name: GenerateDockerCompose
 */

const fs = require('fs');
const path = require('path');
const { spawn } = require('child_process');

module.exports = async function generate_docker_compose(job) {
  console.log('[generate-docker-compose] Starting ggen sync pipeline...');

  const startTime = Date.now();
  const projectRoot = path.resolve(__dirname, '../../..');

  try {
    // Step 1: Verify ontology exists
    const ontologyPath = path.join(projectRoot, '.specify/ggen-paas-ontology.ttl');
    if (!fs.existsSync(ontologyPath)) {
      throw new Error(`Ontology not found: ${ontologyPath}`);
    }
    console.log(`[generate-docker-compose] Ontology verified at ${ontologyPath}`);

    // Step 2: Verify ggen.toml exists
    const configPath = path.join(projectRoot, 'ggen-paas.toml');
    if (!fs.existsSync(configPath)) {
      throw new Error(`ggen configuration not found: ${configPath}`);
    }
    console.log(`[generate-docker-compose] Configuration verified at ${configPath}`);

    // Step 3: Execute ggen sync with specific rule
    console.log('[generate-docker-compose] Executing: ggen sync -c ggen-paas.toml --rule generate-docker-compose');

    const result = await executeGgenSync(projectRoot, {
      config: 'ggen-paas.toml',
      rule: 'generate-docker-compose'
    });

    if (result.code !== 0) {
      throw new Error(`ggen sync failed with exit code ${result.code}: ${result.stderr}`);
    }

    // Step 4: Verify generated output
    const outputPath = path.join(projectRoot, 'generated/docker-compose.yml');
    if (!fs.existsSync(outputPath)) {
      throw new Error(`Generated file not found: ${outputPath}`);
    }
    console.log(`[generate-docker-compose] Generated file verified at ${outputPath}`);

    // Step 5: Basic validation
    const content = fs.readFileSync(outputPath, 'utf-8');
    if (!content.includes('version:') || !content.includes('services:')) {
      throw new Error('Generated docker-compose.yml is invalid (missing version or services)');
    }
    console.log(`[generate-docker-compose] File structure validated (${content.length} bytes)`);

    const duration = Date.now() - startTime;
    console.log(`[generate-docker-compose] ✓ Completed in ${duration}ms`);

    return {
      success: true,
      duration,
      file: outputPath,
      size: fs.statSync(outputPath).size,
      timestamp: new Date().toISOString()
    };

  } catch (error) {
    const duration = Date.now() - startTime;
    console.error(`[generate-docker-compose] ✗ Failed after ${duration}ms: ${error.message}`);
    throw error;
  }
};

/**
 * Execute ggen sync command with specified options
 */
function executeGgenSync(cwd, options) {
  return new Promise((resolve, reject) => {
    const args = ['sync'];

    if (options.config) {
      args.push('-c', options.config);
    }
    if (options.rule) {
      args.push('--rule', options.rule);
    }

    const child = spawn('ggen', args, {
      cwd,
      stdio: ['inherit', 'pipe', 'pipe']
    });

    let stdout = '';
    let stderr = '';

    if (child.stdout) {
      child.stdout.on('data', (data) => {
        const message = data.toString().trim();
        stdout += message;
        console.log(`[ggen] ${message}`);
      });
    }

    if (child.stderr) {
      child.stderr.on('data', (data) => {
        const message = data.toString().trim();
        stderr += message;
        console.error(`[ggen-error] ${message}`);
      });
    }

    child.on('close', (code) => {
      resolve({
        code,
        stdout,
        stderr
      });
    });

    child.on('error', (error) => {
      reject(error);
    });
  });
}
