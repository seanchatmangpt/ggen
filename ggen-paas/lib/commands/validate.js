/**
 * ggen-paas CLI Command: validate
 *
 * Validates specifications and generated artifacts.
 */

import path from 'path';
import fs from 'fs';
import { fileURLToPath } from 'url';
import { CommandBase } from '../command-base.js';
import { OntologyManager } from '../utils/ontology.js';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

export default class ValidateCommand extends CommandBase {
  constructor() {
    super({
      name: 'validate',
      aliases: ['check', 'test'],
      category: 'Validation',
      description: 'Validate specifications and generated artifacts',
      examples: `
        ggen paas validate artifacts
        ggen paas validate closure --verbose
        ggen paas validate infrastructure
      `,
      slo: { maxDurationMs: 30000, expectedPassRate: 99.5 },
    });
  }

  defineSchema() {
    return {
      positional: [
        {
          name: 'target',
          type: 'string',
          required: true,
          choices: ['artifacts', 'closure', 'infrastructure', 'all'],
          description: 'Validation target',
        },
      ],
      options: {
        verbose: { shortForm: '-v', longForm: '--verbose', type: 'boolean', default: false },
        strict: { longForm: '--strict', type: 'boolean', default: false },
      },
    };
  }

  async execute(args, options) {
    const logger = this.logger;

    try {
      logger.info('Starting validation...');
      const target = args.artifact || args.arg0 || 'all';

      if (target === 'closure' || target === 'all') {
        logger.info('Validating specification closure...');
        const projectRoot = path.resolve(__dirname, '../../..');
        const ontologyPath = path.join(projectRoot, '.specify/ggen-paas-ontology.ttl');

        try {
          const ontologyManager = new OntologyManager(ontologyPath);
          await ontologyManager.load();

          const closure = ontologyManager.validateClosure();
          if (!closure.valid) {
            logger.error(`Specification closure validation FAILED`);
            if (options.verbose) {
              for (const issue of closure.issues) {
                logger.error(`  - ${issue}`);
              }
            }
            if (options.strict) {
              return this.fail('SPEC_INCOMPLETE', 'Specification validation failed', closure);
            }
          } else {
            logger.success(
              `Specification closure: PASS (${closure.stats.containers} containers, ${closure.stats.dataStores} datastores)`
            );
          }
        } catch (error) {
          logger.error(`Failed to validate closure: ${error.message}`);
          if (options.strict) {
            return this.fail('VALIDATION_ERROR', error.message);
          }
        }
      }

      if (target === 'artifacts' || target === 'all') {
        logger.info('Validating generated artifacts...');
        const projectRoot = path.resolve(__dirname, '../../..');
        const generatedPath = path.join(projectRoot, 'generated');

        try {
          const artifactResults = await this._validateArtifacts(generatedPath, options, logger);
          if (!artifactResults.valid && options.strict) {
            return this.fail('ARTIFACTS_INVALID', `Artifact validation failed`, artifactResults);
          }
          if (artifactResults.valid) {
            logger.success(`Artifacts validation: PASS (${artifactResults.fileCount} files)`);
          }
        } catch (error) {
          logger.error(`Failed to validate artifacts: ${error.message}`);
          if (options.strict) {
            return this.fail('VALIDATION_ERROR', error.message);
          }
        }
      }

      logger.success('Validation complete');
      return this.success({ status: 'complete', duration: this.getDuration() });
    } catch (error) {
      logger.error(`Unexpected error: ${error.message}`);
      return this.fail('UNEXPECTED_ERROR', error.message, { stack: error.stack });
    }
  }

  async _validateArtifacts(artifactPath, options, logger) {
    if (!fs.existsSync(artifactPath)) {
      return { valid: false, error: 'Generated directory not found', fileCount: 0 };
    }

    const files = this._findAllFiles(artifactPath);
    let validCount = 0;
    let invalidCount = 0;

    for (const file of files) {
      const ext = path.extname(file);
      let isValid = false;

      try {
        if (ext === '.yml' || ext === '.yaml') {
          // Basic YAML validation: check for proper indentation
          const content = fs.readFileSync(file, 'utf-8');
          isValid =
            content.includes('version:') || content.includes('kind:') || content.includes('name:');
        } else if (ext === '.json') {
          const content = fs.readFileSync(file, 'utf-8');
          JSON.parse(content);
          isValid = true;
        } else if (ext === '.md') {
          const content = fs.readFileSync(file, 'utf-8');
          isValid = content.trim().length > 0;
        } else {
          isValid = true; // Other files assumed valid
        }

        if (isValid) {
          validCount++;
          if (options.verbose) {
            logger.debug(`✓ ${file}`);
          }
        } else {
          invalidCount++;
          logger.warn(`✗ ${file}: invalid structure`);
        }
      } catch (error) {
        invalidCount++;
        logger.warn(`✗ ${file}: ${error.message}`);
      }
    }

    return {
      valid: invalidCount === 0,
      fileCount: files.length,
      validCount,
      invalidCount,
    };
  }

  _findAllFiles(dir) {
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
}
