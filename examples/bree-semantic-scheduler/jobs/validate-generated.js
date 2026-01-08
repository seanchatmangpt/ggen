/**
 * Bree Job: Validate Generated Artifacts
 *
 * Validates the syntax and structure of all generated artifacts.
 * Runs as a dependent job after all generation jobs complete.
 *
 * Generated from: bree-paas-generation.ttl
 * Job Name: ValidateGeneratedArtifacts
 */

import fs from 'fs';
import path from 'path';
import { fileURLToPath } from 'url';
import { FileGenerator, JobLogger } from './job-utils.js';

const __dirname = path.dirname(fileURLToPath(import.meta.url));

export default async function validate_generated(job) {
  const logger = new JobLogger('validate-generated');
  const projectRoot = path.resolve(__dirname, '../../..');

  const results = {
    yaml: { valid: 0, invalid: 0, files: [] },
    hcl: { valid: 0, invalid: 0, files: [] },
    json: { valid: 0, invalid: 0, files: [] },
    markdown: { valid: 0, invalid: 0, files: [] },
  };

  try {
    logger.info('Starting artifact validation...');

    const generatedPath = path.join(projectRoot, 'generated');

    if (!fs.existsSync(generatedPath)) {
      throw new Error(`Generated directory not found: ${generatedPath}`);
    }

    const fileGen = new FileGenerator(generatedPath);

    // Step 1: Find all generated files
    logger.info('Scanning generated files...');
    const allFiles = findAllFiles(generatedPath);
    logger.info(`Found ${allFiles.length} files to validate`);

    // Step 2: Validate YAML files
    logger.info('Validating YAML files...');
    const yamlFiles = allFiles.filter((f) => f.endsWith('.yml') || f.endsWith('.yaml'));

    for (const file of yamlFiles) {
      const relativePath = path.relative(generatedPath, file);
      const result = validateYamlFile(file);

      if (result.valid) {
        results.yaml.valid++;
        logger.success(`✓ ${relativePath}`);
      } else {
        results.yaml.invalid++;
        logger.error(`✗ ${relativePath}: ${result.error}`);
        results.yaml.files.push({ file: relativePath, error: result.error });
      }
    }

    logger.info(`YAML: ${results.yaml.valid} valid, ${results.yaml.invalid} invalid`);

    // Step 3: Validate HCL files (Terraform)
    logger.info('Validating HCL (Terraform) files...');
    const hclFiles = allFiles.filter((f) => f.endsWith('.tf'));

    for (const file of hclFiles) {
      const relativePath = path.relative(generatedPath, file);
      const result = validateHclFile(file);

      if (result.valid) {
        results.hcl.valid++;
        logger.success(`✓ ${relativePath}`);
      } else {
        results.hcl.invalid++;
        logger.error(`✗ ${relativePath}: ${result.error}`);
        results.hcl.files.push({ file: relativePath, error: result.error });
      }
    }

    logger.info(`HCL: ${results.hcl.valid} valid, ${results.hcl.invalid} invalid`);

    // Step 4: Validate JSON files
    logger.info('Validating JSON files...');
    const jsonFiles = allFiles.filter((f) => f.endsWith('.json'));

    for (const file of jsonFiles) {
      const relativePath = path.relative(generatedPath, file);
      const result = validateJsonFile(file);

      if (result.valid) {
        results.json.valid++;
        logger.success(`✓ ${relativePath}`);
      } else {
        results.json.invalid++;
        logger.error(`✗ ${relativePath}: ${result.error}`);
        results.json.files.push({ file: relativePath, error: result.error });
      }
    }

    logger.info(`JSON: ${results.json.valid} valid, ${results.json.invalid} invalid`);

    // Step 5: Validate Markdown files
    logger.info('Validating Markdown files...');
    const mdFiles = allFiles.filter((f) => f.endsWith('.md'));

    for (const file of mdFiles) {
      const relativePath = path.relative(generatedPath, file);
      const result = validateMarkdownFile(file);

      if (result.valid) {
        results.markdown.valid++;
        logger.success(`✓ ${relativePath}`);
      } else {
        results.markdown.invalid++;
        logger.error(`✗ ${relativePath}: ${result.error}`);
        results.markdown.files.push({ file: relativePath, error: result.error });
      }
    }

    logger.info(`Markdown: ${results.markdown.valid} valid, ${results.markdown.invalid} invalid`);

    // Step 6: Generate validation report
    const totalValid = results.yaml.valid + results.hcl.valid + results.json.valid + results.markdown.valid;
    const totalInvalid = results.yaml.invalid + results.hcl.invalid + results.json.invalid + results.markdown.invalid;
    const totalFiles = yamlFiles.length + hclFiles.length + jsonFiles.length + mdFiles.length;

    logger.info(`\n=== Validation Summary ===`);
    logger.info(`Total: ${totalValid} valid, ${totalInvalid} invalid out of ${totalFiles} files`);

    if (totalInvalid > 0) {
      logger.error(`Validation FAILED: ${totalInvalid} file(s) with errors`);
      throw new Error(`Artifact validation failed: ${totalInvalid} errors detected`);
    }

    logger.success(`Validation PASSED: All ${totalValid} files validated successfully`);
    logger.success(`Artifact validation complete in ${logger.getDuration()}ms`);

    return {
      success: true,
      results,
      summary: {
        totalFiles,
        totalValid,
        totalInvalid,
        validationPassed: totalInvalid === 0,
      },
      duration: logger.getDuration(),
      logs: logger.getLogsSummary(),
    };
  } catch (error) {
    logger.error(`Failed: ${error.message}`);
    throw error;
  }
}

/**
 * Find all files in directory recursively
 */
function findAllFiles(dir) {
  const files = [];

  function walk(current) {
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
 * Validate YAML file
 */
function validateYamlFile(filePath) {
  try {
    const content = fs.readFileSync(filePath, 'utf-8');

    if (!content.trim()) {
      return { valid: false, error: 'Empty file' };
    }

    const lines = content.split('\n');

    for (const line of lines) {
      if (line.trim().startsWith('#')) continue;

      const match = line.match(/^( *)/);
      if (match) {
        const spaces = match[1].length;
        if (spaces % 2 !== 0) {
          return { valid: false, error: 'Invalid indentation (not multiple of 2)' };
        }
      }
    }

    return { valid: true };
  } catch (error) {
    return { valid: false, error: error.message };
  }
}

/**
 * Validate HCL file
 */
function validateHclFile(filePath) {
  try {
    const content = fs.readFileSync(filePath, 'utf-8');

    if (!content.trim()) {
      return { valid: false, error: 'Empty file' };
    }

    const openBraces = (content.match(/{/g) || []).length;
    const closeBraces = (content.match(/}/g) || []).length;

    if (openBraces !== closeBraces) {
      return { valid: false, error: `Unbalanced braces: ${openBraces} open, ${closeBraces} close` };
    }

    const doubleQuotes = (content.match(/"/g) || []).length;
    if (doubleQuotes % 2 !== 0) {
      return { valid: false, error: 'Unbalanced double quotes' };
    }

    return { valid: true };
  } catch (error) {
    return { valid: false, error: error.message };
  }
}

/**
 * Validate JSON file
 */
function validateJsonFile(filePath) {
  try {
    const content = fs.readFileSync(filePath, 'utf-8');

    if (!content.trim()) {
      return { valid: false, error: 'Empty file' };
    }

    JSON.parse(content);
    return { valid: true };
  } catch (error) {
    return { valid: false, error: `JSON parse error: ${error.message}` };
  }
}

/**
 * Validate Markdown file
 */
function validateMarkdownFile(filePath) {
  try {
    const content = fs.readFileSync(filePath, 'utf-8');

    if (!content.trim()) {
      return { valid: false, error: 'Empty file' };
    }

    if (!content.includes('#')) {
      return { valid: false, error: 'No headings found' };
    }

    const codeBlockCount = (content.match(/```/g) || []).length;
    if (codeBlockCount % 2 !== 0) {
      return { valid: false, error: 'Unbalanced code blocks' };
    }

    return { valid: true };
  } catch (error) {
    return { valid: false, error: error.message };
  }
}
