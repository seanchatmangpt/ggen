/**
 * Bree Job: Validate Generated Artifacts
 *
 * Validates the syntax and structure of all generated artifacts.
 * Runs as a dependent job after all generation jobs complete.
 * Uses cargo make validate targets for comprehensive validation.
 *
 * Generated from: bree-paas-generation.ttl
 * Job Name: ValidateGeneratedArtifacts
 */

const fs = require('fs');
const path = require('path');
const { spawn } = require('child_process');

module.exports = async function validate_generated(job) {
  console.log('[validate-generated] Starting artifact validation...');

  const startTime = Date.now();
  const projectRoot = path.resolve(__dirname, '../../..');
  const generatedPath = path.join(projectRoot, 'generated');

  const results = {
    yaml: { valid: 0, invalid: 0, errors: [] },
    hcl: { valid: 0, invalid: 0, errors: [] },
    json: { valid: 0, invalid: 0, errors: [] }
  };

  try {
    // Step 1: Verify generated directory exists
    if (!fs.existsSync(generatedPath)) {
      throw new Error(`Generated directory not found: ${generatedPath}`);
    }
    console.log(`[validate-generated] Found generated directory`);

    // Step 2: Validate YAML files
    console.log('[validate-generated] Validating YAML files...');
    const yamlFiles = findFiles(generatedPath, '.yml', '.yaml');
    for (const file of yamlFiles) {
      const result = await validateYaml(file);
      if (result.valid) {
        results.yaml.valid++;
      } else {
        results.yaml.invalid++;
        results.yaml.errors.push({ file, error: result.error });
      }
    }
    console.log(`[validate-generated] YAML: ${results.yaml.valid} valid, ${results.yaml.invalid} invalid`);

    // Step 3: Validate HCL files (Terraform)
    console.log('[validate-generated] Validating Terraform HCL files...');
    const hclFiles = findFiles(generatedPath, '.tf');
    for (const file of hclFiles) {
      const result = await validateHcl(file);
      if (result.valid) {
        results.hcl.valid++;
      } else {
        results.hcl.invalid++;
        results.hcl.errors.push({ file, error: result.error });
      }
    }
    console.log(`[validate-generated] HCL: ${results.hcl.valid} valid, ${results.hcl.invalid} invalid`);

    // Step 4: Validate JSON files
    console.log('[validate-generated] Validating JSON files...');
    const jsonFiles = findFiles(generatedPath, '.json');
    for (const file of jsonFiles) {
      const result = validateJson(file);
      if (result.valid) {
        results.json.valid++;
      } else {
        results.json.invalid++;
        results.json.errors.push({ file, error: result.error });
      }
    }
    console.log(`[validate-generated] JSON: ${results.json.valid} valid, ${results.json.invalid} invalid`);

    // Step 5: Run cargo make validation
    console.log('[validate-generated] Running cargo make validation...');
    await executeMakeTarget(projectRoot, 'validate-generated');
    console.log('[validate-generated] Cargo make validation passed');

    // Step 6: Generate report
    const totalFiles = yamlFiles.length + hclFiles.length + jsonFiles.length;
    const totalValid = results.yaml.valid + results.hcl.valid + results.json.valid;
    const totalInvalid = results.yaml.invalid + results.hcl.invalid + results.json.invalid;

    console.log(`[validate-generated] Summary: ${totalValid}/${totalFiles} files valid`);

    if (totalInvalid > 0) {
      console.error('[validate-generated] ✗ Validation failed:');
      console.error(JSON.stringify(results, null, 2));
      throw new Error(`${totalInvalid} file(s) failed validation`);
    }

    const duration = Date.now() - startTime;
    console.log(`[validate-generated] ✓ Validation complete in ${duration}ms`);

    return {
      success: true,
      duration,
      results,
      totalFiles,
      totalValid,
      timestamp: new Date().toISOString()
    };

  } catch (error) {
    const duration = Date.now() - startTime;
    console.error(`[validate-generated] ✗ Failed after ${duration}ms: ${error.message}`);
    throw error;
  }
};

/**
 * Find files with specific extensions
 */
function findFiles(dir, ...extensions) {
  const files = [];

  function walk(current) {
    const entries = fs.readdirSync(current);
    for (const entry of entries) {
      const fullPath = path.join(current, entry);
      const stat = fs.statSync(fullPath);

      if (stat.isDirectory()) {
        walk(fullPath);
      } else if (extensions.some(ext => entry.endsWith(ext))) {
        files.push(fullPath);
      }
    }
  }

  walk(dir);
  return files;
}

/**
 * Validate YAML syntax
 */
async function validateYaml(filePath) {
  try {
    const yaml = require('js-yaml');
    const content = fs.readFileSync(filePath, 'utf-8');
    yaml.load(content);
    return { valid: true };
  } catch (error) {
    return { valid: false, error: error.message };
  }
}

/**
 * Validate HCL syntax (Terraform)
 */
async function validateHcl(filePath) {
  try {
    // Simple validation: check for basic HCL structure
    const content = fs.readFileSync(filePath, 'utf-8');

    if (!/^(resource|variable|output|module|provider|terraform)\s+/.test(content)) {
      return { valid: false, error: 'Invalid HCL: missing resource/variable/output declarations' };
    }

    // Check for balanced braces
    const openBraces = (content.match(/{/g) || []).length;
    const closeBraces = (content.match(/}/g) || []).length;

    if (openBraces !== closeBraces) {
      return { valid: false, error: `Unbalanced braces: ${openBraces} open, ${closeBraces} close` };
    }

    return { valid: true };
  } catch (error) {
    return { valid: false, error: error.message };
  }
}

/**
 * Validate JSON syntax
 */
function validateJson(filePath) {
  try {
    const content = fs.readFileSync(filePath, 'utf-8');
    JSON.parse(content);
    return { valid: true };
  } catch (error) {
    return { valid: false, error: error.message };
  }
}

/**
 * Execute cargo make target
 */
function executeMakeTarget(cwd, target) {
  return new Promise((resolve, reject) => {
    const child = spawn('cargo', ['make', target], {
      cwd,
      stdio: 'inherit'
    });

    child.on('close', (code) => {
      if (code === 0) {
        resolve();
      } else {
        reject(new Error(`cargo make ${target} failed with exit code ${code}`));
      }
    });

    child.on('error', reject);
  });
}
