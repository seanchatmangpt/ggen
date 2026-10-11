import { createHash } from 'node:crypto';
import { mkdir, readFile, writeFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { fileURLToPath } from 'node:url';

const here = resolve(fileURLToPath(new URL('.', import.meta.url)));
const pack = resolve(here, '..');
const evidence = resolve(pack, '.ggen/evidence/no-ci');
const readJson = async name => JSON.parse(await readFile(resolve(evidence, name), 'utf8'));
const sha256 = value => createHash('sha256').update(value).digest('hex');
const canonical = value => JSON.stringify(value, Object.keys(value).sort());

const source = JSON.parse(await readFile(resolve(pack, 'generated/.ggen/evidence/nasa-dark-mode.json'), 'utf8'));
const manufacture = JSON.parse(await readFile(resolve(pack, '.ggen/evidence/manufacture.json'), 'utf8'));
const wasm = await readJson('wasm-equivalence.json');
const roku = await readJson('roku-source-simulation.json');
const browser = await readJson('browser-webgl2.json');
const dependencies = await readJson('dependency-probes.json');

const checks = {
  sourceCapsule: source.failed === 0 && source.standings.sourceCapsule === 'ALIVE',
  manufacture: manufacture.standing === 'ALIVE' && manufacture.replay === 'REPLAY_MATCH',
  wasmEquivalence: wasm.standing === 'ALIVE' && wasm.mismatches === 0 && wasm.mutationControl.standing === 'KILLED',
  browserWebGL2: browser.standing === 'ALIVE' && browser.webgl2 === true && browser.shaderMutationControl === 'KILLED',
  rokuSourceSimulation: roku.standing === 'ALIVE' && roku.mutationControl.standing === 'KILLED',
  package: source.standings.rokuPackage === 'ALIVE',
  dependencyRefusalsTyped: dependencies.deckGl.standing === 'BLOCKED_DEPENDENCY_TRANSPORT' && dependencies.rust.standing === 'BLOCKED_TOOLCHAIN_REQUIRED'
};
if (Object.values(checks).some(value => !value)) throw new Error(`NO_CI_AGGREGATE_BROKEN:${JSON.stringify(checks)}`);

const subject = {
  schema: 'ggen.nasa-dark-mode.no-ci-receipt.v1',
  sourceManufactureRoot: manufacture.generatedRoot,
  missionFeedDigest: source.missionFeedReceipt.digest,
  wasmSha256: wasm.wasm.sha256,
  browserScreenshotSha256: browser.screenshot.sha256,
  rokuPackageSha256: roku.packageSha256,
  exhaustiveWasmProfiles: wasm.exhaustiveProfiles,
  sourceAssertions: source.assertions,
  localBoundaries: {
    sourceCapsule: 'ALIVE',
    deterministicManufacture: 'ALIVE',
    wasmControlCore: 'ALIVE',
    browserDom: 'ALIVE',
    browserWebGL2: 'ALIVE',
    rokuSourceSimulation: 'ALIVE',
    rokuPackage: 'ALIVE',
    receiptReplay: 'ALIVE',
    deckGlRuntime: 'BLOCKED_DEPENDENCY_TRANSPORT',
    rustImplementation: 'BLOCKED_TOOLCHAIN_REQUIRED',
    rokuPhysicalDevice: 'BLOCKED_DEVICE_REQUIRED'
  },
  aggregate: 'PARTIAL_ALIVE',
  releaseAdmission: false
};
const report = { ...subject, receipt: { algorithm: 'sha256', digest: sha256(canonical(subject)) }, checks };
await mkdir(evidence, { recursive: true });
await writeFile(resolve(evidence, 'no-ci-receipt.json'), JSON.stringify(report, null, 2) + '\n');
console.log(JSON.stringify(report));
