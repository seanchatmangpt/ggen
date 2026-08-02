import { spawnSync } from 'node:child_process';
import { mkdir, mkdtemp, readFile, rm, writeFile } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { dirname, resolve } from 'node:path';

const root = resolve(process.cwd());
const control = resolve(root, 'scripts/control-plane.mjs');
const model = JSON.parse(await readFile(resolve(root, 'src/vision2030.json'), 'utf8'));
const checks = [];

function assertContract(id, condition, detail) {
  checks.push({ id, state: condition ? 'PASS' : 'FAIL', detail });
  if (!condition) throw new Error(`${id}:${detail}`);
}

function runControl(args, cwd = root) {
  const result = spawnSync(process.execPath, [control, ...args], {
    cwd,
    encoding: 'utf8',
    env: { ...process.env, NO_COLOR: '1' }
  });
  return { status: result.status, stdout: result.stdout, stderr: result.stderr };
}

async function readJson(path, cwd = root) {
  return JSON.parse(await readFile(resolve(cwd, path), 'utf8'));
}

function dominates(left, right) {
  const noWorse = left.latencyScore <= right.latencyScore
    && left.costScore <= right.costScore
    && left.privacyScore >= right.privacyScore
    && left.accessibilityScore >= right.accessibilityScore
    && left.preferenceScore >= right.preferenceScore;
  const strict = left.latencyScore < right.latencyScore
    || left.costScore < right.costScore
    || left.privacyScore > right.privacyScore
    || left.accessibilityScore > right.accessibilityScore
    || left.preferenceScore > right.preferenceScore;
  return noWorse && strict;
}

let result = runControl(['orient']);
assertContract('orient:exit', result.status === 0, `exit=${result.status};stderr=${result.stderr}`);
const orientation = await readJson('.ggen/evidence/orientation.json');
assertContract('orient:no-declared-unwired', orientation.declaredButUnwired.length === 0, JSON.stringify(orientation.declaredButUnwired));
for (const command of ['orient', 'doctor', 'wizard', 'search', 'explain', 'demo', 'evidence', 'release', 'frontier']) {
  assertContract(`orient:command:${command}`, Object.values(orientation.commandCoverage).includes(command), JSON.stringify(orientation.commandCoverage));
}

result = runControl(['search', 'receipt', 'replay']);
assertContract('search:exit', result.status === 0, `exit=${result.status}`);
const search = await readJson('.ggen/evidence/capability-search.json');
assertContract('search:grounded-results', search.resultCount > 0 && search.results.every(item => item.id && item.verifier), JSON.stringify(search.results));

result = runControl(['explain', 'one-command']);
assertContract('explain:exit', result.status === 0, `exit=${result.status}`);
const explanation = await readJson('.ggen/evidence/explain-one-command.json');
assertContract('explain:implementation', explanation.implementedCommand === 'demo', JSON.stringify(explanation));
assertContract('explain:authority', explanation.sourceAuthority === 'ontology/vision2030.ttl', explanation.sourceAuthority);

result = runControl(['wizard', '--step-2=ambient-root']);
assertContract('wizard:invalid-exit', result.status === 65, `exit=${result.status}`);
assertContract('wizard:invalid-refusal', result.stderr.includes('WIZARD_CHOICE_UNREPRESENTABLE'), result.stderr);

result = runControl(['wizard', '--step-1=watch-party', '--step-2=plan-only', '--step-3=exhibition', '--step-4=federated', '--step-5=PARTIAL_ALIVE']);
assertContract('wizard:legacy-exit', result.status === 0, `exit=${result.status};stderr=${result.stderr}`);
const wizard = await readJson('.ggen/plans/wizard-profile.json');
assertContract('wizard:legacy-normalized', wizard.authority === 'local-owner' && wizard.evidence === 'local-alive', JSON.stringify(wizard));
assertContract('wizard:legacy-warnings', wizard.warnings.length === 2, JSON.stringify(wizard.warnings));
assertContract('wizard:digest', wizard.digest?.algorithm === 'sha256' && wizard.digest.value.length === 64, JSON.stringify(wizard.digest));

result = runControl(['frontier', '--offline']);
assertContract('frontier:exit', result.status === 0, `exit=${result.status}`);
const frontier = await readJson('.ggen/plans/pareto-frontier.json');
assertContract('frontier:nonempty', frontier.frontier.length > 0, JSON.stringify(frontier));
for (const candidate of frontier.frontier) {
  assertContract(`frontier:${candidate.id}:nondominated`, !frontier.candidates.some(other => other.id !== candidate.id && dominates(other, candidate)), JSON.stringify(candidate));
}
assertContract('frontier:offline-preference', frontier.selected.offline === true, JSON.stringify(frontier.selected));

result = runControl(['demo']);
assertContract('demo:exit', result.status === 0, `exit=${result.status};stderr=${result.stderr}`);
const demo = await readJson('.ggen/evidence/demo.json');
assertContract('demo:preview-admitted', demo.releaseAdmission === true && demo.claimCeiling === 'PREVIEW', JSON.stringify(demo));
assertContract('demo:complete-loop', demo.steps.map(step => step.name).join(',') === 'orient,doctor,wizard,plan,frontier,evidence,release-preview', JSON.stringify(demo.steps));

const evidence = await readJson('.ggen/evidence/index.json');
for (const alias of ['doctor', 'wizard', 'plan']) {
  assertContract(`evidence:${alias}`, evidence.aliases.includes(alias), JSON.stringify(evidence.aliases));
}
assertContract('evidence:preview-ceiling', evidence.claimCeiling === 'PREVIEW', evidence.claimCeiling);

result = runControl(['release', '--target=device-alive']);
assertContract('release:device-blocked-exit', result.status === 2, `exit=${result.status}`);
const deviceRelease = await readJson('.ggen/evidence/release-device-alive.json');
assertContract('release:device-fail-closed', deviceRelease.standing === 'BLOCKED' && deviceRelease.missingEvidence.includes('device'), JSON.stringify(deviceRelease));
assertContract('release:no-overclaim', deviceRelease.releaseAdmission === false, JSON.stringify(deviceRelease));

result = runControl(['release', '--target=imaginary']);
assertContract('release:unknown-exit', result.status === 64, `exit=${result.status}`);
assertContract('release:unknown-refusal', result.stderr.includes('RELEASE_TARGET_UNREPRESENTABLE'), result.stderr);

const sandbox = await mkdtemp(resolve(tmpdir(), 'cyberpunk-tv-innovation-'));
try {
  const doctorRecords = model.records.filter(record => record.kind === 'doctor');
  await mkdir(resolve(sandbox, 'src'), { recursive: true });
  await writeFile(resolve(sandbox, 'src/vision2030.json'), JSON.stringify(model, null, 2));
  for (const record of doctorRecords) {
    for (const path of String(record.surfaces ?? '').split(',').map(value => value.trim()).filter(Boolean)) {
      const target = resolve(sandbox, path);
      await mkdir(dirname(target), { recursive: true });
      try {
        await readFile(target);
      } catch {
        await writeFile(target, '{}\n');
      }
    }
  }
  await rm(resolve(sandbox, 'src/rights.json'));
  result = runControl(['doctor'], sandbox);
  assertContract('doctor:granular-negative-exit', result.status === 2, `exit=${result.status}`);
  const blocked = await readJson('.ggen/evidence/doctor.json', sandbox);
  assertContract('doctor:granular-localization', blocked.blockedChecks.length === 1 && blocked.blockedChecks[0] === 'generation', JSON.stringify(blocked));
  assertContract('doctor:unrelated-checks-ready', blocked.checks.filter(check => check.id !== 'generation').every(check => check.state === 'READY'), JSON.stringify(blocked.checks));
  result = runControl(['evidence'], sandbox);
  assertContract('evidence:blocked-doctor-not-admitted', result.status === 0, `exit=${result.status}`);
  const blockedIndex = await readJson('.ggen/evidence/index.json', sandbox);
  assertContract('evidence:blocked-doctor-no-alias', !blockedIndex.aliases.includes('doctor'), JSON.stringify(blockedIndex));

  await writeFile(resolve(sandbox, 'src/rights.json'), '{}\n');
  result = runControl(['doctor'], sandbox);
  assertContract('doctor:repaired-exit', result.status === 0, `exit=${result.status}`);
  result = runControl(['wizard'], sandbox);
  assertContract('release-fixture:wizard', result.status === 0, `exit=${result.status}`);
  result = runControl(['plan'], sandbox);
  assertContract('release-fixture:plan', result.status === 0, `exit=${result.status}`);
  await mkdir(resolve(sandbox, '.ggen/receipts'), { recursive: true });
  await writeFile(resolve(sandbox, '.ggen/receipts/cyberpunk-tv.json'), JSON.stringify({ algorithm: 'blake3-256', root: 'fixture-root', artifacts: [] }) + '\n');
  await writeFile(resolve(sandbox, '.ggen/evidence/replay.json'), JSON.stringify({ schema: 'ggen.cyberpunk-tv.replay-evidence.v1', standing: 'PARTIAL_ALIVE', replay: 'REPLAY_MATCH' }) + '\n');
  await writeFile(resolve(sandbox, '.ggen/evidence/browser-e2e.json'), JSON.stringify({ schema: 'ggen.cyberpunk-tv.browser-evidence.v1', standing: 'PARTIAL_ALIVE', browser: 'EXECUTED' }) + '\n');
  result = runControl(['release', '--target=local-alive'], sandbox);
  assertContract('release:local-alive-admitted', result.status === 0, `exit=${result.status};stderr=${result.stderr}`);
  const localRelease = await readJson('.ggen/evidence/release-local-alive.json', sandbox);
  assertContract('release:local-alive-complete', localRelease.releaseAdmission === true && localRelease.missingEvidence.length === 0, JSON.stringify(localRelease));
  result = runControl(['release', '--target=device-alive'], sandbox);
  assertContract('release:device-only-blocker', result.status === 2, `exit=${result.status}`);
  const sandboxDeviceRelease = await readJson('.ggen/evidence/release-device-alive.json', sandbox);
  assertContract('release:device-exact-missing', JSON.stringify(sandboxDeviceRelease.missingEvidence) === JSON.stringify(['device']), JSON.stringify(sandboxDeviceRelease));

  const mutated = structuredClone(model);
  mutated.records.push({
    kind: 'capability', id: 'ghost-innovation', title: 'Ghost innovation', group: 'dx', order: 999,
    requires: '', provides: 'unwired claim', verifier: 'must-fail', repair: '', cost: 'zero', reversible: true, enabled: true, command: 'ghost'
  });
  await writeFile(resolve(sandbox, 'src/vision2030.json'), JSON.stringify(mutated, null, 2));
  result = runControl(['orient'], sandbox);
  assertContract('mutation:unwired-capability-exit', result.status === 2, `exit=${result.status}`);
  const mutatedOrientation = await readJson('.ggen/evidence/orientation.json', sandbox);
  assertContract('mutation:unwired-capability-killed', mutatedOrientation.declaredButUnwired.includes('ghost-innovation'), JSON.stringify(mutatedOrientation));
} finally {
  await rm(sandbox, { recursive: true, force: true });
}

const failed = checks.filter(check => check.state !== 'PASS');
const report = {
  schema: 'ggen.cyberpunk-tv.innovation-80-20.v1',
  method: 'close declared-but-unwired product loops before adding more surface area',
  gapClosures: [
    'one-screen orientation',
    'grounded capability search',
    'source-to-output explanation',
    'strict wizard admission',
    'granular doctor localization',
    'ontology-scored Pareto frontier',
    'evidence navigation index',
    'fail-closed release claims',
    'one-command admitted demo'
  ],
  assertions: checks.length,
  failed: failed.length,
  mutationControlsKilled: 2,
  standing: failed.length === 0 ? 'PARTIAL_ALIVE' : 'BUILD_BROKEN',
  checks
};
await mkdir(resolve(root, '.ggen/evidence'), { recursive: true });
await writeFile(resolve(root, '.ggen/evidence/innovation-80-20.json'), JSON.stringify(report, null, 2) + '\n');
console.log(JSON.stringify({ standing: report.standing, assertions: report.assertions, mutationsKilled: report.mutationControlsKilled }));
if (failed.length) process.exitCode = 1;
