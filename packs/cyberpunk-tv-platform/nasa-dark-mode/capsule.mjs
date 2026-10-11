import { execFileSync } from 'node:child_process';
import { createHash } from 'node:crypto';
import { mkdir, readFile, rm, writeFile } from 'node:fs/promises';
import { dirname, resolve } from 'node:path';
import { gunzipSync } from 'node:zlib';

const root = resolve(new URL('.', import.meta.url).pathname);
const generated = resolve(root, 'generated');
const payloadParts = ['payload/00.b64', 'payload/01.b64', 'payload/02.b64', 'payload/03.b64'];
const payload = (await Promise.all(payloadParts.map(path => readFile(resolve(root, path), 'utf8')))).map(part => part.trim()).join('');
const runtimeFiles = JSON.parse(gunzipSync(Buffer.from(payload, 'base64')).toString('utf8'));
const authorityPaths = ['README.md', 'ontology/nasa-dark-mode.ttl', 'contract/nasa-dark-mode.json', 'fixtures/eonet-events.json'];

function digest(value) { return createHash('sha256').update(value).digest('hex'); }
async function materialize() {
  await rm(generated, { recursive: true, force: true });
  const files = { ...runtimeFiles };
  for (const path of authorityPaths) files[path] = await readFile(resolve(root, path), 'utf8');
  for (const [path, content] of Object.entries(files).sort(([a], [b]) => a.localeCompare(b))) {
    const target = resolve(generated, path);
    await mkdir(dirname(target), { recursive: true });
    await writeFile(target, content);
  }
  const artifacts = Object.fromEntries(Object.entries(files).sort(([a], [b]) => a.localeCompare(b)).map(([path, content]) => [path, digest(content)]));
  return { artifacts, root: digest(JSON.stringify(artifacts)) };
}

const first = await materialize();
const firstSnapshot = JSON.stringify(first);
const second = await materialize();
if (JSON.stringify(second) !== firstSnapshot) throw new Error('NASA_DARK_MODE_MANUFACTURE_DRIFT');
const report = {
  schema: 'ggen.nasa-dark-mode.manufacture.v1',
  standing: 'ALIVE',
  generatedRoot: second.root,
  artifactCount: Object.keys(second.artifacts).length,
  artifacts: second.artifacts,
  replay: 'REPLAY_MATCH'
};
await mkdir(resolve(root, '.ggen/evidence'), { recursive: true });
await writeFile(resolve(root, '.ggen/evidence/manufacture.json'), JSON.stringify(report, null, 2) + '\n');
console.log(JSON.stringify(report));
if (!process.argv.includes('--manufacture-only')) execFileSync(process.execPath, [resolve(generated, 'verify/verify.mjs')], { cwd: generated, stdio: 'inherit' });
