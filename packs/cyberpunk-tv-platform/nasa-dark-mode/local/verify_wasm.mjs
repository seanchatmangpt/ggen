import { createHash } from 'node:crypto';
import { readFile, writeFile, mkdir } from 'node:fs/promises';
import { resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { applyRemoteKey, createRemoteState, MODES } from '../generated/deck/nasa-dark-mode.mjs';

const here = resolve(fileURLToPath(new URL('.', import.meta.url)));
const pack = resolve(here, '..');
const evidenceDir = resolve(pack, '.ggen/evidence/no-ci');
const wasmPath = process.argv[2] ?? resolve(evidenceDir, 'nasa-core.wasm');
const mutantPath = process.argv[3] ?? resolve(evidenceDir, 'nasa-core-mutant.wasm');

const keyCases = [
  ['left', 0, 1],
  ['right', 1, 2],
  ['up', 2, 3],
  ['down', 3, 4],
  ['OK', 4, 5],
  ['back', 5, 6],
  ['voice-search', 99, 0]
];

function sha256(bytes) {
  return createHash('sha256').update(bytes).digest('hex');
}

function decode(packed) {
  return {
    modeIndex: packed & 0x0f,
    missionIndex: ((packed >>> 4) & 0xff) - 1,
    privacyCurtain: Boolean((packed >>> 12) & 1),
    admitted: Boolean((packed >>> 13) & 1),
    operationCode: (packed >>> 14) & 0x0f
  };
}

async function instantiate(path) {
  const bytes = await readFile(path);
  const { instance } = await WebAssembly.instantiate(bytes, {});
  return { bytes, exports: instance.exports };
}

function verifyExports(exports, stopAfterFirst = false) {
  const mismatches = [];
  let profiles = 0;
  for (let missionCount = 0; missionCount <= 8; missionCount += 1) {
    const missions = missionCount === 0 ? [-1] : Array.from({ length: missionCount }, (_, index) => index);
    for (let modeIndex = 0; modeIndex < MODES.length; modeIndex += 1) {
      for (const missionIndex of missions) {
        for (const privacyCurtain of [false, true]) {
          for (const [key, keyCode, operationCode] of keyCases) {
            profiles += 1;
            const state = createRemoteState(missionCount);
            state.modeIndex = modeIndex;
            state.missionIndex = missionIndex;
            state.privacyCurtain = privacyCurtain;
            const expectedResult = applyRemoteKey(state, key);
            const expected = {
              modeIndex: expectedResult.state.modeIndex,
              missionIndex: expectedResult.state.missionIndex,
              privacyCurtain: expectedResult.state.privacyCurtain,
              admitted: expectedResult.standing === 'ALIVE',
              operationCode
            };
            const observed = decode(exports.apply_remote(modeIndex, missionIndex, missionCount, privacyCurtain ? 1 : 0, keyCode) >>> 0);
            if (JSON.stringify(expected) !== JSON.stringify(observed)) {
              mismatches.push({ missionCount, modeIndex, missionIndex, privacyCurtain, key, expected, observed });
              if (stopAfterFirst) return { profiles, mismatches };
            }
          }
        }
      }
    }
  }
  return { profiles, mismatches };
}

const valid = await instantiate(wasmPath);
if (valid.exports.policy_version() !== 1) throw new Error('WASM_POLICY_VERSION_REFUSED');
const primary = verifyExports(valid.exports);
if (primary.mismatches.length) throw new Error(`WASM_EQUIVALENCE_BROKEN:${JSON.stringify(primary.mismatches[0])}`);

const mutant = await instantiate(mutantPath);
const mutantResult = verifyExports(mutant.exports, true);
if (mutantResult.mismatches.length === 0) throw new Error('WASM_MUTATION_SURVIVED');

const invalid = decode(valid.exports.apply_remote(2, 1, 2, 1, 99) >>> 0);
if (invalid.admitted || invalid.modeIndex !== 2 || invalid.missionIndex !== 1 || !invalid.privacyCurtain) {
  throw new Error(`WASM_INVALID_KEY_REFUSAL_BROKEN:${JSON.stringify(invalid)}`);
}

const report = {
  schema: 'ggen.nasa-dark-mode.wasm-equivalence.v1',
  standing: 'ALIVE',
  compilerBody: 'clang-wasm32-no-libc',
  policyVersion: valid.exports.policy_version(),
  exhaustiveProfiles: primary.profiles,
  mismatches: 0,
  invalidKeyRefusal: 'PASS',
  mutationControl: {
    standing: 'KILLED',
    firstMismatch: mutantResult.mismatches[0]
  },
  wasm: {
    path: wasmPath,
    bytes: valid.bytes.length,
    sha256: sha256(valid.bytes)
  }
};
await mkdir(evidenceDir, { recursive: true });
await writeFile(resolve(evidenceDir, 'wasm-equivalence.json'), JSON.stringify(report, null, 2) + '\n');
console.log(JSON.stringify(report));
