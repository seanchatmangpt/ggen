#!/usr/bin/env node
/**
 * Mermaid上流の登録路を追跡し、全遅延読込図種の検出器、定義、描画器を
 * commit固定で解決する。上流コードは複製せず、出典URLだけを製造する。
 */
import fs from 'node:fs/promises';
import path from 'node:path';
import process from 'node:process';

const owner = 'mermaid-js';
const repo = 'mermaid';
const commit = process.env.MERMAID_COMMIT ?? 'f0ffb41c1ee1ff667b528e86c3b082249726eeef';
const rawBase = `https://raw.githubusercontent.com/${owner}/${repo}/${commit}/`;
const blobBase = `https://github.com/${owner}/${repo}/blob/${commit}/`;
const orchestrationPath = 'packages/mermaid/src/diagram-api/diagram-orchestration.ts';

async function tryFetch(repoPath) {
  const response = await fetch(rawBase + repoPath);
  if (!response.ok) return null;
  return await response.text();
}

async function fetchText(repoPath) {
  const text = await tryFetch(repoPath);
  if (text === null) throw new Error(`UPSTREAM_FETCH_FAILED ${repoPath}`);
  return text;
}

function joined(fromPath, specifier) {
  return path.posix.normalize(path.posix.join(path.posix.dirname(fromPath), specifier));
}

function sourceCandidates(fromPath, specifier) {
  const original = joined(fromPath, specifier);
  const out = [];
  if (original.endsWith('.js')) {
    out.push(original.slice(0, -3) + '.ts');
    out.push(original.slice(0, -3) + '.tsx');
  }
  out.push(original);
  return [...new Set(out)];
}

async function resolveExistingPath(fromPath, specifier) {
  for (const candidate of sourceCandidates(fromPath, specifier)) {
    if ((await tryFetch(candidate)) !== null) return candidate;
  }
  throw new Error(`UPSTREAM_IMPORT_NOT_FOUND from=${fromPath} specifier=${specifier}`);
}

function importMap(source) {
  const map = new Map();
  const re = /import\s+(?:\{\s*([^}]+?)\s*\}|([A-Za-z_$][\w$]*))\s+from\s+['"]([^'"]+)['"]/g;
  for (const match of source.matchAll(re)) {
    const spec = match[3];
    if (!spec.startsWith('../diagrams/')) continue;
    if (match[2]) map.set(match[2], spec);
    if (match[1]) {
      for (const item of match[1].split(',')) {
        const parts = item.trim().split(/\s+as\s+/);
        map.set(parts.at(-1), spec);
      }
    }
  }
  return map;
}

function registeredNames(source) {
  return [...source.matchAll(/registerLazyLoadedDiagrams\(([\s\S]*?)\);/g)]
    .flatMap((m) => m[1].split(','))
    .map((x) => x.replace(/\/\/.*$/gm, '').trim())
    .filter(Boolean);
}

function diagramId(detectorSource, fallback) {
  return detectorSource.match(/const\s+id\s*=\s*['"]([^'"]+)['"]/)?.[1] ?? fallback;
}

function loaderSpecifier(detectorSource) {
  return detectorSource.match(/import\(\s*['"]([^'"]+)['"]\s*\)/)?.[1] ?? null;
}

function rendererSpecifiers(definitionSource) {
  const specs = [];
  const re = /import\s+(?:\{[^}]*\}|\*\s+as\s+[A-Za-z_$][\w$]*|[A-Za-z_$][\w$]*)\s+from\s+['"]([^'"]+)['"]/g;
  for (const match of definitionSource.matchAll(re)) {
    if (/renderer|svgDraw/i.test(match[1])) specs.push(match[1]);
  }
  return [...new Set(specs)];
}

const orchestration = await fetchText(orchestrationPath);
const imports = importMap(orchestration);
const names = registeredNames(orchestration);
const rows = [];

for (const name of names) {
  const detectorSpec = imports.get(name);
  if (!detectorSpec) continue;
  const detectorPath = await resolveExistingPath(orchestrationPath, detectorSpec);
  const detectorSource = await fetchText(detectorPath);
  const id = diagramId(detectorSource, name);
  const loader = loaderSpecifier(detectorSource);
  if (!loader) {
    rows.push({ id, detectorPath, definitionPath: null, rendererPaths: [], status: 'NO_DYNAMIC_LOADER' });
    continue;
  }

  const definitionPath = await resolveExistingPath(detectorPath, loader);
  const definitionSource = await fetchText(definitionPath);
  const rendererPaths = [];
  for (const specifier of rendererSpecifiers(definitionSource)) {
    rendererPaths.push(await resolveExistingPath(definitionPath, specifier));
  }

  rows.push({
    id,
    detectorPath,
    definitionPath,
    rendererPaths: [...new Set(rendererPaths)],
    detectorUrl: blobBase + detectorPath,
    definitionUrl: blobBase + definitionPath,
    rendererUrls: [...new Set(rendererPaths)].map((p) => blobBase + p),
    status: rendererPaths.length ? 'RESOLVED' : 'INLINE_TRANSITIVE_OR_SHARED'
  });
}

// ZenUML is an external Mermaid package, not part of the core addDiagrams list.
rows.push({
  id: 'zenuml',
  detectorPath: 'packages/mermaid-zenuml/src/detector.ts',
  definitionPath: 'packages/mermaid-zenuml/src/zenuml-definition.ts',
  rendererPaths: ['packages/mermaid-zenuml/src/zenumlRenderer.ts'],
  detectorUrl: blobBase + 'packages/mermaid-zenuml/src/detector.ts',
  definitionUrl: blobBase + 'packages/mermaid-zenuml/src/zenuml-definition.ts',
  rendererUrls: [blobBase + 'packages/mermaid-zenuml/src/zenumlRenderer.ts'],
  status: 'EXTERNAL_PACKAGE'
});

rows.sort((a, b) => a.id.localeCompare(b.id));
await fs.mkdir('build/mermaid-pack', { recursive: true });
await fs.writeFile(
  'build/mermaid-pack/renderer-sources.json',
  JSON.stringify({ repository: `${owner}/${repo}`, commit, orchestrationPath, rows }, null, 2) + '\n'
);
console.log(`GREEN resolved ${rows.length} Mermaid renderer source entries at ${commit}`);
