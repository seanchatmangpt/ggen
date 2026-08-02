#!/usr/bin/env python3
from __future__ import annotations

import hashlib
import json
import re
import sys
from pathlib import Path
from playwright.sync_api import sync_playwright

PACK = Path(__file__).resolve().parent.parent
GEN = PACK / "generated"
EVIDENCE = PACK / ".ggen/evidence/no-ci"
EVIDENCE.mkdir(parents=True, exist_ok=True)

module = (GEN / "deck/nasa-dark-mode.mjs").read_text()
module = module.replace("export function ", "function ")
module = re.sub(r"export \{ MODES, REMOTE_ACTIONS \};\s*$", "", module)
fixture = json.loads((GEN / "fixtures/eonet-events.json").read_text())

app = r'''
const fixture = __FIXTURE__;
const feed = buildMissionFeed(fixture, '2026-08-01');
let state = createRemoteState(feed.missions.length);
const canvas = document.getElementById('earth');
const gl = canvas.getContext('webgl2');
if (!gl) throw new Error('WEBGL2_CONTEXT_REFUSED');
const vertex = `#version 300 es
in vec2 position;
void main() { gl_Position = vec4(position, 0.0, 1.0); gl_PointSize = 18.0; }`;
const fragment = `#version 300 es
precision highp float;
out vec4 color;
void main() {
  vec2 p = gl_PointCoord - vec2(0.5);
  if (dot(p,p) > 0.25) discard;
  color = vec4(0.35, 0.95, 1.0, 1.0);
}`;
function compile(type, source) {
  const shader = gl.createShader(type);
  gl.shaderSource(shader, source);
  gl.compileShader(shader);
  if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) throw new Error('SHADER_COMPILE_REFUSED:' + gl.getShaderInfoLog(shader));
  return shader;
}
const program = gl.createProgram();
gl.attachShader(program, compile(gl.VERTEX_SHADER, vertex));
gl.attachShader(program, compile(gl.FRAGMENT_SHADER, fragment));
gl.linkProgram(program);
if (!gl.getProgramParameter(program, gl.LINK_STATUS)) throw new Error('SHADER_LINK_REFUSED:' + gl.getProgramInfoLog(program));
gl.useProgram(program);
const points = eonetFeatureCollection(fixture).features.map(feature => {
  const [lon, lat] = feature.geometry.coordinates;
  return [lon / 180, lat / 90];
}).flat();
const buffer = gl.createBuffer();
gl.bindBuffer(gl.ARRAY_BUFFER, buffer);
gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(points), gl.STATIC_DRAW);
const positionLocation = gl.getAttribLocation(program, 'position');
gl.enableVertexAttribArray(positionLocation);
gl.vertexAttribPointer(positionLocation, 2, gl.FLOAT, false, 0, 0);
gl.clearColor(0.02, 0.03, 0.08, 1);
gl.clear(gl.COLOR_BUFFER_BIT);
gl.drawArrays(gl.POINTS, 0, points.length / 2);

function render() {
  const mission = feed.missions[state.missionIndex];
  document.body.dataset.mode = MODES[state.modeIndex];
  document.getElementById('mode').textContent = MODES[state.modeIndex].toUpperCase();
  document.getElementById('mission').textContent = state.privacyCurtain ? 'PRIVACY CURTAIN' : mission.title;
  document.getElementById('meta').textContent = state.privacyCurtain ? 'Press Escape to restore' : `${mission.categories.join(' · ')} | ${mission.eventDate}`;
  document.getElementById('receipt').textContent = state.previousReceipt || feed.receipt.digest;
  document.getElementById('curtain').hidden = !state.privacyCurtain;
}
document.addEventListener('keydown', event => {
  const keys = {ArrowLeft:'left', ArrowRight:'right', ArrowUp:'up', ArrowDown:'down', Enter:'OK', Escape:'back'};
  const key = keys[event.key];
  if (!key) return;
  event.preventDefault();
  state = applyRemoteKey(state, key).state;
  render();
});
let mutationKilled = false;
try { compile(gl.FRAGMENT_SHADER, '#version 300 es\nthis is invalid'); } catch (error) { mutationKilled = String(error).includes('SHADER_COMPILE_REFUSED'); }
render();
const debug = gl.getExtension('WEBGL_debug_renderer_info');
window.__NDM__ = {
  standing: 'ALIVE',
  webgl2: true,
  renderer: debug ? gl.getParameter(debug.UNMASKED_RENDERER_WEBGL) : gl.getParameter(gl.RENDERER),
  vendor: debug ? gl.getParameter(debug.UNMASKED_VENDOR_WEBGL) : gl.getParameter(gl.VENDOR),
  pointCount: points.length / 2,
  feedDigest: feed.receipt.digest,
  mutationKilled,
  getState: () => structuredClone(state)
};
'''.replace("__FIXTURE__", json.dumps(fixture, separators=(",", ":")))

html = f'''<!doctype html><html><head><meta charset="utf-8"><style>
html,body{{margin:0;width:100%;height:100%;overflow:hidden;background:#050711;color:#f5f8ff;font-family:Arial,sans-serif}}
body{{background:radial-gradient(circle at 35% 20%,#17294b,#050711 58%)}}
#earth{{position:absolute;inset:0;width:100%;height:100%}}
header{{position:absolute;left:48px;top:36px;z-index:2}}
.eyebrow{{letter-spacing:.25em;color:#8deaff;font-size:13px}}h1{{font-size:82px;margin:6px 0;line-height:.9}}
#mode{{position:absolute;right:48px;top:48px;border:1px solid #8deaff;padding:12px 18px}}
#card{{position:absolute;right:48px;bottom:64px;width:520px;padding:24px;background:#050711e8;border:1px solid #8deaff88;z-index:2}}
#mission{{font-size:30px;font-weight:bold}}#meta{{margin-top:12px;color:#bdeeff}}#receipt{{font-family:monospace;font-size:10px;word-break:break-all;margin-top:20px}}
footer{{position:absolute;left:48px;right:48px;bottom:22px;display:flex;justify-content:space-between;font-size:12px;z-index:2}}
#curtain{{position:absolute;inset:0;background:#000;z-index:5}}#curtain span{{position:absolute;inset:45% 0 auto;text-align:center;font-size:42px}}
</style></head><body><canvas id="earth" width="1280" height="720"></canvas><header><div class="eyebrow">PLANETARY MISSION CONTROL</div><h1>NASA DARK MODE</h1></header><div id="mode"></div><section id="card"><div id="mission"></div><div id="meta"></div><div id="receipt"></div></section><footer><span>NASA Earthdata GIBS · NASA EONET</span><span>Not an official NASA product. No endorsement implied.</span></footer><div id="curtain" hidden><span>PRIVACY CURTAIN</span></div><script>{module}\n{app}</script></body></html>'''

flags = [
    "--use-angle=swiftshader",
    "--enable-unsafe-swiftshader",
    "--enable-webgl",
    "--ignore-gpu-blocklist",
    "--no-sandbox",
    "--disable-gpu-sandbox",
]

with sync_playwright() as p:
    browser = p.chromium.launch(executable_path="/usr/bin/chromium", headless=False, args=flags)
    page = browser.new_page(viewport={"width": 1280, "height": 720})
    console: list[dict[str, str]] = []
    page.on("console", lambda message: console.append({"type": message.type, "text": message.text}))
    page.on("pageerror", lambda error: console.append({"type": "pageerror", "text": str(error)}))
    page.set_content(html, wait_until="domcontentloaded")
    try:
        page.wait_for_function("window.__NDM__ && window.__NDM__.standing === 'ALIVE'", timeout=10000)
    except Exception as error:
        print(json.dumps({"standing":"BUILD_BROKEN","error":str(error),"console":console,"body":page.locator("body").inner_text()[:2000]}), file=sys.stderr)
        raise
    initial = page.evaluate("window.__NDM__")
    for key in ["ArrowRight", "ArrowRight", "ArrowDown", "Enter", "Escape"]:
        page.keyboard.press(key)
    final_state = page.evaluate("window.__NDM__.getState()")
    dom = {
        "mode": page.locator("#mode").inner_text(),
        "mission": page.locator("#mission").inner_text(),
        "meta": page.locator("#meta").inner_text(),
        "privacyHidden": page.locator("#curtain").get_attribute("hidden") is not None,
    }
    screenshot = EVIDENCE / "browser-webgl2.png"
    page.screenshot(path=str(screenshot))
    browser.close()

if not initial["webgl2"] or initial["pointCount"] != 2 or not initial["mutationKilled"]:
    raise RuntimeError(f"WEBGL2_PROJECTION_BROKEN:{initial}")
if final_state["modeIndex"] != 2 or final_state["missionIndex"] != 1 or not final_state["privacyCurtain"]:
    raise RuntimeError(f"BROWSER_REMOTE_DIVERGENCE:{final_state}")
if len(final_state["receipts"]) != 10:
    raise RuntimeError(f"BROWSER_RECEIPT_CHAIN_BROKEN:{len(final_state['receipts'])}")
if dom["mode"] != "BRIEFING" or dom["mission"] != "PRIVACY CURTAIN" or dom["privacyHidden"]:
    raise RuntimeError(f"BROWSER_DOM_DIVERGENCE:{dom}")

report = {
    "schema": "ggen.nasa-dark-mode.browser-webgl2-evidence.v1",
    "standing": "ALIVE",
    "executionMode": "headed-chromium-xvfb-angle-swiftshader",
    "chromium": "144.0.7559.96",
    "webgl2": True,
    "renderer": initial["renderer"],
    "vendor": initial["vendor"],
    "pointCount": initial["pointCount"],
    "missionFeedDigest": initial["feedDigest"],
    "finalState": final_state,
    "dom": dom,
    "shaderMutationControl": "KILLED",
    "screenshot": {
        "path": str(screenshot),
        "sha256": hashlib.sha256(screenshot.read_bytes()).hexdigest(),
        "bytes": screenshot.stat().st_size,
    },
    "console": console,
    "deckGlRuntime": {
        "standing": "BLOCKED_DEPENDENCY_TRANSPORT",
        "pinnedVersion": "9.1.14",
        "note": "WebGL2 executed locally; the exact deck.gl package body was not available through admitted local transports."
    }
}
(EVIDENCE / "browser-webgl2.json").write_text(json.dumps(report, indent=2, sort_keys=True) + "\n")
print(json.dumps(report, sort_keys=True))
