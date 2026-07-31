// Generated from ARD §2 system-planes (TICKET-012). One entry per plane query row.
// A true one-folder-per-row split was not attempted: no pack in this workspace uses a
// per-row-dynamic "to:" path inside a for-loop body (grep across packs/*/templates/*.tmpl
// confirmed only single, static `to:` targets per template file). This module is the
// single-file projection of the same 4 rows instead.
export interface SystemPlane {
  id: string;
  title: string;
}

export const SYSTEM_PLANES: SystemPlane[] = [
  { id: "https://github.com/seanchatmangpt/ggen/blob/main/packs/wasm4pm-interview-assist-pack/doc/ard#plane-manufacturing", title: "2.1 Manufacturing plane" },
  { id: "https://github.com/seanchatmangpt/ggen/blob/main/packs/wasm4pm-interview-assist-pack/doc/ard#plane-projection", title: "2.4 Projection plane" },
  { id: "https://github.com/seanchatmangpt/ggen/blob/main/packs/wasm4pm-interview-assist-pack/doc/ard#plane-runtime", title: "2.2 Runtime cognition plane" },
  { id: "https://github.com/seanchatmangpt/ggen/blob/main/packs/wasm4pm-interview-assist-pack/doc/ard#plane-sandbox", title: "2.3 Sandbox actuation plane" },
];
