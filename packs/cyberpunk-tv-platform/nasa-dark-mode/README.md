# NASA Dark Mode

NASA Dark Mode is a bounded television mission-control projection manufactured as an extension of the Cyberpunk Television Platform.

It preserves one admitted subject across two interchangeable runtime bodies:

```text
NASA public-media policy + EONET event metadata + GIBS imagery authority
→ normalized mission feed
→ browser/deck.gl layer plan OR Roku SceneGraph projection
→ remote-control intent broker
→ consequence/refusal receipt
→ deterministic replay
```

## G1 fence

- NASA names, imagery, and scientific data provide source context; this project is not an official NASA product and must not imply NASA endorsement.
- NASA logos, insignia, and identifiers are not bundled.
- Every displayed source retains attribution and any item-level copyright notice.
- EONET is used for visualization and situational awareness, not as authoritative emergency guidance.
- GIBS imagery remains a remote public-data layer; no imagery is copied into this repository.
- deck.gl is the browser renderer. Native Roku execution is SceneGraph XML plus BrightScript. Both consume the same mission-feed contract.
- Hooks and remote keys manufacture intents only. No external actuation bypasses the bounded broker.

## Execute the local capsule

```bash
node capsule.mjs
(cd generated/roku && zip -qr ../.ggen/evidence/nasa-dark-mode-roku.zip manifest source components data)
(cd generated && node verify/verify.mjs --verify-package .ggen/evidence/nasa-dark-mode-roku.zip)
```

The local capsule can prove source, transformation, state-machine, package, receipt, and replay behavior. A real Roku device is still required for device-boundary standing.
