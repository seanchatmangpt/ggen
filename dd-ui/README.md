# Deterministic Dynamic UI profile

This directory contributes ggen's bounded world observations to the ecosystem DDUI contract.

The runtime projection law is owned by `seanchatmangpt/wasm4pm` DDUI v2. This profile is verified against exact engine commit `8d48e784a4215857c8428c09bb09a91c05a8be97`; it does not fork renderer logic.

`world.json` is observation input, not an authority grant. The verifier projects 5 avatars across 4 contexts, checks exact replay, requires `irreversibleUiSelections = 0`, and refuses any runtime-AI render authority or direct actuation.

The reusable RDF/query/template manufacturing grammar is owned by `seanchatmangpt/ggen-marketplace` pack `deterministic-dynamic-ui-pack`.

DfCM rule: preserve all lawful reversible presentation candidates before deterministic presentation selection. Business SELECT, CONSTRUCT, and DO remain separate; rendered actions remain unselected intents.
