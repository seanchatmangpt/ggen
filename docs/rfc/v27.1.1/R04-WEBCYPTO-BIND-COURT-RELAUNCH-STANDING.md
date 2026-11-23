# R-04 WebCrypto bind court — relaunch ALIVE (v27.1.1)

| Field | Value |
|-------|-------|
| Document ID | `R04-WEBCYPTO-BIND-COURT-RELAUNCH-STANDING` |
| Prior attempt | [bc-9589ff95](bc-9589ff95-17ad-5fb8-b804-89725457e51e) → **ERROR** `resource_exhausted` (empty) |
| Relaunch agent | [bc-cd52b5d7](bc-cd52b5d7-1e20-5b70-93f2-faf3bb6deaec) on `seanchatmangpt/ash_a2a` |
| Agent status | **IDLE** / completed |
| Branch | `cursor/r04-webcrypto-channel-bind-bc54` @ `ff08fc06` |
| Court | `mix test test/ash_a2a/session/r04_webcrypto_channel_bind_court_test.exs --only serial_solo` → **2**/EXIT:**0**; with SESS → **9**/EXIT:**0** |
| Claim | Chrome CDP + `AshA2A.Session.ChannelBind` / `Statem.bind_channel/2` + `verify_reconnect/2` — non-extractable WebCrypto channel-bind **ALIVE** (local court) |
| PR | **ABSENT** — `gh pr create --draft` → **403** (`Resource not accessible by integration`) |
| Body closure | still **OPEN** until draft PR + CI green on ash_a2a |
| Foundry / zero-HITL | **NOT** promoted |

**Exact next:** Founder/parent opens draft PR from `ff08fc06`; CI green; replay court command above.
