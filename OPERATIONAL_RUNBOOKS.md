# Operational Runbooks - ggen v6.0.0

**Version**: 1.0 | **Date**: 2026-03-24 | **Status**: Production Ready

> **Golden Rule**: System auto-recovers for all scenarios below. Operator intervenes only if auto-recovery fails (escalation receipt will notify).

---

## Scenario 1: Sensor Manager Repeatedly Crashing

**Symptoms**:
- Logs show repeated restart attempts (SensorManager #1, #2, #3...)
- Restart count climbing rapidly in metrics
- Tracing: `error[SensorManager] sensor read failed`

**Root cause**:
Sensor read loop hitting transient failures (network timeout, I/O error) → error propagated → supervisor restarts with exponential backoff → eventually hits max retry limit if errors persist.

**Automatic recovery**:
Supervisor applies Transient restart strategy (max 3-5 retries, fixed/exponential backoff) → logs restart attempts → after max retries exceeded, emits `transient_restart_exhausted` receipt and transitions to degraded mode.

**Manual intervention**:
1. Check dashboard: `Supervisor Health` panel → locate SensorManager restart count
2. If restart_count > 5 in last 5min:
   - Run: `ggen supervisor-status --component sensor_manager`
   - If health check fails: `ggen supervisor-restart --component sensor_manager --strategy fresh` (clears state)
3. Verify recovery: Check logs for `SensorManager started` with restart_count=1 (reset)

**Steps**:
1. Dashboard: Note SensorManager restart_count and last error
2. If auto-recovery stalled: SSH to node, `journalctl -u ggen-supervisor -n 50` to inspect last failures
3. Confirm: Is sensor hardware accessible? Is network up? (transient vs permanent failure)
4. For transient: Let auto-recovery continue (backoff increases interval)
5. For permanent: Drain connections, redeploy SensorManager via `ggen deploy --component sensor_manager --force-restart`
6. Verify: Logs show single `SensorManager started (attempt #1)` entry

**Time to resolve**: 30-120 seconds (auto-recovery) or 2-5 minutes (manual redeploy)

---

## Scenario 2: Network Partition Detected

**Symptoms**:
- Governor emits `degraded_mode` receipt (health check fails 3/3 times)
- Firestore health check timeouts (DEADLINE_EXCEEDED)
- All new signals return `UNAVAILABLE` status

**Root cause**:
Network split between Cloud Run and Firestore → health check loop detects 3 consecutive failures (30s total) → system automatically degrades to read-only mode (refuses new actions, preserves state).

**Automatic recovery**:
Governor continues health checks every 10s (exponential backoff) → when Firestore responds again → emits `degraded_mode_cleared` receipt → resumes normal operation within single health check cycle (~10s post-recovery).

**Manual intervention**:
1. Dashboard: Check `Network Health` panel → look for Firestore connectivity errors
2. Run: `ggen network-diagnostic --target firestore` (validates connectivity)
3. If network is degraded:
   - Verify GCP Cloud NAT is healthy: `gcloud compute routers nats list`
   - Check Firestore region: `gcloud firestore locations list`
4. Monitor: Metrics should show recovery receipt within 30s of network restoration

**Steps**:
1. Check Network: `ggen network-diagnostic --target firestore --timeout 5s`
2. If UNAVAILABLE: Verify GCP Cloud Interconnect status (if multi-region) or NAT health
3. If tests fail: Contact GCP support; operator cannot resolve network-level issues
4. Confirm recovery: Watch Firestore latency metric drop below 500ms, health checks return 200 OK
5. Verify: Look for `degraded_mode_cleared` receipt in Firestore (timestamp = recovery time)

**Time to resolve**: 10-30 seconds (automatic) once network recovers; manual investigation 5-10 minutes

---

## Scenario 3: Circuit Breaker Open

**Symptoms**:
- External service (e.g., GCP IAM API) unreachable
- Governor logs: `circuit_breaker_open` for service-X
- New actions targeting that service return `ServiceUnavailable` immediately (no retry)

**Root cause**:
Remote service timing out → circuit breaker detects N consecutive failures (threshold = 5) → **opens circuit** → subsequent requests fail fast (prevents cascading load) → exponential backoff applies before attempting recovery probe.

**Automatic recovery**:
After backoff window (10s-5min exponential), circuit breaker sends probe request → if service responds → transitions to half-open → all requests succeed for T seconds → transitions back to closed (normal). If probe fails → backoff increases.

**Manual intervention**:
1. Dashboard: `Circuit Breaker Status` → identify open circuits by service name
2. Run: `ggen circuit-breaker-status --service iam-binding-api`
3. Manually reset (if service is confirmed healthy):
   - `ggen circuit-breaker-reset --service iam-binding-api` (transitions to half-open immediately)
   - Monitor next 30s: If probe succeeds → auto-closes; if fails → reopens, backoff continues
4. If service is down: No manual action; auto-recovery will engage when service recovers

**Steps**:
1. Identify open circuit: Firestore receipt shows `circuit_breaker_open` event
2. Verify external service health: `curl https://iam.googleapis.com/v1/health` (or service-specific health endpoint)
3. If service is healthy: `ggen circuit-breaker-reset --service iam-binding-api`
4. If service is down: Escalate to service owner; monitor for `circuit_breaker_closed` receipt (auto-recovery)
5. Verify: Check metrics `circuit_breaker_state{service="iam-binding-api"}` = 0 (closed)

**Time to resolve**: 30s-10min (automatic probe backoff) or instant (manual reset + healthy service)

---

## Scenario 4: Event Store Growing Too Fast (Disk Filling)

**Symptoms**:
- Firestore collection size approaching quota (metrics: `firestore_bytes_used` > 95% of limit)
- Write latency increasing (P99 > 1s)
- Pub/Sub messages aging (undelivered > 100s)

**Root cause**:
High signal volume → many actions → many receipts written → event store grows → disk space depletes unless cleanup triggered → Firestore may throttle writes if quota exceeded.

**Automatic recovery**:
Cleanup job runs nightly (12:00 UTC) → exports receipts > 90 days old to Cloud Storage archive → deletes from Firestore → storage usage resets. If storage usage exceeds 90% **before** nightly cleanup, auto-trigger cleanup immediately.

**Manual intervention**:
1. Dashboard: Check `Firestore Storage` gauge (% of quota used)
2. If > 95%: Run immediate cleanup:
   - `ggen firestore-cleanup --older-than-days 30 --dry-run` (preview what will be deleted)
   - If safe: `ggen firestore-cleanup --older-than-days 30 --execute` (deletes receipts > 30 days old)
3. Monitor archive: Verify exports written to `gs://ggen-audit/firestore-exports/`
4. Verify: Check `firestore_bytes_used` metric drops within 2 minutes of cleanup

**Steps**:
1. Check storage: `ggen firestore-status --metric bytes_used`
2. If % usage > 90%: Trigger cleanup: `ggen firestore-cleanup --older-than-days 30 --execute`
3. Verify archive: `gsutil ls gs://ggen-audit/firestore-exports/ | tail -5` (check latest exports)
4. Monitor metric: Watch `firestore_bytes_used` decrease over 2-5 minutes
5. If cleanup stalls: Check Cloud Storage quota/permissions: `ggen audit-permissions --resource cloud-storage`

**Time to resolve**: 2-5 minutes (automatic cleanup) or 1-2 minutes (manual trigger)

---

## Scenario 5: Consensus Quorum Lost (Only 1 Node Reachable)

**Symptoms**:
- Supervisor tree health check shows only 1/3 child processes responding
- Consensus voting fails (quorum = 2/3)
- Receipts cannot be written (Firestore unavailable from 2+ nodes)
- System enters safe mode: refuses new actions, read-only for existing state

**Root cause**:
Network partition or node crash → 2 of 3 supervisor nodes become unreachable → quorum lost → coordination impossible → system auto-degrades to prevent split-brain.

**Automatic recovery**:
Governor monitors peer connectivity every 5s → when 2/3+ become reachable again → quorum re-established → resumes normal operations. In safe mode, reads work (cache hit) but writes blocked.

**Manual intervention**:
1. Dashboard: `Supervisor Tree Health` → check which children are dead/unresponsive
2. Run: `ggen supervisor-health --detailed` (list all children + connectivity status)
3. For each dead node:
   - SSH to node, check: `systemctl status ggen-supervisor`
   - If down: `systemctl restart ggen-supervisor`
   - If up but unresponsive: Check logs: `journalctl -u ggen-supervisor -n 100 | grep -E "(error|connection)"
4. Monitor: Within 30s of node restart, quorum should restore, safe mode clears
5. Verify: `ggen supervisor-health` shows 3/3 healthy, `degraded_mode` receipt cleared

**Steps**:
1. Identify dead nodes: `ggen supervisor-health --detailed` (lists all children with status)
2. For each dead/stalled child:
   - SSH: `systemctl restart ggen-supervisor` (restart supervisor tree)
   - Wait 5s for health probe interval
3. Verify recovery: `ggen supervisor-health` shows all healthy OR no `degraded_mode` receipt in Firestore
4. If node is unrecoverable: `ggen supervisor-remove-child --id <dead_node_id>` (removes from quorum, recalculates)
5. Once quorum restored: System auto-resumes normal operations (receipts begin flowing again)

**Time to resolve**: 30s-2min (auto-recovery via health probes) or 5-10min (node restart + recovery validation)

---

## Summary: 80/20 Action Items

| Scenario | Key Metric | Green Light (Auto OK) | Red Light (Needs You) |
|----------|-----------|----------------------|----------------------|
| **Sensor Crash** | restart_count in Supervisor health | count stable < 5/5min | count > 5/5min → check sensor health |
| **Network Partition** | Firestore health check latency | < 500ms, no degraded_mode | > 1s or degraded_mode receipt → verify network |
| **Circuit Open** | circuit_breaker_state metric | 0 (closed) | 1 (open) → verify service health, manual reset if healthy |
| **Disk Filling** | firestore_bytes_used % | < 85% | > 90% → trigger cleanup, verify archive |
| **Quorum Lost** | supervisor.healthy_children count | 3/3 | < 2/3 → restart node, verify connectivity |

---

**Next Steps for Operator**:
1. Set up Prometheus alerts for all 5 metrics (thresholds defined above)
2. Configure Firestore audit logging to track receipt growth rate
3. Weekly: Run `ggen operational-check` (validates all scenarios tested)
4. Monthly: Run chaos tests (kill nodes, simulate network latency, fill disks) → confirm auto-recovery works

**Definition of Done**: No manual runbook step needed 95% of the time (system auto-recovers). Runbook exists for edge cases.

