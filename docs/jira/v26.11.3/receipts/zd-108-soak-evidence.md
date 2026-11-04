# ZD-108 — AB-20 detached soak evidence receipt

Date: 2026-11-04. Lane: read-only census (Explore), coordinator-authored
receipt. Subject: the detached evidence run BK-2 left alive
(pid 28463, log `/tmp/ab20_detached.log`).

## Result

- pid 28463: not alive — run finished.
- All three markers present in the log: line 48 `SOAK1_EXIT=0`,
  line 93 `SOAK2_EXIT=0`, line 142 `FULL_EXIT=0`.
- Final suite line: `test result: ok. 4 passed; 0 failed; 0 ignored`
  (1320.78s, soak_10k-class suite, `soak_10k_did_change_cycles … ok`).
- Elapsed well under the ~4.5h ETA (log last modified 05:47).

## Standing

AB-20's tokens_delta_stress now carries TWO independent green witnesses:
the AB-20 lane's own gates (green ×2, receipt `ab-20-tokens-stress.md`,
2327-2379s soaks) and this detached run (soak ×2 + full release suite,
exit 0). Evidence-complete.

Caveat: the log file lives in /tmp and is not repo-pinned — this receipt
is the durable record; /tmp cleanup may reclaim the log.
