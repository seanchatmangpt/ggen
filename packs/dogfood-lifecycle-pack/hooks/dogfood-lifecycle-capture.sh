#!/usr/bin/env bash
# dogfood-lifecycle-capture.sh — Operation Dogfood (v26.7.13) PostToolUse hook.
#
# Appends one dfl:ToolEvent Turtle node per tool event to
# .cargo-cicd/lifecycle/session-<id>.ttl, conforming to
# packs/dogfood-lifecycle-pack/{ontology,shapes}.ttl. This is how Multifractal
# Workflow governs its own Claude Code lifecycle: every tool call becomes an
# admitted RDF observation the session-end recipe can validate + receipt.
#
# OBSERVATION ONLY. This hook records what a tool event did; it never asserts
# authority, permission, or actuation. Outcome is the TOOL-level result (Ok
# unless the tool itself errored/was blocked); the underlying command's exit
# lives inside the content-addressed prov:generated payload, not here.
#
# BEST-EFFORT: any internal failure must never disrupt the session. Every step
# is guarded and the hook always exits 0.

payload=$(cat 2>/dev/null || true)
{
  command -v jq >/dev/null 2>&1 || exit 0
  command -v b3sum >/dev/null 2>&1 || exit 0

  sid=$(printf '%s' "$payload" | jq -r '.session_id // "unknown"' 2>/dev/null || echo unknown)
  tool=$(printf '%s' "$payload" | jq -r '.tool_name // "unknown"' 2>/dev/null || echo unknown)
  # Only the closed tool-name set is admitted by dfl:ToolNameScheme / the shape.
  case "$tool" in
    Bash|Edit|Write|Read|Grep|Glob|Task|WebFetch|WebSearch) : ;;
    *) exit 0 ;;
  esac

  # Content-address the input and the result with real blake3 digests.
  in_hash=$(printf '%s' "$payload" | jq -c '.tool_input // {}' 2>/dev/null | b3sum --no-names 2>/dev/null | cut -c1-64)
  out_hash=$(printf '%s' "$payload" | jq -c '.tool_response // {}' 2>/dev/null | b3sum --no-names 2>/dev/null | cut -c1-64)
  [ -n "$in_hash" ] && [ -n "$out_hash" ] || exit 0

  # Tool-level outcome: Error only if the tool itself signalled an error.
  outcome=$(printf '%s' "$payload" | jq -r '
    if (.tool_response.is_error // .is_error // false) == true then "Error" else "Ok" end
  ' 2>/dev/null || echo Ok)
  case "$outcome" in Ok|Error|Blocked) : ;; *) outcome=Ok ;; esac

  ts=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

  dir="/Users/sac/praxis/.cargo-cicd/lifecycle"
  mkdir -p "$dir" 2>/dev/null || exit 0
  f="$dir/session-${sid}.ttl"

  # Concurrent tool calls in one batch fire this hook concurrently. Without a
  # lock, two invocations can read the same sequence count and interleave
  # their multi-line appends (each printf is its own write(2) call, so a
  # single logical write is not atomic), corrupting the Turtle block
  # structure. Serialize the read-seq + append critical section with a
  # portable mkdir-based lock (atomic, no external dependency) instead of
  # relying on flock, which is not part of base macOS.
  lock="$f.lock"
  tries=0
  while ! mkdir "$lock" 2>/dev/null; do
    tries=$((tries + 1))
    [ "$tries" -ge 50 ] && exit 0
    sleep 0.05
  done
  trap 'rmdir "$lock" 2>/dev/null' EXIT

  if [ ! -f "$f" ]; then
    header=$(printf '@prefix dfl:     <http://seanchatmangpt.github.io/packs/dogfood-lifecycle#> .\n@prefix prov:    <http://www.w3.org/ns/prov#> .\n@prefix dcterms: <http://purl.org/dc/terms/> .\n@prefix skos:    <http://www.w3.org/2004/02/skos/core#> .\n@prefix time:    <http://www.w3.org/2006/time#> .\n@prefix xsd:     <http://www.w3.org/2001/XMLSchema#> .\n\n<urn:dfl:agent:%s> a prov:SoftwareAgent , prov:Agent .\n\n<urn:dfl:session:%s> a dfl:Session , prov:Activity ;\n    dcterms:identifier "%s" ;\n    prov:wasAssociatedWith <urn:dfl:agent:%s> .\n' "$sid" "$sid" "$sid" "$sid")
    seq=0
  else
    header=""
    seq=$(grep -c 'a dfl:ToolEvent' "$f" 2>/dev/null || echo 0)
  fi

  # Build the full event node as one string and append it with a SINGLE
  # printf (one write(2) call), so even a lock-bypassing writer can't split
  # a well-formed block mid-statement.
  event=$(printf '\n<urn:dfl:event:%s:%s> a dfl:ToolEvent , prov:Activity ;\n    dcterms:isPartOf <urn:dfl:session:%s> ;\n    prov:wasAssociatedWith <urn:dfl:agent:%s> ;\n    skos:notation "%s" ;\n    dfl:sequenceIndex "%s"^^xsd:integer ;\n    time:inXSDDateTimeStamp "%s"^^xsd:dateTimeStamp ;\n    prov:used <urn:blake3:%s> ;\n    prov:generated <urn:blake3:%s> ;\n    dfl:outcome dfl:%s .\n' "$sid" "$seq" "$sid" "$sid" "$tool" "$seq" "$ts" "$in_hash" "$out_hash" "$outcome")

  printf '%s%s' "$header" "$event" >> "$f" 2>/dev/null || true

  rmdir "$lock" 2>/dev/null
  trap - EXIT
} 2>/dev/null || true

exit 0
