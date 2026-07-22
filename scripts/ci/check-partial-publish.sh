#!/usr/bin/env bash
# check-partial-publish.sh — read-only partial-publish detector.
#
# WHY THIS EXISTS (retrofit:PartialPublishRollbackUnknown,
# .tcps/retrofit/ggen/load-path.ttl): a single git tag fans out to 4
# independently-triggered, independently-shaped release targets —
# release.yml (workflow_dispatch, uploads 4 binary tarballs + 4 sha256 to
# ONE GitHub Release object), release-debian.yml (push:tags, builds+uploads
# a .deb), docker-build-push.yml (push:tags, pushes 2 images to 2
# registries), homebrew-release.yml (release:published, bumps an EXTERNAL
# tap formula). Nothing in this repo previously checked whether all 4 of
# them actually finished for a given tag. That gap was not hypothetical:
# release-debian.yml silently broke on a relative-path bug, docker-build-
# push.yml silently broke on an invalid `now()` expression (every run fails
# before any job starts — confirmed: zero successful runs in its visible
# history), and homebrew-release.yml silently never fired for months
# because GitHub suppresses `release:published` webhooks for releases
# created with the default GITHUB_TOKEN (confirmed: zero runs for any real
# v26.7.x tag). All three were "green" from release.yml's point of view —
# it has no visibility into the other 3 targets and never claimed to.
#
# This script is READ-ONLY DETECTION ONLY (hard constraint, see
# .tcps/retrofit/ggen/load-path.ttl's PartialPublishRollbackUnknown
# evidence — auto-rollback is explicitly out of scope, a future decision
# requiring human authorization). Every call below is a GET: `gh release
# view`, `gh run list`, `gh api .../actions/runs/<id>/jobs`, `gh api
# repos/<homebrew-tap>/contents/...`. Nothing here creates, deletes, or
# modifies a tag, release, package, docker image, or external repo. Fixing
# a detected gap (re-dispatching a workflow, editing the tap formula by
# hand) is a separate, human-triggered action.
#
# Usage:
#   scripts/ci/check-partial-publish.sh [TAG]
#   REPO=owner/repo scripts/ci/check-partial-publish.sh v26.7.44
#
# TAG defaults to the most recent tag reported by the GitHub tags API when
# omitted (used by the workflow_dispatch leg of detect-partial-publish.yml,
# which has no triggering run to read a head_branch from).
#
# Output: one Markdown table (Target | Status | Detail) to stdout, suitable
# for `>> "$GITHUB_STEP_SUMMARY"` or plain terminal reading.
#
# Exit code: 1 only when the overall verdict is FAIL (a target definitively
# did not publish). Never exits 1 for PENDING — a target that simply hasn't
# run yet is not a build break (see INV-2 in the design this implements:
# release-debian/docker/homebrew trigger asynchronously, independently of
# release.yml, and can legitimately still be in flight when this runs).
#
# KNOWN LIMITATION, disclosed not hidden (found by adversarial review of this
# script, not a silent gap): check_release_binaries/check_release_debian
# verify asset PRESENCE by name only (`gh release view --json assets`), never
# downloading and comparing content against the GitHub-computed `digest`
# field the API actually returns. A build step that succeeds at the process
# level while producing empty/truncated/wrong bytes (e.g. runner disk
# pressure, a stale cached binary) would upload a file under the exact
# expected name and this script would report PASS. Verifying content would
# require downloading each asset and its sha256 sidecar (real network I/O,
# not a lightweight metadata GET like every other check here) -- a real,
# separate, larger feature deliberately not bundled into this first pass.
# The failure classes this script DOES catch (the ones actually observed in
# this repo's history -- path bugs, invalid expressions, webhook suppression,
# never-ran) remain fully covered.

set -euo pipefail

REPO="${REPO:-seanchatmangpt/ggen}"

for bin in gh jq; do
  if ! command -v "$bin" >/dev/null 2>&1; then
    echo "BUILD_BROKEN: required tool '$bin' not found on PATH" >&2
    exit 2
  fi
done

TAG="${1:-}"
if [ -z "$TAG" ]; then
  TAG="$(gh api "repos/$REPO/tags" --jq '.[0].name' 2>/dev/null || true)"
  if [ -z "$TAG" ]; then
    echo "BUILD_BROKEN: no TAG argument given and 'gh api repos/$REPO/tags' returned none" >&2
    exit 2
  fi
fi
VERSION="${TAG#v}"

# Rows accumulate as "Target|Status|Detail" (pipe-delimited; Detail must not
# itself contain a literal '|' — enforced by construction below, never by
# escaping untrusted input).
ROWS=()
OVERALL="PASS"

bump_overall() {
  # FAIL beats PENDING beats PASS.
  local status="$1"
  if [ "$status" = "FAIL" ]; then
    OVERALL="FAIL"
  elif [ "$status" = "PENDING" ] && [ "$OVERALL" != "FAIL" ]; then
    OVERALL="PENDING"
  fi
}

add_row() {
  local target="$1" status="$2" detail="$3"
  ROWS+=("${target}|${status}|${detail}")
  bump_overall "$status"
}

# ---------------------------------------------------------------------------
# Target 1 — release.yml (binary release: 4 tarballs + 4 sha256 sidecars on
# ONE GitHub Release object keyed by $TAG).
# ---------------------------------------------------------------------------
check_release_binaries() {
  local platforms=(x86_64-apple-darwin aarch64-apple-darwin x86_64-unknown-linux-gnu aarch64-unknown-linux-gnu)
  local expected=()
  for p in "${platforms[@]}"; do
    expected+=("ggen-${p}.tar.gz" "ggen-${p}.tar.gz.sha256")
  done

  local assets_json
  if ! assets_json="$(gh release view "$TAG" -R "$REPO" --json assets -q '.assets[].name' 2>/dev/null)"; then
    add_row "release.yml" "PENDING" "no GitHub Release object exists yet for ${TAG}"
    return
  fi

  local missing=()
  for want in "${expected[@]}"; do
    if ! grep -qxF "$want" <<<"$assets_json"; then
      missing+=("$want")
    fi
  done

  if [ "${#missing[@]}" -eq 0 ]; then
    add_row "release.yml" "PASS" "all 8 assets present (4 tarballs + 4 sha256)"
  else
    add_row "release.yml" "FAIL" "release exists but missing: $(IFS=,; echo "${missing[*]}")"
  fi
}

# ---------------------------------------------------------------------------
# Target 2 — release-debian.yml (uploads a .deb to the same Release object).
# ---------------------------------------------------------------------------
check_release_debian() {
  local assets_json
  assets_json="$(gh release view "$TAG" -R "$REPO" --json assets -q '.assets[].name' 2>/dev/null || true)"

  if grep -q '\.deb$' <<<"$assets_json"; then
    add_row "release-debian.yml" "PASS" ".deb asset present on the release"
    return
  fi

  local runs_json
  runs_json="$(gh run list -R "$REPO" --workflow=release-debian.yml --branch "$TAG" \
    --json status,conclusion,url --limit 5 2>/dev/null || echo '[]')"

  if [ "$(jq 'length' <<<"$runs_json")" -eq 0 ]; then
    add_row "release-debian.yml" "PENDING" "no run found for ${TAG} yet (push:tags trigger may not have fired)"
    return
  fi

  local status conclusion url
  status="$(jq -r '.[0].status' <<<"$runs_json")"
  conclusion="$(jq -r '.[0].conclusion' <<<"$runs_json")"
  url="$(jq -r '.[0].url' <<<"$runs_json")"

  if [ "$status" != "completed" ]; then
    add_row "release-debian.yml" "PENDING" "run in progress (status=${status}) — ${url}"
  elif [ "$conclusion" != "success" ]; then
    add_row "release-debian.yml" "FAIL" "run completed with conclusion=${conclusion} — ${url}"
  else
    add_row "release-debian.yml" "FAIL" "run succeeded but no .deb asset found on the release (contract drift) — ${url}"
  fi
}

# ---------------------------------------------------------------------------
# Target 3 — docker-build-push.yml (2 images, 2 registries; no artifact
# lands on the Release object, so this target can ONLY be checked via run
# + per-job conclusion, never via release assets).
# ---------------------------------------------------------------------------
check_docker_build_push() {
  local runs_json
  runs_json="$(gh run list -R "$REPO" --workflow=docker-build-push.yml --branch "$TAG" --event push \
    --json databaseId,status,conclusion,url --limit 5 2>/dev/null || echo '[]')"

  if [ "$(jq 'length' <<<"$runs_json")" -eq 0 ]; then
    add_row "docker-build-push.yml" "PENDING" "no push-triggered run found for ${TAG} yet"
    return
  fi

  local run_id status conclusion url
  run_id="$(jq -r '.[0].databaseId' <<<"$runs_json")"
  status="$(jq -r '.[0].status' <<<"$runs_json")"
  conclusion="$(jq -r '.[0].conclusion' <<<"$runs_json")"
  url="$(jq -r '.[0].url' <<<"$runs_json")"

  if [ "$status" != "completed" ]; then
    add_row "docker-build-push.yml" "PENDING" "run in progress (status=${status}) — ${url}"
    return
  fi

  # Do NOT early-return on `conclusion != "success"` here. docker-build-push.yml
  # has two jobs (scan-images, sign-and-sbom) that run AFTER build-controller/
  # build-cli and can fail independently of the actual publish (e.g. cosign's
  # keyless signing requires `id-token: write`, which this workflow does not
  # grant -- a real, reproducing gap, not hypothetical). A failure in either
  # downstream job flips the overall run conclusion to "failure" even though
  # the images were genuinely built and pushed. Always check the two publish
  # jobs' own conclusions directly; only fall back to the overall run
  # conclusion when the jobs API returns nothing at all (a workflow-file-level
  # parse failure, e.g. the pre-PR#441 now() bug, where no job -- publish or
  # otherwise -- was ever scheduled and there is no finer-grained signal).
  local jobs_json
  jobs_json="$(gh api "repos/$REPO/actions/runs/${run_id}/jobs" --jq '[.jobs[] | {name, conclusion}]' 2>/dev/null || echo '[]')"

  if [ "$(jq 'length' <<<"$jobs_json")" -eq 0 ]; then
    add_row "docker-build-push.yml" "FAIL" "run completed with conclusion=${conclusion}, zero jobs scheduled (workflow file issue) — ${url}"
    return
  fi

  local controller_ok cli_ok
  controller_ok="$(jq -r '[.[] | select(.name=="Build Controller Docker Image")][0].conclusion // "missing"' <<<"$jobs_json")"
  cli_ok="$(jq -r '[.[] | select(.name=="Build CLI Docker Image")][0].conclusion // "missing"' <<<"$jobs_json")"

  if [ "$controller_ok" = "success" ] && [ "$cli_ok" = "success" ]; then
    add_row "docker-build-push.yml" "PASS" "both images built (controller=success, cli=success; overall run conclusion=${conclusion} -- downstream scan/sign jobs may differ and do not affect this verdict) — ${url}"
  else
    add_row "docker-build-push.yml" "FAIL" "controller job=${controller_ok}, cli job=${cli_ok} — ${url}"
  fi
}

# ---------------------------------------------------------------------------
# Target 4 — homebrew-release.yml (bumps version in an EXTERNAL tap repo's
# Formula/ggen.rb; the only proof of success is that external file, since
# this run never touches the ggen release object).
# ---------------------------------------------------------------------------
check_homebrew() {
  local formula version_line tap_version
  formula="$(gh api repos/seanchatmangpt/homebrew-tap/contents/Formula/ggen.rb --jq '.content' 2>/dev/null | base64 -d 2>/dev/null || true)"

  if [ -z "$formula" ]; then
    add_row "homebrew-release.yml" "PENDING" "could not read seanchatmangpt/homebrew-tap Formula/ggen.rb (transient API error, or tap unreachable)"
    return
  fi

  # [[:space:]]* (POSIX bracket class) rather than \s — BSD grep/sed (macOS,
  # used to develop and locally validate this script) do not support the \s
  # GNU/PCRE shorthand in basic or extended regex mode.
  version_line="$(grep -m1 '^[[:space:]]*version ' <<<"$formula" || true)"
  tap_version="$(sed -E 's/^[[:space:]]*version "([^"]+)".*/\1/' <<<"$version_line")"

  if [ "$tap_version" = "$VERSION" ]; then
    add_row "homebrew-release.yml" "PASS" "tap formula version ${tap_version} matches ${TAG}"
    return
  fi

  local runs_json
  runs_json="$(gh run list -R "$REPO" --workflow=homebrew-release.yml --branch "$TAG" \
    --json status,conclusion,url --limit 5 2>/dev/null || echo '[]')"

  if [ "$(jq 'length' <<<"$runs_json")" -eq 0 ]; then
    add_row "homebrew-release.yml" "PENDING" "tap formula still at ${tap_version:-<unreadable>}; no run found for ${TAG} yet (release:published webhook is suppressed for GITHUB_TOKEN-created releases — see PR #441)"
    return
  fi

  local status conclusion url
  status="$(jq -r '.[0].status' <<<"$runs_json")"
  conclusion="$(jq -r '.[0].conclusion' <<<"$runs_json")"
  url="$(jq -r '.[0].url' <<<"$runs_json")"

  if [ "$status" != "completed" ]; then
    add_row "homebrew-release.yml" "PENDING" "run in progress (status=${status}) — ${url}"
  elif [ "$conclusion" != "success" ]; then
    add_row "homebrew-release.yml" "FAIL" "run completed with conclusion=${conclusion} — ${url}"
  else
    add_row "homebrew-release.yml" "FAIL" "run succeeded but tap formula still shows ${tap_version:-<unreadable>}, expected ${VERSION} (contract drift) — ${url}"
  fi
}

check_release_binaries
check_release_debian
check_docker_build_push
check_homebrew

# ---------------------------------------------------------------------------
# Render
# ---------------------------------------------------------------------------
echo "## Partial-Publish Detection — ${TAG}"
echo ""
echo "Repo: \`${REPO}\` · Checked: $(date -u +"%Y-%m-%dT%H:%M:%SZ")"
echo ""
echo "| Target | Status | Detail |"
echo "|--------|--------|--------|"
for row in "${ROWS[@]}"; do
  IFS='|' read -r target status detail <<<"$row"
  case "$status" in
    PASS) icon="PASS" ;;
    PENDING) icon="PENDING" ;;
    FAIL) icon="FAIL" ;;
    *) icon="$status" ;;
  esac
  echo "| ${target} | ${icon} | ${detail} |"
done
echo ""
echo "**Overall: ${OVERALL}**"
echo ""
if [ "$OVERALL" = "FAIL" ]; then
  echo "One or more release targets definitively did not publish for ${TAG}. This is a read-only report — no rollback or retry was attempted; a human must decide the remediation."
elif [ "$OVERALL" = "PENDING" ]; then
  echo "All targets are either passing or still in flight for ${TAG}. Re-run after the in-flight workflows complete for a final verdict."
else
  echo "All 4 correlated release targets confirmed published for ${TAG}."
fi

if [ "$OVERALL" = "FAIL" ]; then
  exit 1
fi
exit 0
