#!/usr/bin/env bash
# git-to-xes.sh — Convert git history to XES event log
#
# Usage: ./git-to-xes.sh [git-log-args...]
#   Example: ./git-to-xes.sh --since="2026-01-01" --author="sean"
#   Example: ./git-to-xes.sh -20
#   Output: XES XML to stdout (pipe to file: ./git-to-xes.sh > log.xes)
#
# Event types detected from commit messages:
#   commit     — Standard commit
#   test-run   — Commit references test execution (test, assert, TDD, RED, GREEN)
#   merge      — Merge commit
#   review     — Commit references code review (review, PR, pull request)
#   deploy     — Commit references deployment (deploy, release, version)
#
# XES standard: https://xes-standard.org/

set -euo pipefail

# Default git log arguments
GIT_ARGS=("${@:---all}")

# Print XES header
print_xes_header() {
    cat <<'XES_HEADER'
<?xml version="1.0" encoding="UTF-8"?>
<log xes.version="2.0" xes.features="nested-attributes" xmlns="http://www.xes-standard.org/">
  <extension name="Concept" prefix="concept" uri="http://www.xes-standard.org/concept.xesext"/>
  <extension name="Time" prefix="time" uri="http://www.xes-standard.org/time.xesext"/>
  <extension name="Organizational" prefix="org" uri="http://www.xes-standard.org/org.xesext"/>
  <extension name="Lifecycle" prefix="lifecycle" prefix="lifecycle" uri="http://www.xes-standard.org/lifecycle.xesext"/>
  <global scope="trace">
    <string key="concept:name" value="case"/>
  </global>
  <global scope="event">
    <string key="concept:name" value="event"/>
    <string key="lifecycle:transition" value="complete"/>
  </global>
XES_HEADER
}

# Print XES footer
print_xes_footer() {
    cat <<'XES_FOOTER'
</log>
XES_FOOTER
}

# XML-escape a string for XES attribute values
xml_escape() {
    local str="$1"
    str="${str//&/&amp;}"
    str="${str//</&lt;}"
    str="${str//>/&gt;}"
    str="${str//\"/&quot;}"
    str="${str//\'/&apos;}"
    printf '%s' "$str"
}

# Detect event type from commit message
detect_event_type() {
    local msg="$1"
    msg_lower="$(echo "$msg" | tr '[:upper:]' '[:lower:]')"

    if echo "$msg_lower" | grep -qE '(deploy|release|version|publish|ship)'; then
        echo "deploy"
    elif echo "$msg_lower" | grep -qE '(merge|pull.?request|pr)'; then
        echo "review"
    elif echo "$msg_lower" | grep -qE '(test|tdd|red|green|assert|_test\.|_test\.exs|_test\.rs|_test\.go)'; then
        echo "test-run"
    elif git log -1 --format="%P" "$2" | grep -q ' '; then
        echo "merge"
    else
        echo "commit"
    fi
}

# Detect commit message convention compliance
detect_convention_compliance() {
    local msg="$1"
    if echo "$msg" | grep -qE '^(feat|fix|docs|refactor|test|chore|perf|ci|build|style)\('; then
        echo "true"
    else
        echo "false"
    fi
}

# Detect DoD keywords in commit message
detect_dod_keywords() {
    local msg="$1"
    local keywords=""

    # Testing
    if echo "$msg" | grep -qiE '(test|tdd|red|green|assert)'; then
        keywords="${keywords}testing,"
    fi

    # OTEL / Verification
    if echo "$msg" | grep -qiE '(otel|opentelemetry|span|trace|telemetry|jaeger)'; then
        keywords="${keywords}verification,"
    fi

    # Weaver
    if echo "$msg" | grep -qiE '(weaver|semconv|registry.?check|spans\.yaml)'; then
        keywords="${keywords}weaver,"
    fi

    # WvdA Soundness
    if echo "$msg" | grep -qiE '(wvda|deadlock|liveness|bounded|timeout|soundness)'; then
        keywords="${keywords}soundness,"
    fi

    # Armstrong
    if echo "$msg" | grep -qiE '(armstrong|supervision|supervisor|let.?it.?crash|restart)'; then
        keywords="${keywords}armstrong,"
    fi

    # Compiler
    if echo "$msg" | grep -qiE '(clippy|warnings.?as.?errors|go.?vet|cargo.?fmt|mix.?format|compile)'; then
        keywords="${keywords}compiler,"
    fi

    # Remove trailing comma
    keywords="${keywords%,}"
    if [ -z "$keywords" ]; then
        keywords="none"
    fi
    echo "$keywords"
}

# Main: parse git log and emit XES
main() {
    print_xes_header

    # Use null-delimited git log format for reliable parsing
    # %H = hash, %an = author name, %ae = author email, %aI = author date ISO 8601
    # %s = subject, %b = body
    git log --format="%H%x00%an%x00%ae%x00%aI%x00%s%x00%b%x01" "${GIT_ARGS[@]}" 2>/dev/null | {
        local case_id=""
        local first_event=true

        while IFS= read -r -d '' line; do
            # Skip empty lines
            [ -z "$line" ] && continue

            # Split by null bytes
            IFS=$'\x00' read -r hash author email date subject body <<< "$line"

            # Skip if hash is empty
            [ -z "$hash" ] && continue

            # Generate case ID from first 8 chars of hash (each commit is a case)
            case_id="${hash:0:8}"

            # Detect event metadata
            event_type=$(detect_event_type "$subject" "$hash")
            convention=$(detect_convention_compliance "$subject")
            dod_keywords=$(detect_dod_keywords "$subject")

            # Count files changed
            files_changed=$(git diff-tree --no-commit-id --name-only -r "$hash" 2>/dev/null | wc -l | tr -d ' ')

            # Close previous trace if not first event
            if [ "$first_event" = false ]; then
                echo "  </trace>"
            fi
            first_event=false

            # Print trace
            echo "  <trace>"
            echo "    <string key=\"concept:name\" value=\"${case_id}\"/>"
            echo "    <event>"
            echo "      <string key=\"concept:name\" value=\"${event_type}\"/>"
            echo "      <string key=\"lifecycle:transition\" value=\"complete\"/>"
            echo "      <string key=\"time:timestamp\" value=\"${date}\"/>"
            echo "      <string key=\"org:resource\" value=\"$(xml_escape "$author")\"/>"
            echo "      <string key=\"org:role\" value=\"developer\"/>"
            echo "      <string key=\"commit:hash\" value=\"${hash}\"/>"
            echo "      <string key=\"commit:subject\" value=\"$(xml_escape "$subject")\"/>"
            echo "      <string key=\"commit:convention\" value=\"${convention}\"/>"
            echo "      <int key=\"commit:files_changed\" value=\"${files_changed}\"/>"
            echo "      <string key=\"dod:keywords\" value=\"${dod_keywords}\"/>"
            echo "    </event>"
        done

        # Close last trace
        if [ "$first_event" = false ]; then
            echo "  </trace>"
        fi
    }

    print_xes_footer
}

main "$@"
