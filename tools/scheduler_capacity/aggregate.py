"""Experimental scheduler-capacity aggregation primitives."""

def summarize(entries):
    outcomes = {}
    tools = {}
    durable_commits = 0
    code_lines = 0
    for entry in entries:
        outcomes[entry["status"]] = outcomes.get(entry["status"], 0) + 1
        tool = entry["tool"]
        tools[tool] = tools.get(tool, 0) + 1
        durable_commits += 1 if entry.get("commit_sha") else 0
        code_lines += int(entry.get("code_lines", 0))
    return {
        "highest_cycle": max((e["cycle"] for e in entries), default=0),
        "outcomes": outcomes,
        "tool_operations": tools,
        "durable_commits": durable_commits,
        "code_lines": code_lines,
    }

def lower_bound(previous, entries):
    return max(previous, max((e["cycle"] for e in entries), default=0))
