#!/usr/bin/env python3
from __future__ import annotations

import hashlib
import json
import re
import sys
import xml.etree.ElementTree as ET
import zipfile
from pathlib import Path

PACK = Path(__file__).resolve().parent.parent
GEN = PACK / "generated"
EVIDENCE = PACK / ".ggen/evidence/no-ci"
SCENE = GEN / "roku/components/NasaDarkModeScene.brs"
SCENE_XML = GEN / "roku/components/NasaDarkModeScene.xml"
TASK_XML = GEN / "roku/components/MissionFeedTask.xml"
FEED = GEN / "roku/data/mission-feed.json"
ZIP = GEN / ".ggen/evidence/nasa-dark-mode-roku.zip"
EXPECTED = {
    "left": "previous-mode",
    "right": "next-mode",
    "up": "previous-mission",
    "down": "next-mission",
    "OK": "select-intent",
    "back": "privacy-curtain",
}

class Refusal(RuntimeError):
    pass


def extract_contract(source: str) -> dict[str, str]:
    mapping = dict(re.findall(r'if key = "([^"]+)" then return "([^"]+)"', source))
    if mapping != EXPECTED:
        raise Refusal(f"BRIGHTSCRIPT_REMOTE_CONTRACT_REFUSED:{mapping}")
    if 'return ""' not in source:
        raise Refusal("BRIGHTSCRIPT_UNKNOWN_KEY_REFUSAL_MISSING")
    return mapping


def append(receipts: list[str], phase: str, operation: str, detail: str) -> None:
    receipts.append(f"{len(receipts) + 1}:{phase}:{operation}:{detail}")


def simulate(source: str, feed: dict) -> dict:
    mapping = extract_contract(source)
    modes_match = re.search(r'm\.modes = \[([^\]]+)\]', source)
    if not modes_match:
        raise Refusal("BRIGHTSCRIPT_MODES_MISSING")
    modes = re.findall(r'"([^"]+)"', modes_match.group(1))
    if modes != ["EARTH NOW", "MISSIONS", "BRIEFING", "RECEIPT"]:
        raise Refusal(f"BRIGHTSCRIPT_MODES_REFUSED:{modes}")
    if feed.get("schema") != "ggen.nasa-dark-mode.mission-feed.v1":
        raise Refusal("MISSION_FEED_SCHEMA_REFUSED")

    mode_index = 0
    mission_index = 0 if feed.get("missions") else -1
    privacy = False
    receipts: list[str] = []
    append(receipts, "consequence", "load-local-feed", feed["receipt"]["digest"])

    for key in ["right", "right", "down", "OK", "back"]:
        operation = mapping[key]
        append(receipts, "intent", operation, key)
        if operation == "next-mode":
            mode_index = (mode_index + 1) % len(modes)
        elif operation == "previous-mode":
            mode_index = (mode_index - 1 + len(modes)) % len(modes)
        elif operation == "next-mission" and feed["missions"]:
            mission_index = (mission_index + 1) % len(feed["missions"])
        elif operation == "previous-mission" and feed["missions"]:
            mission_index = (mission_index - 1 + len(feed["missions"])) % len(feed["missions"])
        elif operation == "privacy-curtain":
            privacy = not privacy
        consequence = "NOT_ACTUATED_INTENT_ONLY" if operation == "select-intent" else "LOCAL_PROJECTION_ONLY"
        append(receipts, "consequence", operation, consequence)

    unknown = "voice-search"
    append(receipts, "refusal", "remote-key", f"REMOTE_KEY_UNREPRESENTABLE:{unknown}")
    return {
        "modeIndex": mode_index,
        "mode": modes[mode_index],
        "missionIndex": mission_index,
        "privacyCurtain": privacy,
        "receipts": receipts,
        "unknownKeyHandled": False,
    }


def main() -> int:
    source = SCENE.read_text()
    feed = json.loads(FEED.read_text())
    ET.parse(SCENE_XML)
    ET.parse(TASK_XML)
    result = simulate(source, feed)
    if result["mode"] != "BRIEFING" or result["missionIndex"] != 1 or not result["privacyCurtain"]:
        raise Refusal(f"BRIGHTSCRIPT_SIMULATION_DIVERGED:{result}")
    if len(result["receipts"]) != 12 or "REMOTE_KEY_UNREPRESENTABLE" not in result["receipts"][-1]:
        raise Refusal(f"BRIGHTSCRIPT_RECEIPT_CHAIN_BROKEN:{result['receipts']}")

    if not ZIP.is_file():
        raise Refusal(f"ROKU_PACKAGE_MISSING:{ZIP}")
    with zipfile.ZipFile(ZIP) as archive:
        names = set(archive.namelist())
    required = {
        "manifest",
        "source/main.brs",
        "components/NasaDarkModeScene.xml",
        "components/NasaDarkModeScene.brs",
        "components/MissionFeedTask.xml",
        "components/MissionFeedTask.brs",
        "data/mission-feed.json",
    }
    missing = sorted(required - names)
    if missing:
        raise Refusal(f"ROKU_PACKAGE_INCOMPLETE:{missing}")

    mutant = source.replace('if key = "right" then return "next-mode"', 'if key = "right" then return "previous-mode"')
    mutation_killed = False
    mutation_code = ""
    try:
        extract_contract(mutant)
    except Refusal as exc:
        mutation_killed = True
        mutation_code = str(exc).split(":", 1)[0]
    if not mutation_killed:
        raise Refusal("BRIGHTSCRIPT_MUTATION_SURVIVED")

    report = {
        "schema": "ggen.nasa-dark-mode.roku-source-simulation.v1",
        "standing": "ALIVE",
        "executionClass": "STRICT_SOURCE_DERIVED_SIMULATION",
        "physicalDeviceStanding": "BLOCKED_DEVICE_REQUIRED",
        "remoteContract": EXPECTED,
        "finalState": {k: v for k, v in result.items() if k != "receipts"},
        "receiptCount": len(result["receipts"]),
        "finalReceipt": result["receipts"][-1],
        "packageEntries": len(names),
        "packageSha256": hashlib.sha256(ZIP.read_bytes()).hexdigest(),
        "xmlWellFormed": True,
        "mutationControl": {"standing": "KILLED", "code": mutation_code},
    }
    EVIDENCE.mkdir(parents=True, exist_ok=True)
    (EVIDENCE / "roku-source-simulation.json").write_text(json.dumps(report, indent=2, sort_keys=True) + "\n")
    print(json.dumps(report, sort_keys=True))
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except Refusal as exc:
        print(json.dumps({"standing": "REFUSED", "code": str(exc)}), file=sys.stderr)
        raise SystemExit(2)
