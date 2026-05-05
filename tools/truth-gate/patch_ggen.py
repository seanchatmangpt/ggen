import json
import os

path = os.path.expanduser('~/ggen/.claude/settings.json')
with open(path, 'r') as f:
    data = json.load(f)

hooks = {
    "PreToolUse": [
      {
        "matcher": "Edit|Write",
        "hooks": [
          {
            "type": "command",
            "command": "./tools/truth-gate/target/release/truth-gate",
            "timeout": 10
          }
        ]
      }
    ],
    "PostToolUse": [
      {
        "matcher": "Edit|Write",
        "hooks": [
          {
            "type": "command",
            "command": "./tools/truth-gate/target/release/truth-gate",
            "timeout": 10
          }
        ]
      }
    ],
    "ConfigChange": [
      {
        "hooks": [
          {
            "type": "command",
            "command": "./tools/truth-gate/target/release/truth-gate",
            "timeout": 10
          }
        ]
      }
    ]
}

data['hooks'] = hooks

with open(path, 'w') as f:
    json.dump(data, f, indent=2)
