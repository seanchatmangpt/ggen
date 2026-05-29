#!/usr/bin/env python3
import subprocess
import json
import os
import sys
import tempfile

BINARY = "./tools/truth-gate/target/debug/truth-gate"

def run_truth_gate(stdin_data):
    p = subprocess.Popen(
        [BINARY],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True
    )
    stdout, stderr = p.communicate(input=json.dumps(stdin_data))
    return p.returncode, stdout, stderr

def main():
    # Build truth-gate first
    print("Building truth-gate...")
    subprocess.check_call(["cargo", "build", "--manifest-path", "tools/truth-gate/Cargo.toml"])

    if not os.path.exists(BINARY):
        print(f"Error: Compiled binary not found at {BINARY}")
        sys.exit(1)

    print("\n--- Running Verification Harness for truth-gate ---\n")
    tests_passed = 0
    tests_failed = 0

    def assert_blocked(stdin_data, desc):
        nonlocal tests_passed, tests_failed
        code, stdout, stderr = run_truth_gate(stdin_data)
        if code == 2:
            print(f"✅ PASSED: {desc} (Blocked as expected, exit code {code})")
            tests_passed += 1
        else:
            print(f"❌ FAILED: {desc} (Expected exit code 2, got {code})")
            print(f"Stdout: {stdout}")
            print(f"Stderr: {stderr}")
            tests_failed += 1

    def assert_allowed(stdin_data, desc):
        nonlocal tests_passed, tests_failed
        code, stdout, stderr = run_truth_gate(stdin_data)
        if code == 0:
            print(f"✅ PASSED: {desc} (Allowed as expected, exit code {code})")
            tests_passed += 1
        else:
            print(f"❌ FAILED: {desc} (Expected exit code 0, got {code})")
            print(f"Stdout: {stdout}")
            print(f"Stderr: {stderr}")
            tests_failed += 1

    # Test 1: BeforeTool hook with replace_file_content introducing unittest.mock
    stdin_t1 = {
        "hook_event_name": "BeforeTool",
        "tool_name": "replace_file_content",
        "tool_input": {
            "TargetFile": "tests/chicago_tdd/test_sample.py",
            "ReplacementContent": "from unittest.mock import MagicMock"
        }
    }
    assert_blocked(stdin_t1, "BeforeTool replace_file_content introducing mock import")

    # Test 2: BeforeTool hook with clean replace_file_content
    stdin_t2 = {
        "hook_event_name": "BeforeTool",
        "tool_name": "replace_file_content",
        "tool_input": {
            "TargetFile": "tests/chicago_tdd/test_sample.py",
            "ReplacementContent": "class RealBoundaryVerifier:\n    pass"
        }
    }
    assert_allowed(stdin_t2, "BeforeTool replace_file_content with clean code")

    # Test 3: BeforeTool hook with multi_replace_file_content introducing TODO
    stdin_t3 = {
        "hook_event_name": "BeforeTool",
        "tool_name": "multi_replace_file_content",
        "tool_input": {
            "TargetFile": "tests/chicago_tdd/test_sample.py",
            "ReplacementChunks": [
                {
                    "ReplacementContent": "# TODO: implement actual tests"
                }
            ]
        }
    }
    assert_blocked(stdin_t3, "BeforeTool multi_replace_file_content introducing TODO")

    # Test 4: BeforeTool hook with write_to_file introducing FakeMCP
    stdin_t4 = {
        "hook_event_name": "BeforeTool",
        "tool_name": "write_to_file",
        "tool_input": {
            "TargetFile": "tests/chicago_tdd/test_sample.py",
            "CodeContent": "mcp_client = FakeMCP()"
        }
    }
    assert_blocked(stdin_t4, "BeforeTool write_to_file introducing FakeMCP")

    # Test 5: AfterTool hook with a file containing a mock class definition
    with tempfile.TemporaryDirectory() as tmpdir:
        test_file = os.path.join(tmpdir, "tests", "test_bad_file.py")
        os.makedirs(os.path.dirname(test_file), exist_ok=True)
        with open(test_file, "w") as f:
            f.write("class MockTraceValidator:\n    pass")

        stdin_t5 = {
            "hook_event_name": "AfterTool",
            "tool_name": "replace_file_content",
            "tool_input": {
                "TargetFile": test_file
            }
        }
        assert_blocked(stdin_t5, "AfterTool with file containing Mock class definition")

    # Test 6: ConfigChange removing truth-gate hook
    stdin_t6 = {
        "hook_event_name": "ConfigChange",
        "file_path": ".gemini/settings.json",
        "tool_input": {
            "content": json.dumps({
                "hooks": {
                    "BeforeTool": []
                }
            })
        }
    }
    assert_blocked(stdin_t6, "ConfigChange disabling BeforeTool hooks")

    print(f"\nSummary: {tests_passed} passed, {tests_failed} failed.")
    if tests_failed > 0:
        sys.exit(1)
    else:
        sys.exit(0)

if __name__ == "__main__":
    main()
