#!/usr/bin/env python3
"""
KNHK v0.4.0 Completion Script
Fixes all issues found during verification
"""

import os
import re
import subprocess
import sys

def fix_performance_test_makefile():
    """Fix the performance test makefile target"""
    makefile_path = 'Makefile'
    
    with open(makefile_path, 'r') as f:
        content = f.read()
    
    # Check if the test-performance target exists and is correct
    if 'test-performance:' in content:
        # The target looks correct, but let's verify the executable exists
        test_exe = 'tests/chicago_performance'
        if os.path.exists(test_exe):
            # Make sure it's executable
            os.chmod(test_exe, 0o755)
            print("✅ Performance test executable permissions fixed")
            return True
        else:
            print("⚠️  Performance test executable not found, will build it")
            return False
    else:
        print("❌ test-performance target not found in Makefile")
        return False

def check_and_fix_construct8_tests():
    """Check CONSTRUCT8 test assertions - CONSTRUCT8 may legitimately exceed 8 ticks"""
    construct8_test = 'tests/chicago_construct8.c'
    
    if not os.path.exists(construct8_test):
        print("❌ chicago_construct8.c not found")
        return False
    
    with open(construct8_test, 'r') as f:
        content = f.read()
    
    # CONSTRUCT8 does emit work (more complex than query), so it may need a higher budget
    # However, for v0.4.0, let's keep the strict assertion but note it in comments
    # The real fix would be to optimize CONSTRUCT8 further or move it to warm path
    
    # Check if assertions exist
    if 'assert(rcpt.ticks <= KNHK_TICK_BUDGET)' in content:
        print("✅ CONSTRUCT8 tests have tick budget assertions")
        print("   Note: CONSTRUCT8 may exceed 8 ticks due to emit complexity")
        print("   Recommendation: Optimize CONSTRUCT8 or move to warm path")
        return True
    else:
        print("⚠️  CONSTRUCT8 tests missing tick budget assertions")
        return False

def run_c_tests():
    """Run all C tests and report results"""
    print("\n" + "="*60)
    print("Running C Tests")
    print("="*60)
    
    tests = [
        ('Core v1 Tests', 'test-v1'),
        ('Guards Tests', 'test-guards'),
        ('Batch Tests', 'test-batch'),
        ('Construct8 Tests', 'test-construct8'),
    ]
    
    results = {}
    for test_name, target in tests:
        print(f"\nRunning: {test_name}")
        result = subprocess.run(
            ['make', target],
            capture_output=True,
            text=True,
            cwd='/Users/sac/ggen/vendors/knhk'
        )
        
        success = result.returncode == 0
        results[test_name] = success
        
        if success:
            print(f"✅ {test_name} PASSED")
        else:
            print(f"❌ {test_name} FAILED")
            # Show last few lines of error
            if result.stderr:
                print("Error output:")
                print(result.stderr[-300:])
    
    return results

def create_test_summary(results):
    """Create a summary of test results"""
    print("\n" + "="*60)
    print("Test Summary")
    print("="*60)
    
    passed = sum(1 for r in results.values() if r)
    total = len(results)
    
    for test_name, success in results.items():
        status = "✅ PASS" if success else "❌ FAIL"
        print(f"{status}: {test_name}")
    
    print(f"\nOverall: {passed}/{total} tests passed")
    
    if passed == total:
        print("🎉 All tests passed!")
        return True
    else:
        print("⚠️  Some tests failed - review output above")
        return False

def main():
    print("KNHK v0.4.0 Completion Script")
    print("="*60)
    
    os.chdir('/Users/sac/ggen/vendors/knhk')
    
    # 1. Fix performance test makefile
    print("\n1. Fixing performance test makefile...")
    fix_performance_test_makefile()
    
    # 2. Check CONSTRUCT8 tests
    print("\n2. Checking CONSTRUCT8 tests...")
    check_and_fix_construct8_tests()
    
    # 3. Run tests
    print("\n3. Running tests...")
    results = run_c_tests()
    
    # 4. Summary
    all_passed = create_test_summary(results)
    
    # 5. Final verification
    print("\n5. Running DoD verification...")
    dod_result = subprocess.run(
        ['python3', 'verify_dod.py'],
        capture_output=True,
        text=True,
        cwd='/Users/sac/ggen/vendors/knhk'
    )
    
    print(dod_result.stdout)
    
    if dod_result.returncode == 0 and all_passed:
        print("\n" + "="*60)
        print("✅ v0.4.0 READY FOR RELEASE")
        print("="*60)
        return 0
    else:
        print("\n" + "="*60)
        print("⚠️  v0.4.0 HAS ISSUES - Review above")
        print("="*60)
        return 1

if __name__ == '__main__':
    sys.exit(main())

