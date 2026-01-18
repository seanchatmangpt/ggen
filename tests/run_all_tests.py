#!/usr/bin/env python3
"""
Comprehensive test runner for academic paper lifecycle → IPO tests.
Runs all test suites and generates comprehensive report.
"""

import subprocess
import json
import sys
from datetime import datetime
from pathlib import Path


class TestRunner:
    """Orchestrates test execution and reporting."""

    def __init__(self):
        self.results = {
            "execution_time": None,
            "total_tests": 0,
            "passed": 0,
            "failed": 0,
            "suites": {},
        }
        self.start_time = datetime.now()

    def run_suite(self, suite_name, test_file):
        """Run a single test suite."""
        print(f"\n{'='*80}")
        print(f"Running: {suite_name}")
        print(f"{'='*80}")

        try:
            result = subprocess.run(
                ["pytest", test_file, "-v", "--tb=short"],
                capture_output=True,
                text=True,
                timeout=300,
            )

            # Parse output
            output = result.stdout + result.stderr
            passed = output.count(" PASSED")
            failed = output.count(" FAILED")

            self.results["suites"][suite_name] = {
                "file": test_file,
                "passed": passed,
                "failed": failed,
                "success_rate": passed / (passed + failed) if (passed + failed) > 0 else 0,
                "status": "PASS" if failed == 0 else "FAIL",
            }

            self.results["passed"] += passed
            self.results["failed"] += failed
            self.results["total_tests"] += passed + failed

            print(f"✅ {suite_name}: {passed} passed, {failed} failed")
            return failed == 0

        except subprocess.TimeoutExpired:
            print(f"❌ {suite_name}: TIMEOUT")
            self.results["suites"][suite_name] = {
                "status": "TIMEOUT",
                "passed": 0,
                "failed": 0,
            }
            return False
        except Exception as e:
            print(f"❌ {suite_name}: ERROR - {e}")
            self.results["suites"][suite_name] = {
                "status": "ERROR",
                "error": str(e),
            }
            return False

    def run_all(self):
        """Run all test suites."""
        print(f"\n{'#'*80}")
        print(f"# COMPREHENSIVE TEST SUITE: Research → IPO Lifecycle")
        print(f"{'#'*80}")
        print(f"Start time: {self.start_time}")

        test_suites = [
            ("Academic Lifecycle", "test_academic_lifecycle.py"),
            ("Business Validation", "test_business_validation.py"),
        ]

        all_passed = True
        for suite_name, test_file in test_suites:
            passed = self.run_suite(suite_name, test_file)
            all_passed = all_passed and passed

        self.results["execution_time"] = str(datetime.now() - self.start_time)

        # Print summary
        self.print_summary()

        return 0 if all_passed else 1

    def print_summary(self):
        """Print comprehensive test summary."""
        print(f"\n{'='*80}")
        print(f"TEST EXECUTION SUMMARY")
        print(f"{'='*80}\n")

        print(f"Total Tests: {self.results['total_tests']}")
        print(f"Passed: {self.results['passed']}")
        print(f"Failed: {self.results['failed']}")

        if self.results['total_tests'] > 0:
            success_rate = self.results['passed'] / self.results['total_tests']
            print(f"Success Rate: {success_rate:.1%}")
        else:
            print(f"Success Rate: N/A (no tests run)")

        print(f"Duration: {self.results['execution_time']}\n")

        print(f"{'Suite':<30} {'Passed':<10} {'Failed':<10} {'Status':<10}")
        print(f"{'-'*60}")

        for suite_name, suite_results in self.results['suites'].items():
            passed = suite_results.get('passed', 0)
            failed = suite_results.get('failed', 0)
            status = suite_results.get('status', 'UNKNOWN')
            print(f"{suite_name:<30} {passed:<10} {failed:<10} {status:<10}")

        print(f"\n{'='*80}\n")


class TestDashboard:
    """Generates test dashboard and reports."""

    @staticmethod
    def generate_html_report(results):
        """Generate HTML test report."""
        html = f"""
<!DOCTYPE html>
<html>
<head>
    <title>Test Execution Dashboard</title>
    <style>
        body {{ font-family: Arial, sans-serif; margin: 20px; }}
        .header {{ background: #2c3e50; color: white; padding: 20px; border-radius: 5px; }}
        .summary {{ display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 20px; margin: 20px 0; }}
        .metric {{ background: #ecf0f1; padding: 15px; border-radius: 5px; }}
        .metric-value {{ font-size: 28px; font-weight: bold; color: #2c3e50; }}
        .metric-label {{ font-size: 14px; color: #7f8c8d; }}
        table {{ width: 100%; border-collapse: collapse; margin-top: 20px; }}
        th, td {{ padding: 12px; text-align: left; border-bottom: 1px solid #ddd; }}
        th {{ background: #34495e; color: white; }}
        .pass {{ color: green; }}
        .fail {{ color: red; }}
        .status-pass {{ background: #d5f4e6; }}
        .status-fail {{ background: #fadbd8; }}
    </style>
</head>
<body>
    <div class="header">
        <h1>Test Execution Dashboard</h1>
        <p>Academic Paper Lifecycle → Conference → Startup → Unicorn → IPO</p>
    </div>

    <div class="summary">
        <div class="metric">
            <div class="metric-value">{results['total_tests']}</div>
            <div class="metric-label">Total Tests</div>
        </div>
        <div class="metric">
            <div class="metric-value pass">{results['passed']}</div>
            <div class="metric-label">Passed</div>
        </div>
        <div class="metric">
            <div class="metric-value fail">{results['failed']}</div>
            <div class="metric-label">Failed</div>
        </div>
    </div>

    <h2>Success Rate by Suite</h2>
    <table>
        <tr>
            <th>Test Suite</th>
            <th>Passed</th>
            <th>Failed</th>
            <th>Success Rate</th>
            <th>Status</th>
        </tr>
"""
        for suite_name, suite_results in results['suites'].items():
            passed = suite_results.get('passed', 0)
            failed = suite_results.get('failed', 0)
            success_rate = suite_results.get('success_rate', 0)
            status = suite_results.get('status', 'UNKNOWN')
            status_class = 'status-pass' if status == 'PASS' else 'status-fail'

            html += f"""
        <tr class="{status_class}">
            <td>{suite_name}</td>
            <td class="pass">{passed}</td>
            <td class="fail">{failed}</td>
            <td>{success_rate:.1%}</td>
            <td>{status}</td>
        </tr>
"""

        html += """
    </table>

    <h2>Test Categories</h2>
    <ul>
        <li><strong>Academic Lifecycle</strong>: Paper creation, validation, generation, compilation</li>
        <li><strong>Conference Season</strong>: Multi-venue submission, review cycles, acceptance</li>
        <li><strong>Startup Metrics</strong>: Revenue growth, customer acquisition, Series funding</li>
        <li><strong>Unicorn Milestone</strong>: $1B+ valuation achievement, competitive moat</li>
        <li><strong>IPO Readiness</strong>: S-1 filing, public market validation, post-IPO success</li>
    </ul>

    <h2>Success Criteria</h2>
    <table>
        <tr>
            <th>Phase</th>
            <th>Duration</th>
            <th>Success Rate Target</th>
            <th>Key Metric</th>
        </tr>
        <tr>
            <td>Academic (Month 0)</td>
            <td>3 months</td>
            <td>95%+</td>
            <td>Published papers (5+)</td>
        </tr>
        <tr>
            <td>Startup (Month 3)</td>
            <td>12 months</td>
            <td>90%+</td>
            <td>$1M ARR, 3+ customers</td>
        </tr>
        <tr>
            <td>Growth (Month 12)</td>
            <td>24 months</td>
            <td>85%+</td>
            <td>$15M ARR, 75+ customers</td>
        </tr>
        <tr>
            <td>Unicorn (Month 36)</td>
            <td>24 months</td>
            <td>80%+</td>
            <td>$1B+ valuation, 500+ customers</td>
        </tr>
        <tr>
            <td>IPO (Month 60)</td>
            <td>Ongoing</td>
            <td>75%+</td>
            <td>$100M+ ARR, public company</td>
        </tr>
    </table>

    <footer style="margin-top: 40px; padding-top: 20px; border-top: 1px solid #ddd; color: #7f8c8d;">
        <p>Generated: {datetime.now().isoformat()}</p>
        <p>Duration: {results['execution_time']}</p>
    </footer>
</body>
</html>
"""
        return html

    @staticmethod
    def save_report(html_content, output_file="test_report.html"):
        """Save HTML report to file."""
        with open(output_file, 'w') as f:
            f.write(html_content)
        print(f"✅ Report saved to: {output_file}")


def main():
    """Main test execution."""
    runner = TestRunner()
    exit_code = runner.run_all()

    # Generate HTML report
    dashboard = TestDashboard()
    html_report = dashboard.generate_html_report(runner.results)
    dashboard.save_report(html_report, "tests/test_report.html")

    return exit_code


if __name__ == "__main__":
    sys.exit(main())
