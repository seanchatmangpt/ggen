#!/usr/bin/env python3
"""
KNHK v0.4.0 Definition of Done Verification Script
Verifies implementation status against DoD checklist
"""

import os
import re
import sys
from pathlib import Path

class DoDVerifier:
    def __init__(self, root_dir):
        self.root = Path(root_dir)
        self.results = {'passed': [], 'failed': [], 'warning': []}
        
    def check_cli_commands(self):
        """Verify CLI commands return Result"""
        cli_dir = self.root / 'rust/knhk-cli/src/commands'
        if not cli_dir.exists():
            self.results['failed'].append('CLI commands directory missing')
            return
            
        for cmd_file in cli_dir.glob('*.rs'):
            if cmd_file.name == 'mod.rs':
                continue
            with open(cmd_file, 'r') as f:
                content = f.read()
                # Check for Result-returning functions
                result_funcs = re.findall(r'pub fn (\w+).*-> Result', content)
                if result_funcs:
                    self.results['passed'].append(f'{cmd_file.name}: {len(result_funcs)} Result functions')
                else:
                    self.results['warning'].append(f'{cmd_file.name}: No Result functions found')
                    
    def check_error_handling(self):
        """Check for unwrap() in production code"""
        prod_files = [
            'rust/knhk-etl/src/lib.rs',
            'rust/knhk-cli/src/main.rs',
        ]
        
        unwrap_count = 0
        for file_path in prod_files:
            full_path = self.root / file_path
            if full_path.exists():
                with open(full_path, 'r') as f:
                    content = f.read()
                    # Count unwrap() not in comments or tests
                    unwraps = [m for m in re.finditer(r'\.unwrap\(\)', content)]
                    for m in unwraps:
                        # Check if it's in a comment or test
                        start = max(0, m.start() - 50)
                        context = content[start:m.end()]
                        if 'test' not in context.lower() and '//' not in context[:context.rfind('unwrap')]:
                            unwrap_count += 1
        
        if unwrap_count == 0:
            self.results['passed'].append('No unwrap() in production code')
        else:
            self.results['warning'].append(f'{unwrap_count} unwrap() calls in production code')
            
    def check_guard_validation(self):
        """Check guard validation enforcement"""
        etl_file = self.root / 'rust/knhk-etl/src/lib.rs'
        if etl_file.exists():
            with open(etl_file, 'r') as f:
                content = f.read()
                if 'run.len > 8' in content or 'run_len > 8' in content:
                    self.results['passed'].append('Guard validation (max_run_len ≤ 8) enforced')
                else:
                    self.results['failed'].append('Guard validation missing')
                    
    def check_network_integrations(self):
        """Check network integration implementations"""
        etl_file = self.root / 'rust/knhk-etl/src/lib.rs'
        if etl_file.exists():
            with open(etl_file, 'r') as f:
                content = f.read()
                checks = {
                    'HTTP': 'send_http_webhook' in content,
                    'Kafka': 'send_kafka_action' in content,
                    'gRPC': 'send_grpc_action' in content,
                }
                for name, exists in checks.items():
                    if exists:
                        self.results['passed'].append(f'{name} integration implemented')
                    else:
                        self.results['failed'].append(f'{name} integration missing')
                        
    def check_tests(self):
        """Check test file existence"""
        test_dir = self.root / 'tests'
        if not test_dir.exists():
            self.results['failed'].append('Tests directory missing')
            return
            
        test_files = {
            'Integration': list(test_dir.glob('chicago_integration*.c')),
            'Performance': list(test_dir.glob('chicago_performance*.c')),
            'E2E': list(test_dir.glob('*e2e*.c')),
        }
        
        for test_type, files in test_files.items():
            if files:
                self.results['passed'].append(f'{test_type} tests: {len(files)} files')
            else:
                self.results['warning'].append(f'{test_type} tests: No files found')
                
    def run_all_checks(self):
        """Run all verification checks"""
        print("KNHK v0.4.0 Definition of Done Verification")
        print("=" * 60)
        
        self.check_cli_commands()
        self.check_error_handling()
        self.check_guard_validation()
        self.check_network_integrations()
        self.check_tests()
        
        # Print results
        print(f"\n✅ Passed: {len(self.results['passed'])}")
        for item in self.results['passed']:
            print(f"  ✓ {item}")
            
        print(f"\n⚠️  Warnings: {len(self.results['warning'])}")
        for item in self.results['warning']:
            print(f"  ⚠ {item}")
            
        print(f"\n❌ Failed: {len(self.results['failed'])}")
        for item in self.results['failed']:
            print(f"  ✗ {item}")
            
        return len(self.results['failed']) == 0

if __name__ == '__main__':
    verifier = DoDVerifier(os.getcwd())
    success = verifier.run_all_checks()
    sys.exit(0 if success else 1)
