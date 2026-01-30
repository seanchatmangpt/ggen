#!/usr/bin/env python3

"""
SQLite Database Schema Validation Tests
Purpose: Comprehensive test suite for schema validation using Python
Usage: python3 test-schema.py [database_path]
"""

import sqlite3
import json
import sys
import os
from datetime import datetime, timedelta
from pathlib import Path

class Colors:
    RED = '\033[0;31m'
    GREEN = '\033[0;32m'
    YELLOW = '\033[1;33m'
    BLUE = '\033[0;34m'
    NC = '\033[0m'

class TestRunner:
    def __init__(self, db_path):
        self.db_path = db_path
        self.conn = None
        self.tests_run = 0
        self.tests_passed = 0
        self.tests_failed = 0
        self.failures = []

    def setup(self):
        """Initialize database from schema"""
        schema_file = Path(__file__).parent / "schema.sql"

        # Remove existing database
        if os.path.exists(self.db_path):
            os.remove(self.db_path)

        # Create connection
        self.conn = sqlite3.connect(self.db_path)

        # Enable foreign keys
        self.conn.execute("PRAGMA foreign_keys = ON")

        # Load schema
        with open(schema_file, 'r') as f:
            schema_sql = f.read()

        # Execute schema
        self.conn.executescript(schema_sql)
        self.conn.commit()

        print(f"{Colors.GREEN}✓ Database initialized{Colors.NC}")

    def cleanup(self):
        """Close database connection and cleanup"""
        if self.conn:
            self.conn.close()
        if os.path.exists(self.db_path):
            os.remove(self.db_path)

    def test_case(self, name):
        """Start a test case"""
        self.tests_run += 1
        return f"  [{self.tests_run}] {name}... "

    def test_pass(self, message):
        """Mark test as passed"""
        print(f"{message}{Colors.GREEN}PASS{Colors.NC}")
        self.tests_passed += 1

    def test_fail(self, message, error):
        """Mark test as failed"""
        print(f"{message}{Colors.RED}FAIL{Colors.NC}")
        print(f"        {Colors.RED}Error: {error}{Colors.NC}")
        self.tests_failed += 1
        self.failures.append((message, error))

    def run_tests(self):
        """Run all test suites"""
        print("\n" + "="*50)
        print("SQLite Database Schema Validation Tests")
        print("="*50)

        self.test_schema_structure()
        self.test_columns()
        self.test_indexes()
        self.test_constraints()
        self.test_data_types()
        self.test_json_operations()
        self.test_foreign_keys()
        self.test_views()
        self.test_integration()

        self.print_report()

    def test_schema_structure(self):
        """Test schema structure"""
        print("\n" + "="*50)
        print("Test Suite: Schema Structure")
        print("="*50)

        tables = ["receipts", "agent_memory", "audit_log", "workflow_sessions",
                  "collision_detection", "pipeline_metrics", "slo_violations"]

        cursor = self.conn.cursor()

        for table in tables:
            msg = self.test_case(f"Table '{table}' exists")
            cursor.execute(
                "SELECT COUNT(*) FROM sqlite_master WHERE type='table' AND name=?",
                (table,)
            )
            count = cursor.fetchone()[0]
            if count == 1:
                self.test_pass(msg)
            else:
                self.test_fail(msg, f"Table {table} not found")

    def test_columns(self):
        """Test table columns"""
        print("\n" + "="*50)
        print("Test Suite: Table Columns")
        print("="*50)

        cursor = self.conn.cursor()

        # Test receipts columns
        receipts_cols = ["id", "execution_id", "agent_id", "operation", "status",
                        "timestamp", "created_at", "manifest_hash", "ontology_hash",
                        "files_generated", "files_modified", "duration_ms",
                        "receipt_json", "error_message", "environment"]

        for col in receipts_cols:
            msg = self.test_case(f"receipts.{col} exists")
            cursor.execute("PRAGMA table_info(receipts)")
            cols = [row[1] for row in cursor.fetchall()]
            if col in cols:
                self.test_pass(msg)
            else:
                self.test_fail(msg, f"Column {col} not found")

    def test_indexes(self):
        """Test index creation"""
        print("\n" + "="*50)
        print("Test Suite: Indexes")
        print("="*50)

        cursor = self.conn.cursor()

        msg = self.test_case("Indexes created for receipts")
        cursor.execute(
            "SELECT COUNT(*) FROM sqlite_master WHERE type='index' AND tbl_name='receipts'"
        )
        count = cursor.fetchone()[0]
        if count >= 4:
            self.test_pass(msg)
        else:
            self.test_fail(msg, f"Expected >=4 indexes on receipts, got {count}")

        msg = self.test_case("Total indexes in database")
        cursor.execute(
            "SELECT COUNT(*) FROM sqlite_master WHERE type='index' AND tbl_name NOT LIKE 'sqlite_%'"
        )
        count = cursor.fetchone()[0]
        if count >= 15:
            self.test_pass(msg)
        else:
            self.test_fail(msg, f"Expected >=15 indexes total, got {count}")

    def test_constraints(self):
        """Test constraint validation"""
        print("\n" + "="*50)
        print("Test Suite: Constraints")
        print("="*50)

        cursor = self.conn.cursor()

        # Test UNIQUE constraint
        msg = self.test_case("UNIQUE constraint on execution_id")
        try:
            cursor.execute(
                "INSERT INTO receipts (execution_id, agent_id, operation, status, timestamp, duration_ms) VALUES (?, ?, ?, ?, ?, ?)",
                ("unique-test", "agent1", "validation", "passed", "2026-01-29T00:00:00Z", 100)
            )
            self.conn.commit()

            # Try to insert duplicate
            try:
                cursor.execute(
                    "INSERT INTO receipts (execution_id, agent_id, operation, status, timestamp, duration_ms) VALUES (?, ?, ?, ?, ?, ?)",
                    ("unique-test", "agent2", "validation", "failed", "2026-01-29T00:00:00Z", 200)
                )
                self.conn.commit()
                self.test_fail(msg, "UNIQUE constraint not enforced")
            except sqlite3.IntegrityError:
                self.test_pass(msg)
        except Exception as e:
            self.test_fail(msg, str(e))
        finally:
            # Cleanup
            cursor.execute("DELETE FROM receipts WHERE execution_id='unique-test'")
            self.conn.commit()

        # Test CHECK constraint
        msg = self.test_case("CHECK constraint on status")
        try:
            cursor.execute(
                "INSERT INTO receipts (execution_id, agent_id, operation, status, timestamp, duration_ms) VALUES (?, ?, ?, ?, ?, ?)",
                ("check-test", "agent1", "validation", "invalid_status", "2026-01-29T00:00:00Z", 100)
            )
            self.conn.commit()
            self.test_fail(msg, "CHECK constraint not enforced")
        except sqlite3.IntegrityError:
            self.test_pass(msg)

    def test_data_types(self):
        """Test data type handling"""
        print("\n" + "="*50)
        print("Test Suite: Data Types")
        print("="*50)

        cursor = self.conn.cursor()

        # Test INTEGER
        msg = self.test_case("INTEGER data type")
        try:
            cursor.execute(
                "INSERT INTO receipts (execution_id, agent_id, operation, status, timestamp, duration_ms) VALUES (?, ?, ?, ?, ?, ?)",
                ("int-test", "agent1", "validation", "passed", "2026-01-29T00:00:00Z", 12345)
            )
            self.conn.commit()

            cursor.execute("SELECT duration_ms FROM receipts WHERE execution_id=?", ("int-test",))
            value = cursor.fetchone()[0]
            if value == 12345:
                self.test_pass(msg)
            else:
                self.test_fail(msg, f"Expected 12345, got {value}")
        except Exception as e:
            self.test_fail(msg, str(e))

        # Test TEXT
        msg = self.test_case("TEXT data type")
        try:
            cursor.execute(
                "INSERT INTO agent_memory (agent_id, memory_json) VALUES (?, ?)",
                ("text-test", '{"key": "value"}')
            )
            self.conn.commit()

            cursor.execute("SELECT memory_json FROM agent_memory WHERE agent_id=?", ("text-test",))
            value = cursor.fetchone()[0]
            if "key" in value:
                self.test_pass(msg)
            else:
                self.test_fail(msg, f"Expected JSON with 'key', got {value}")
        except Exception as e:
            self.test_fail(msg, str(e))

    def test_json_operations(self):
        """Test JSON operations"""
        print("\n" + "="*50)
        print("Test Suite: JSON Operations")
        print("="*50)

        cursor = self.conn.cursor()

        # Test JSON storage
        msg = self.test_case("Store JSON in receipt_json")
        try:
            json_data = {"files": ["file1.rs", "file2.rs"], "hashes": {"file1.rs": "abc123"}}
            cursor.execute(
                "INSERT INTO receipts (execution_id, agent_id, operation, status, timestamp, duration_ms, receipt_json) VALUES (?, ?, ?, ?, ?, ?, ?)",
                ("json-test", "agent1", "generation", "passed", "2026-01-29T00:00:00Z", 250, json.dumps(json_data))
            )
            self.conn.commit()
            self.test_pass(msg)
        except Exception as e:
            self.test_fail(msg, str(e))

        # Test JSON extraction
        msg = self.test_case("Extract JSON array element")
        try:
            cursor.execute(
                "SELECT json_extract(receipt_json, '$.files[0]') FROM receipts WHERE execution_id=?",
                ("json-test",)
            )
            result = cursor.fetchone()[0]
            if result == "file1.rs":
                self.test_pass(msg)
            else:
                self.test_fail(msg, f"Expected 'file1.rs', got {result}")
        except Exception as e:
            self.test_fail(msg, str(e))

    def test_foreign_keys(self):
        """Test foreign key constraints"""
        print("\n" + "="*50)
        print("Test Suite: Foreign Keys")
        print("="*50)

        cursor = self.conn.cursor()

        msg = self.test_case("Foreign key on collision_detection.session_id")
        try:
            # Insert valid session
            cursor.execute(
                "INSERT INTO workflow_sessions (session_id, start_time, status, agent_count) VALUES (?, ?, ?, ?)",
                ("fk-test-session", datetime.now().isoformat(), "running", 2)
            )

            # Insert collision with valid reference
            cursor.execute(
                "INSERT INTO collision_detection (session_id, agent_id_1, agent_id_2, collision_type, description, severity, timestamp) VALUES (?, ?, ?, ?, ?, ?, ?)",
                ("fk-test-session", "agent1", "agent2", "file_conflict", "test", "warning", "2026-01-29T00:00:00Z")
            )
            self.conn.commit()
            self.test_pass(msg)
        except Exception as e:
            self.test_fail(msg, str(e))

        msg = self.test_case("Foreign key rejects invalid reference")
        try:
            cursor.execute(
                "INSERT INTO collision_detection (session_id, agent_id_1, agent_id_2, collision_type, description, severity, timestamp) VALUES (?, ?, ?, ?, ?, ?, ?)",
                ("invalid-session", "agent1", "agent2", "file_conflict", "test", "warning", "2026-01-29T00:00:00Z")
            )
            self.conn.commit()
            self.test_fail(msg, "Foreign key constraint not enforced")
        except sqlite3.IntegrityError:
            self.test_pass(msg)

    def test_views(self):
        """Test view functionality"""
        print("\n" + "="*50)
        print("Test Suite: Views")
        print("="*50)

        cursor = self.conn.cursor()

        # Insert test data
        for i in range(10):
            cursor.execute(
                "INSERT INTO receipts (execution_id, agent_id, operation, status, timestamp, files_generated, duration_ms) VALUES (?, ?, ?, ?, ?, ?, ?)",
                (f"view-test-{i}", f"agent-{i%3}", "validation", "passed" if i%2==0 else "failed", "2026-01-29T00:00:00Z", i, i*50)
            )
        self.conn.commit()

        views = ["recent_executions", "agent_performance_summary", "pipeline_stage_performance"]

        for view in views:
            msg = self.test_case(f"View '{view}' exists and is queryable")
            try:
                cursor.execute(f"SELECT COUNT(*) FROM {view}")
                self.test_pass(msg)
            except Exception as e:
                self.test_fail(msg, str(e))

    def test_integration(self):
        """Test integration scenarios"""
        print("\n" + "="*50)
        print("Test Suite: Integration Scenarios")
        print("="*50)

        cursor = self.conn.cursor()

        msg = self.test_case("Complete execution flow")
        try:
            exec_id = f"integration-test-{int(datetime.now().timestamp())}"

            # Insert receipt
            cursor.execute(
                "INSERT INTO receipts (execution_id, agent_id, operation, status, timestamp, files_generated, duration_ms) VALUES (?, ?, ?, ?, ?, ?, ?)",
                (exec_id, "integration-agent", "generation", "passed", datetime.now().isoformat(), 3, 500)
            )

            # Insert audit log
            cursor.execute(
                "INSERT INTO audit_log (timestamp, agent_id, operation, status, duration_ms) VALUES (?, ?, ?, ?, ?)",
                (datetime.now().isoformat(), "integration-agent", "generation complete", "completed", 500)
            )

            self.conn.commit()

            # Verify both inserted
            cursor.execute("SELECT COUNT(*) FROM receipts WHERE execution_id=?", (exec_id,))
            receipt_count = cursor.fetchone()[0]

            cursor.execute("SELECT COUNT(*) FROM audit_log WHERE agent_id=?", ("integration-agent",))
            audit_count = cursor.fetchone()[0]

            if receipt_count > 0 and audit_count > 0:
                self.test_pass(msg)
            else:
                self.test_fail(msg, f"receipt_count={receipt_count}, audit_count={audit_count}")
        except Exception as e:
            self.test_fail(msg, str(e))

    def print_report(self):
        """Print test report"""
        print("\n" + "="*50)
        print("Test Results Summary")
        print("="*50)
        print(f"Total Tests:  {self.tests_run}")
        print(f"Passed:       {Colors.GREEN}{self.tests_passed}{Colors.NC}")

        if self.tests_failed > 0:
            print(f"Failed:       {Colors.RED}{self.tests_failed}{Colors.NC}")
        else:
            print(f"Failed:       {Colors.GREEN}{self.tests_failed}{Colors.NC}")

        print()

        if self.tests_failed == 0:
            print(f"{Colors.GREEN}✓ All tests passed!{Colors.NC}")
            return True
        else:
            print(f"{Colors.RED}✗ Some tests failed{Colors.NC}")
            if self.failures:
                print("\nFailures:")
                for test, error in self.failures:
                    print(f"  - {test}")
                    print(f"    {error}")
            return False

def main():
    db_path = sys.argv[1] if len(sys.argv) > 1 else ".ggen/tier2-test.db"

    runner = TestRunner(db_path)
    try:
        runner.setup()
        runner.run_tests()
        success = runner.tests_failed == 0
    finally:
        runner.cleanup()

    sys.exit(0 if success else 1)

if __name__ == "__main__":
    main()
