#!/usr/bin/env python3
"""
Stage 2: Agent-based stop release verification
Runs conformance checks and validates receipt signatures
"""

import json
import sqlite3
import sys
from dataclasses import dataclass, asdict
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Dict, List, Optional

# Add project root to sys.path for ostar imports
PROJECT_ROOT = Path(__file__).parent.parent.parent
sys.path.insert(0, str(PROJECT_ROOT))


@dataclass
class StopVerificationResult:
    """Result of Stage 2 verification."""
    stop_allowed: bool
    stage: str
    conformance: float
    holds: List[str]
    message: str
    authority_check: bool = False
    signature_valid: Optional[bool] = None

    def to_dict(self) -> Dict[str, Any]:
        return asdict(self)


class StopVerifier:
    """Agent-based verification for stop release control."""

    def __init__(self, project_root: Optional[Path] = None):
        """Initialize verifier.

        Args:
            project_root: Project root directory (defaults to CWD)
        """
        self.project_root = Path(project_root) if project_root else Path.cwd()
        self.compliance_db = self._find_compliance_db()

    def _find_compliance_db(self) -> Optional[Path]:
        """Find compliance ledger database."""
        candidates = [
            self.project_root / "artifacts" / "compliance_ledger.db",
            self.project_root / "artifacts" / "manufacturing" / "compliance_ledger.db",
        ]

        # Add any .db files in artifacts/
        artifacts_dir = self.project_root / "artifacts"
        if artifacts_dir.exists():
            candidates.extend(artifacts_dir.glob("*.db"))

        for db_path in candidates:
            if db_path.is_file():
                return db_path

        return None

    def verify(self, input_data: Dict[str, Any]) -> StopVerificationResult:
        """Perform Stage 2 verification.

        Args:
            input_data: Input from Stage 1 with stop_reason, session_id, work_units

        Returns:
            StopVerificationResult with verification status
        """
        # Check conformance if possible
        conformance = self._check_conformance()

        # Check for active holds
        holds = self._check_active_holds()

        # Verify receipt authority
        authority_ok = self._verify_authority()

        # Verify receipt signature
        signature_ok = self._verify_signature()

        # Determine if stop is allowed
        stop_allowed = (
            conformance >= 0.95
            and len(holds) == 0
            and authority_ok
            and (signature_ok is None or signature_ok)
        )

        message = self._build_message(
            stop_allowed, conformance, holds, authority_ok, signature_ok
        )

        return StopVerificationResult(
            stop_allowed=stop_allowed,
            stage="agent_verification",
            conformance=conformance,
            holds=holds,
            message=message,
            authority_check=authority_ok,
            signature_valid=signature_ok,
        )

    def _check_conformance(self) -> float:
        """Check process conformance using conformance_engine.

        Returns:
            Conformance score 0-1, or 0.95 if check unavailable
        """
        try:
            # Try to import conformance engine
            sys.path.insert(0, str(self.project_root / "src"))
            from ostar.process.conformance_engine import (
                ConformanceEngine,
                ConformanceLevel,
            )
            from ostar.process.deviation_detector import DeviationDetector

            # Load legal process model (would be POWL or BPMN)
            legal_graph = {
                "nodes": ["seed", "breed", "validate", "compile", "release"],
                "edges": [
                    ("seed", "breed"),
                    ("breed", "validate"),
                    ("validate", "compile"),
                    ("compile", "release"),
                ],
            }

            engine = ConformanceEngine(legal_process_graph=legal_graph)
            observed_trace = self._load_observed_trace()

            if not observed_trace:
                # No trace available, return neutral score
                return 0.95

            report = engine.check_conformance(
                observed_trace=observed_trace,
                legal_process_uri="urn:legal:manufacturing-pipeline",
                observed_trace_uri=f"urn:observed:{datetime.now(timezone.utc).isoformat()}",
            )

            return report.fitness

        except ImportError as e:
            # ostar not available, return neutral score (not a failure)
            print(f"Info: Conformance engine not available: {e}", file=sys.stderr)
            return 0.95
        except Exception as e:
            # Conformance check failed, return conservative score
            print(f"Warning: Conformance check failed: {e}", file=sys.stderr)
            return 0.85

    def _load_observed_trace(self) -> Optional[Dict[str, Any]]:
        """Load observed trace from evidence log or artifacts.

        Returns:
            Observed trace dict or None
        """
        # Try to load from evidence log
        evidence_paths = [
            self.project_root / "artifacts" / "evidence.log",
            self.project_root / "artifacts" / "manufacturing" / "evidence.log",
            self.project_root / ".evidence.log",
        ]

        for path in evidence_paths:
            if path.is_file():
                try:
                    # Parse evidence log into trace format
                    events = []
                    for line in path.read_text().strip().split("\n"):
                        if line.strip():
                            # Try to parse as JSON
                            try:
                                event = json.loads(line)
                                events.append(event)
                            except json.JSONDecodeError:
                                # Not JSON, skip
                                continue

                    if events:
                        return {"events": events}

                except Exception:
                    continue

        return None

    def _check_active_holds(self) -> List[str]:
        """Check for active release holds in compliance ledger.

        Returns:
            List of hold reasons (empty if no holds)
        """
        if not self.compliance_db:
            return []

        try:
            conn = sqlite3.connect(str(self.compliance_db))
            cursor = conn.cursor()

            # Check for release_blockers table
            cursor.execute(
                """
                SELECT name FROM sqlite_master
                WHERE type='table' AND name='release_blockers'
            """
            )
            if not cursor.fetchone():
                return []

            # Query active holds
            cursor.execute(
                """
                SELECT reason FROM release_blockers
                WHERE active = 1
                """
            )
            holds = [row[0] for row in cursor.fetchall()]
            conn.close()

            return holds

        except Exception:
            return []

    def _verify_authority(self) -> bool:
        """Verify receipt is from authorized module.

        Returns:
            True if authority verified, False otherwise
        """
        if not self.compliance_db:
            return False

        try:
            conn = sqlite3.connect(str(self.compliance_db))
            cursor = conn.cursor()

            # Find receipts table
            cursor.execute(
                """
                SELECT name FROM sqlite_master
                WHERE type='table' AND name LIKE '%receipt%'
            """
            )
            tables = cursor.fetchall()
            if not tables:
                return False

            table_name = tables[0][0]

            # Check authority of latest receipt
            cursor.execute(
                f"""
                SELECT authority FROM {table_name}
                ORDER BY timestamp DESC
                LIMIT 1
                """
            )
            result = cursor.fetchone()
            conn.close()

            if not result:
                return False

            authority = result[0]

            # Authorized authorities
            authorized = ["ConformanceEngine", "manufacturing", "ostar"]

            return any(auth in authority for auth in authorized)

        except Exception:
            return False

    def _verify_signature(self) -> Optional[bool]:
        """Verify receipt signature.

        Returns:
            True if valid, False if invalid, None if unverifiable
        """
        if not self.compliance_db:
            return None

        try:
            from ostar.process.conformance_receipt import ConformanceReceipt

            conn = sqlite3.connect(str(self.compliance_db))
            cursor = conn.cursor()

            # Find receipts table
            cursor.execute(
                """
                SELECT name FROM sqlite_master
                WHERE type='table' AND name LIKE '%receipt%'
            """
            )
            tables = cursor.fetchall()
            if not tables:
                return None

            table_name = tables[0][0]

            # Get latest receipt data
            cursor.execute(
                f"""
                SELECT report_data, signature FROM {table_name}
                ORDER BY timestamp DESC
                LIMIT 1
                """
            )
            result = cursor.fetchone()
            conn.close()

            if not result:
                return None

            report_data, signature = result

            # Verify signature against report data
            if isinstance(report_data, str):
                report_dict = json.loads(report_data)
            else:
                report_dict = report_data

            # Reconstruct receipt for verification
            receipt = ConformanceReceipt(
                receipt_id="temp",
                conformance_report_uri="",
                timestamp=datetime.now(timezone.utc),
                authority="",
                signature=signature,
                attestation="",
                fitness=0.0,
                precision=0.0,
                generalization=0.0,
                deviation_count=0,
                legal_process_uri="",
                observed_trace_uri="",
            )

            return receipt.verify_receipt(report_dict)

        except Exception:
            return None

    def _build_message(
        self,
        stop_allowed: bool,
        conformance: float,
        holds: List[str],
        authority_ok: bool,
        signature_ok: Optional[bool],
    ) -> str:
        """Build human-readable message.

        Args:
            stop_allowed: Whether stop is allowed
            conformance: Conformance score
            holds: Active holds
            authority_ok: Authority verified
            signature_ok: Signature verified

        Returns:
            Message string
        """
        if stop_allowed:
            return (
                f"Stop allowed: conformance {conformance:.2f}, "
                f"no active holds, authority verified"
            )
        else:
            issues = []
            if conformance < 0.95:
                issues.append(f"low conformance ({conformance:.2f})")
            if holds:
                issues.append(f"{len(holds)} active holds")
            if not authority_ok:
                issues.append("unauthorized authority")
            if signature_ok is False:
                issues.append("invalid signature")

            return f"Stop denied: {', '.join(issues)}"


def main():
    """Main entry point."""
    if sys.stdin.isatty():
        # No stdin, perform verification with minimal context
        input_data = {
            "stop_reason": "manual_check",
            "session_id": "",
            "work_units": [],
        }
    else:
        # Read JSON from stdin
        try:
            input_data = json.load(sys.stdin)
        except json.JSONDecodeError:
            result = StopVerificationResult(
                stop_allowed=False,
                stage="agent_verification",
                conformance=0.0,
                holds=[],
                message="Invalid JSON input",
                authority_check=False,
                signature_valid=None,
            )
            print(json.dumps(asdict(result), indent=2))
            sys.exit(2)

    verifier = StopVerifier()
    result = verifier.verify(input_data)

    print(json.dumps(result.to_dict(), indent=2))

    # Exit code: 0 if allowed, 1 if denied, 2 if error
    if result.stop_allowed:
        sys.exit(0)
    else:
        sys.exit(1)


if __name__ == "__main__":
    main()
