"""
KYC/AML Compliance - Python Transaction Monitoring
Provides real-time transaction monitoring and alerting
"""

from dataclasses import dataclass
from datetime import datetime
from typing import List, Optional
from decimal import Decimal

@dataclass
class Transaction:
    transaction_id: str
    customer_id: str
    amount: Decimal
    currency: str
    timestamp: datetime
    counterparty_country: Optional[str] = None

@dataclass
class MonitoringRule:
    rule_id: str
    rule_type: str
    threshold: Decimal
    time_window_hours: int

class TransactionMonitor:
    def __init__(self):
        self.rules: List[MonitoringRule] = []
        self.transactions: List[Transaction] = []

    def add_rule(self, rule: MonitoringRule):
        self.rules.append(rule)

    def check_transaction(self, tx: Transaction) -> List[str]:
        alerts = []
        for rule in self.rules:
            if self._rule_triggered(tx, rule):
                alerts.append(f"ALERT: {rule.rule_type} triggered for {tx.transaction_id}")
        return alerts

    def _rule_triggered(self, tx: Transaction, rule: MonitoringRule) -> bool:
        if rule.rule_type == 'THRESHOLD':
            return tx.amount > rule.threshold
        elif rule.rule_type == 'VELOCITY':
            recent_txs = [t for t in self.transactions
                         if t.customer_id == tx.customer_id
                         and (tx.timestamp - t.timestamp).total_seconds() < rule.time_window_hours * 3600]
            total = sum(t.amount for t in recent_txs)
            return total > rule.threshold
        return False

if __name__ == '__main__':
    monitor = TransactionMonitor()
    monitor.add_rule(MonitoringRule('RULE-1', 'THRESHOLD', Decimal('10000'), 24))

    tx = Transaction('TX-001', 'CUST-001', Decimal('15000'), 'USD', datetime.now())
    alerts = monitor.check_transaction(tx)
    print(f"Alerts: {alerts}")
