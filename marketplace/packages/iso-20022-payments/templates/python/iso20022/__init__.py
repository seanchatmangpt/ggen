"""
ISO 20022 Payment Analytics - Python Implementation
Provides payment data analysis and reporting
"""

from dataclasses import dataclass
from datetime import datetime, date
from typing import List, Optional, Dict
from decimal import Decimal
import xml.etree.ElementTree as ET
import re
from schwifty import IBAN, BIC

# ============================================================
# Data Classes
# ============================================================

@dataclass
class Party:
    name: str
    postal_address: Optional['PostalAddress'] = None

@dataclass
class PostalAddress:
    country: str
    address_lines: List[str]

@dataclass
class Account:
    iban: Optional[str] = None
    other: Optional[str] = None

@dataclass
class FinancialInstitution:
    bic: Optional[str] = None
    name: Optional[str] = None

@dataclass
class Amount:
    value: Decimal
    currency: str

@dataclass
class PaymentIdentification:
    instruction_identification: str
    end_to_end_identification: str

@dataclass
class RemittanceInformation:
    unstructured: Optional[List[str]] = None
    structured: Optional[Dict] = None

@dataclass
class CreditTransferTransaction:
    payment_identification: PaymentIdentification
    instructed_amount: Amount
    creditor: Party
    creditor_account: Account
    creditor_agent: FinancialInstitution
    remittance_information: Optional[RemittanceInformation] = None

@dataclass
class PaymentInformation:
    payment_information_identification: str
    payment_method: str  # 'TRF' or 'DD'
    requested_execution_date: date
    debtor: Party
    debtor_account: Account
    debtor_agent: FinancialInstitution
    credit_transfer_transactions: List[CreditTransferTransaction]

@dataclass
class GroupHeader:
    message_identification: str
    creation_date_time: datetime
    number_of_transactions: int
    control_sum: Decimal
    initiating_party: Party

@dataclass
class CustomerCreditTransferInitiation:
    group_header: GroupHeader
    payment_information: List[PaymentInformation]

# ============================================================
# Payment Analytics
# ============================================================

class PaymentAnalytics:
    """Analyze payment data for insights and compliance"""

    def __init__(self):
        self.payments: List[CustomerCreditTransferInitiation] = []

    def add_payment(self, payment: CustomerCreditTransferInitiation):
        """Add payment for analysis"""
        self.payments.append(payment)

    def total_volume_by_currency(self) -> Dict[str, Decimal]:
        """Calculate total payment volume by currency"""
        volumes: Dict[str, Decimal] = {}

        for payment in self.payments:
            for pmt_inf in payment.payment_information:
                for tx in pmt_inf.credit_transfer_transactions:
                    currency = tx.instructed_amount.currency
                    amount = tx.instructed_amount.value
                    volumes[currency] = volumes.get(currency, Decimal(0)) + amount

        return volumes

    def transaction_count(self) -> int:
        """Get total transaction count"""
        count = 0
        for payment in self.payments:
            for pmt_inf in payment.payment_information:
                count += len(pmt_inf.credit_transfer_transactions)
        return count

    def payments_by_creditor(self) -> Dict[str, List[CreditTransferTransaction]]:
        """Group payments by creditor"""
        by_creditor: Dict[str, List[CreditTransferTransaction]] = {}

        for payment in self.payments:
            for pmt_inf in payment.payment_information:
                for tx in pmt_inf.credit_transfer_transactions:
                    creditor_name = tx.creditor.name
                    if creditor_name not in by_creditor:
                        by_creditor[creditor_name] = []
                    by_creditor[creditor_name].append(tx)

        return by_creditor

    def payments_above_threshold(self, threshold: Decimal, currency: str = 'EUR') -> List[CreditTransferTransaction]:
        """Find payments above threshold amount"""
        high_value = []

        for payment in self.payments:
            for pmt_inf in payment.payment_information:
                for tx in pmt_inf.credit_transfer_transactions:
                    if tx.instructed_amount.currency == currency and tx.instructed_amount.value > threshold:
                        high_value.append(tx)

        return high_value

    def payments_by_country(self) -> Dict[str, int]:
        """Count payments by creditor country"""
        by_country: Dict[str, int] = {}

        for payment in self.payments:
            for pmt_inf in payment.payment_information:
                for tx in pmt_inf.credit_transfer_transactions:
                    if tx.creditor.postal_address:
                        country = tx.creditor.postal_address.country
                        by_country[country] = by_country.get(country, 0) + 1

        return by_country

    def average_payment_amount(self, currency: str = 'EUR') -> Decimal:
        """Calculate average payment amount for currency"""
        total = Decimal(0)
        count = 0

        for payment in self.payments:
            for pmt_inf in payment.payment_information:
                for tx in pmt_inf.credit_transfer_transactions:
                    if tx.instructed_amount.currency == currency:
                        total += tx.instructed_amount.value
                        count += 1

        return total / count if count > 0 else Decimal(0)

    def payment_velocity_report(self, days: int = 7) -> Dict[str, any]:
        """Generate payment velocity report for recent days"""
        cutoff = datetime.now().date()
        recent_payments = []

        for payment in self.payments:
            for pmt_inf in payment.payment_information:
                if (cutoff - pmt_inf.requested_execution_date).days <= days:
                    recent_payments.extend(pmt_inf.credit_transfer_transactions)

        total_amount = sum(tx.instructed_amount.value for tx in recent_payments)
        avg_per_day = total_amount / days if days > 0 else Decimal(0)

        return {
            'period_days': days,
            'transaction_count': len(recent_payments),
            'total_amount': total_amount,
            'average_per_day': avg_per_day,
            'currencies': list(set(tx.instructed_amount.currency for tx in recent_payments))
        }

# ============================================================
# IBAN/BIC Validation
# ============================================================

def validate_iban(iban_str: str) -> bool:
    """Validate IBAN using schwifty library"""
    try:
        iban = IBAN(iban_str)
        return iban.is_valid
    except:
        return False

def validate_bic(bic_str: str) -> bool:
    """Validate BIC using schwifty library"""
    try:
        bic = BIC(bic_str)
        return bic.is_valid
    except:
        return False

def format_iban(iban_str: str) -> str:
    """Format IBAN with spaces"""
    try:
        iban = IBAN(iban_str)
        return iban.formatted
    except:
        return iban_str

# ============================================================
# XML Parsing
# ============================================================

def parse_pain001_xml(xml_str: str) -> CustomerCreditTransferInitiation:
    """Parse pain.001 XML into data structures"""
    root = ET.fromstring(xml_str)
    ns = {'iso': 'urn:iso:std:iso:20022:tech:xsd:pain.001.001.03'}

    # Parse Group Header
    grp_hdr = root.find('.//iso:GrpHdr', ns)
    group_header = GroupHeader(
        message_identification=grp_hdr.find('iso:MsgId', ns).text,
        creation_date_time=datetime.fromisoformat(grp_hdr.find('iso:CreDtTm', ns).text),
        number_of_transactions=int(grp_hdr.find('iso:NbOfTxs', ns).text),
        control_sum=Decimal(grp_hdr.find('iso:CtrlSum', ns).text),
        initiating_party=Party(name=grp_hdr.find('.//iso:InitgPty/iso:Nm', ns).text)
    )

    # Parse Payment Information
    payment_info_list = []
    for pmt_inf in root.findall('.//iso:PmtInf', ns):
        debtor = Party(name=pmt_inf.find('iso:Dbtr/iso:Nm', ns).text)
        debtor_account = Account(iban=pmt_inf.find('iso:DbtrAcct/iso:Id/iso:IBAN', ns).text)
        debtor_agent = FinancialInstitution(bic=pmt_inf.find('iso:DbtrAgt/iso:FinInstnId/iso:BIC', ns).text)

        # Parse transactions
        transactions = []
        for tx in pmt_inf.findall('iso:CdtTrfTxInf', ns):
            pmt_id = PaymentIdentification(
                instruction_identification=tx.find('iso:PmtId/iso:InstrId', ns).text,
                end_to_end_identification=tx.find('iso:PmtId/iso:EndToEndId', ns).text
            )

            amt_elem = tx.find('.//iso:InstdAmt', ns)
            amount = Amount(
                value=Decimal(amt_elem.text),
                currency=amt_elem.get('Ccy')
            )

            creditor = Party(name=tx.find('iso:Cdtr/iso:Nm', ns).text)
            creditor_account = Account(iban=tx.find('iso:CdtrAcct/iso:Id/iso:IBAN', ns).text)
            creditor_agent = FinancialInstitution(bic=tx.find('iso:CdtrAgt/iso:FinInstnId/iso:BIC', ns).text)

            # Parse remittance info if present
            rmt_inf_elem = tx.find('iso:RmtInf', ns)
            remittance = None
            if rmt_inf_elem is not None:
                ustrd_elems = rmt_inf_elem.findall('iso:Ustrd', ns)
                if ustrd_elems:
                    remittance = RemittanceInformation(
                        unstructured=[elem.text for elem in ustrd_elems]
                    )

            transactions.append(CreditTransferTransaction(
                payment_identification=pmt_id,
                instructed_amount=amount,
                creditor=creditor,
                creditor_account=creditor_account,
                creditor_agent=creditor_agent,
                remittance_information=remittance
            ))

        payment_info_list.append(PaymentInformation(
            payment_information_identification=pmt_inf.find('iso:PmtInfId', ns).text,
            payment_method=pmt_inf.find('iso:PmtMtd', ns).text,
            requested_execution_date=date.fromisoformat(pmt_inf.find('iso:ReqdExctnDt', ns).text),
            debtor=debtor,
            debtor_account=debtor_account,
            debtor_agent=debtor_agent,
            credit_transfer_transactions=transactions
        ))

    return CustomerCreditTransferInitiation(
        group_header=group_header,
        payment_information=payment_info_list
    )

# ============================================================
# Example Usage
# ============================================================

if __name__ == '__main__':
    # Example: Validate IBAN
    iban = "DE89370400440532013000"
    print(f"IBAN {iban} is valid: {validate_iban(iban)}")
    print(f"Formatted IBAN: {format_iban(iban)}")

    # Example: Validate BIC
    bic = "DEUTDEFF"
    print(f"BIC {bic} is valid: {validate_bic(bic)}")

    # Example: Analytics
    analytics = PaymentAnalytics()
    print(f"Total transaction count: {analytics.transaction_count()}")
    print(f"Volume by currency: {analytics.total_volume_by_currency()}")
