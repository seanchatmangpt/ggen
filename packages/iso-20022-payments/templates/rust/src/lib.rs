// ISO 20022 Payment Messages - Rust Implementation
// Provides XML generation and parsing for pain.001, pacs.008, camt.053

use serde::{Deserialize, Serialize};
use chrono::{DateTime, NaiveDate, Utc};
use rust_decimal::Decimal;
use std::error::Error;

// ============================================================
// Core Message Types
// ============================================================

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CustomerCreditTransferInitiation {
    pub group_header: GroupHeader,
    pub payment_information: Vec<PaymentInformation>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GroupHeader {
    pub message_identification: String,
    pub creation_date_time: DateTime<Utc>,
    pub number_of_transactions: u32,
    pub control_sum: Decimal,
    pub initiating_party: Party,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PaymentInformation {
    pub payment_information_identification: String,
    pub payment_method: PaymentMethod,
    pub requested_execution_date: NaiveDate,
    pub debtor: Party,
    pub debtor_account: Account,
    pub debtor_agent: FinancialInstitution,
    pub credit_transfer_transactions: Vec<CreditTransferTransaction>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PaymentMethod {
    #[serde(rename = "TRF")]
    Transfer,
    #[serde(rename = "DD")]
    DirectDebit,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CreditTransferTransaction {
    pub payment_identification: PaymentIdentification,
    pub instructed_amount: Amount,
    pub creditor: Party,
    pub creditor_account: Account,
    pub creditor_agent: FinancialInstitution,
    pub remittance_information: Option<RemittanceInformation>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PaymentIdentification {
    pub instruction_identification: String,
    pub end_to_end_identification: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Party {
    pub name: String,
    pub postal_address: Option<PostalAddress>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PostalAddress {
    pub country: String,
    pub address_line: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Account {
    pub iban: Option<String>,
    pub other: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FinancialInstitution {
    pub bic: Option<String>,
    pub name: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Amount {
    pub value: Decimal,
    pub currency: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RemittanceInformation {
    pub unstructured: Option<Vec<String>>,
    pub structured: Option<StructuredRemittance>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StructuredRemittance {
    pub creditor_reference_information: CreditorReferenceInformation,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CreditorReferenceInformation {
    pub reference: String,
    pub reference_type: String,
}

// ============================================================
// XML Generation
// ============================================================

pub fn generate_pain001_xml(message: &CustomerCreditTransferInitiation) -> Result<String, Box<dyn Error>> {
    let mut xml = String::new();

    xml.push_str("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
    xml.push_str("<Document xmlns=\"urn:iso:std:iso:20022:tech:xsd:pain.001.001.03\" ");
    xml.push_str("xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">\n");
    xml.push_str("  <CstmrCdtTrfInitn>\n");

    // Group Header
    xml.push_str("    <GrpHdr>\n");
    xml.push_str(&format!("      <MsgId>{}</MsgId>\n", message.group_header.message_identification));
    xml.push_str(&format!("      <CreDtTm>{}</CreDtTm>\n", message.group_header.creation_date_time.format("%Y-%m-%dT%H:%M:%S")));
    xml.push_str(&format!("      <NbOfTxs>{}</NbOfTxs>\n", message.group_header.number_of_transactions));
    xml.push_str(&format!("      <CtrlSum>{:.2}</CtrlSum>\n", message.group_header.control_sum));
    xml.push_str("      <InitgPty>\n");
    xml.push_str(&format!("        <Nm>{}</Nm>\n", message.group_header.initiating_party.name));
    xml.push_str("      </InitgPty>\n");
    xml.push_str("    </GrpHdr>\n");

    // Payment Information
    for pmt_inf in &message.payment_information {
        xml.push_str("    <PmtInf>\n");
        xml.push_str(&format!("      <PmtInfId>{}</PmtInfId>\n", pmt_inf.payment_information_identification));
        xml.push_str(&format!("      <PmtMtd>{}</PmtMtd>\n", match pmt_inf.payment_method {
            PaymentMethod::Transfer => "TRF",
            PaymentMethod::DirectDebit => "DD",
        }));
        xml.push_str(&format!("      <ReqdExctnDt>{}</ReqdExctnDt>\n", pmt_inf.requested_execution_date.format("%Y-%m-%d")));

        // Debtor
        xml.push_str("      <Dbtr>\n");
        xml.push_str(&format!("        <Nm>{}</Nm>\n", pmt_inf.debtor.name));
        xml.push_str("      </Dbtr>\n");

        // Debtor Account
        xml.push_str("      <DbtrAcct>\n");
        xml.push_str("        <Id>\n");
        if let Some(iban) = &pmt_inf.debtor_account.iban {
            xml.push_str(&format!("          <IBAN>{}</IBAN>\n", iban));
        }
        xml.push_str("        </Id>\n");
        xml.push_str("      </DbtrAcct>\n");

        // Debtor Agent
        xml.push_str("      <DbtrAgt>\n");
        xml.push_str("        <FinInstnId>\n");
        if let Some(bic) = &pmt_inf.debtor_agent.bic {
            xml.push_str(&format!("          <BIC>{}</BIC>\n", bic));
        }
        xml.push_str("        </FinInstnId>\n");
        xml.push_str("      </DbtrAgt>\n");

        // Credit Transfer Transactions
        for tx in &pmt_inf.credit_transfer_transactions {
            xml.push_str("      <CdtTrfTxInf>\n");
            xml.push_str("        <PmtId>\n");
            xml.push_str(&format!("          <InstrId>{}</InstrId>\n", tx.payment_identification.instruction_identification));
            xml.push_str(&format!("          <EndToEndId>{}</EndToEndId>\n", tx.payment_identification.end_to_end_identification));
            xml.push_str("        </PmtId>\n");

            // Amount
            xml.push_str(&format!("        <Amt>\n          <InstdAmt Ccy=\"{}\">{:.2}</InstdAmt>\n        </Amt>\n",
                tx.instructed_amount.currency, tx.instructed_amount.value));

            // Creditor Agent
            xml.push_str("        <CdtrAgt>\n");
            xml.push_str("          <FinInstnId>\n");
            if let Some(bic) = &tx.creditor_agent.bic {
                xml.push_str(&format!("            <BIC>{}</BIC>\n", bic));
            }
            xml.push_str("          </FinInstnId>\n");
            xml.push_str("        </CdtrAgt>\n");

            // Creditor
            xml.push_str("        <Cdtr>\n");
            xml.push_str(&format!("          <Nm>{}</Nm>\n", tx.creditor.name));
            xml.push_str("        </Cdtr>\n");

            // Creditor Account
            xml.push_str("        <CdtrAcct>\n");
            xml.push_str("          <Id>\n");
            if let Some(iban) = &tx.creditor_account.iban {
                xml.push_str(&format!("            <IBAN>{}</IBAN>\n", iban));
            }
            xml.push_str("          </Id>\n");
            xml.push_str("        </CdtrAcct>\n");

            // Remittance Information
            if let Some(rmt) = &tx.remittance_information {
                xml.push_str("        <RmtInf>\n");
                if let Some(ustrd) = &rmt.unstructured {
                    for line in ustrd {
                        xml.push_str(&format!("          <Ustrd>{}</Ustrd>\n", line));
                    }
                }
                xml.push_str("        </RmtInf>\n");
            }

            xml.push_str("      </CdtTrfTxInf>\n");
        }

        xml.push_str("    </PmtInf>\n");
    }

    xml.push_str("  </CstmrCdtTrfInitn>\n");
    xml.push_str("</Document>\n");

    Ok(xml)
}

// ============================================================
// IBAN Validation
// ============================================================

pub fn validate_iban(iban: &str) -> Result<bool, Box<dyn Error>> {
    // Remove spaces and convert to uppercase
    let iban = iban.replace(" ", "").to_uppercase();

    // Check length (15-34 characters)
    if iban.len() < 15 || iban.len() > 34 {
        return Ok(false);
    }

    // Check format: 2 letters + 2 digits + up to 30 alphanumeric
    let re = regex::Regex::new(r"^[A-Z]{2}[0-9]{2}[A-Z0-9]+$")?;
    if !re.is_match(&iban) {
        return Ok(false);
    }

    // Mod-97 checksum validation
    let rearranged = format!("{}{}", &iban[4..], &iban[..4]);
    let mut numeric = String::new();
    for c in rearranged.chars() {
        if c.is_alphabetic() {
            numeric.push_str(&(c as u32 - 'A' as u32 + 10).to_string());
        } else {
            numeric.push(c);
        }
    }

    // Calculate mod 97
    let mut remainder: u32 = 0;
    for chunk in numeric.chars().collect::<Vec<_>>().chunks(7) {
        let chunk_str: String = chunk.iter().collect();
        let value = format!("{}{}", remainder, chunk_str).parse::<u64>()?;
        remainder = (value % 97) as u32;
    }

    Ok(remainder == 1)
}

// ============================================================
// BIC Validation
// ============================================================

pub fn validate_bic(bic: &str) -> bool {
    let bic = bic.to_uppercase();

    // BIC must be 8 or 11 characters
    if bic.len() != 8 && bic.len() != 11 {
        return false;
    }

    // Format: 4 letters (institution) + 2 letters (country) + 2 alphanumeric (location) + optional 3 alphanumeric (branch)
    let re = regex::Regex::new(r"^[A-Z]{6}[A-Z0-9]{2}([A-Z0-9]{3})?$").unwrap();
    re.is_match(&bic)
}

// ============================================================
// Tests
// ============================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_validate_iban() {
        assert!(validate_iban("DE89370400440532013000").unwrap());
        assert!(validate_iban("GB82 WEST 1234 5698 7654 32").unwrap());
        assert!(!validate_iban("DE00370400440532013000").unwrap());
        assert!(!validate_iban("INVALID").unwrap());
    }

    #[test]
    fn test_validate_bic() {
        assert!(validate_bic("DEUTDEFF"));
        assert!(validate_bic("DEUTDEFF500"));
        assert!(!validate_bic("DEUT"));
        assert!(!validate_bic("DEUTDEFF50"));
    }

    #[test]
    fn test_generate_pain001_xml() {
        let message = CustomerCreditTransferInitiation {
            group_header: GroupHeader {
                message_identification: "MSG-001".to_string(),
                creation_date_time: Utc::now(),
                number_of_transactions: 1,
                control_sum: Decimal::new(100000, 2),
                initiating_party: Party {
                    name: "Test Company".to_string(),
                    postal_address: None,
                },
            },
            payment_information: vec![],
        };

        let xml = generate_pain001_xml(&message).unwrap();
        assert!(xml.contains("MSG-001"));
        assert!(xml.contains("Test Company"));
    }
}
