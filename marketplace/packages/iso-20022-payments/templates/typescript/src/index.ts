// ISO 20022 Payment Gateway SDK - TypeScript Implementation
// Provides payment initiation and status tracking

import { XMLBuilder, XMLParser } from 'fast-xml-parser';
import * as IBAN from 'iban';

// ============================================================
// Type Definitions
// ============================================================

export interface CustomerCreditTransferInitiation {
  groupHeader: GroupHeader;
  paymentInformation: PaymentInformation[];
}

export interface GroupHeader {
  messageIdentification: string;
  creationDateTime: Date;
  numberOfTransactions: number;
  controlSum: number;
  initiatingParty: Party;
}

export interface PaymentInformation {
  paymentInformationIdentification: string;
  paymentMethod: 'TRF' | 'DD';
  requestedExecutionDate: Date;
  debtor: Party;
  debtorAccount: Account;
  debtorAgent: FinancialInstitution;
  creditTransferTransactions: CreditTransferTransaction[];
}

export interface CreditTransferTransaction {
  paymentIdentification: PaymentIdentification;
  instructedAmount: Amount;
  creditor: Party;
  creditorAccount: Account;
  creditorAgent: FinancialInstitution;
  remittanceInformation?: RemittanceInformation;
}

export interface PaymentIdentification {
  instructionIdentification: string;
  endToEndIdentification: string;
}

export interface Party {
  name: string;
  postalAddress?: PostalAddress;
}

export interface PostalAddress {
  country: string;
  addressLine: string[];
}

export interface Account {
  iban?: string;
  other?: string;
}

export interface FinancialInstitution {
  bic?: string;
  name?: string;
}

export interface Amount {
  value: number;
  currency: string;
}

export interface RemittanceInformation {
  unstructured?: string[];
  structured?: StructuredRemittance;
}

export interface StructuredRemittance {
  creditorReferenceInformation: {
    reference: string;
    referenceType: string;
  };
}

export interface PaymentStatusReport {
  messageIdentification: string;
  originalMessageIdentification: string;
  transactionStatus: TransactionStatus[];
}

export interface TransactionStatus {
  statusIdentification: string;
  originalEndToEndIdentification: string;
  transactionStatus: 'ACCP' | 'ACSC' | 'ACTC' | 'ACWC' | 'RJCT' | 'PDNG';
  statusReasonCode?: string;
  additionalStatusReasonInformation?: string;
}

// ============================================================
// Payment Gateway Client
// ============================================================

export class ISO20022PaymentGateway {
  private apiUrl: string;
  private apiKey: string;

  constructor(apiUrl: string, apiKey: string) {
    this.apiUrl = apiUrl;
    this.apiKey = apiKey;
  }

  /**
   * Initiate a credit transfer payment (pain.001)
   */
  async initiatePayment(payment: CustomerCreditTransferInitiation): Promise<string> {
    // Validate payment data
    this.validatePayment(payment);

    // Generate pain.001 XML
    const xml = this.generatePain001XML(payment);

    // Submit to payment gateway
    const response = await fetch(`${this.apiUrl}/payments/initiate`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/xml',
        'Authorization': `Bearer ${this.apiKey}`,
      },
      body: xml,
    });

    if (!response.ok) {
      throw new Error(`Payment initiation failed: ${response.statusText}`);
    }

    const result = await response.json();
    return result.messageIdentification;
  }

  /**
   * Get payment status (pain.002)
   */
  async getPaymentStatus(messageId: string): Promise<PaymentStatusReport> {
    const response = await fetch(`${this.apiUrl}/payments/${messageId}/status`, {
      headers: {
        'Authorization': `Bearer ${this.apiKey}`,
      },
    });

    if (!response.ok) {
      throw new Error(`Failed to get payment status: ${response.statusText}`);
    }

    const xml = await response.text();
    return this.parsePain002XML(xml);
  }

  /**
   * Validate payment data
   */
  private validatePayment(payment: CustomerCreditTransferInitiation): void {
    // Validate debtor IBAN
    for (const pmtInf of payment.paymentInformation) {
      if (pmtInf.debtorAccount.iban && !this.validateIBAN(pmtInf.debtorAccount.iban)) {
        throw new Error(`Invalid debtor IBAN: ${pmtInf.debtorAccount.iban}`);
      }

      // Validate debtor agent BIC
      if (pmtInf.debtorAgent.bic && !this.validateBIC(pmtInf.debtorAgent.bic)) {
        throw new Error(`Invalid debtor BIC: ${pmtInf.debtorAgent.bic}`);
      }

      // Validate transactions
      for (const tx of pmtInf.creditTransferTransactions) {
        if (tx.creditorAccount.iban && !this.validateIBAN(tx.creditorAccount.iban)) {
          throw new Error(`Invalid creditor IBAN: ${tx.creditorAccount.iban}`);
        }

        if (tx.creditorAgent.bic && !this.validateBIC(tx.creditorAgent.bic)) {
          throw new Error(`Invalid creditor BIC: ${tx.creditorAgent.bic}`);
        }

        if (tx.instructedAmount.value <= 0) {
          throw new Error('Amount must be positive');
        }
      }
    }

    // Validate control sum
    const totalAmount = payment.paymentInformation.reduce((sum, pmtInf) => {
      return sum + pmtInf.creditTransferTransactions.reduce((txSum, tx) => {
        return txSum + tx.instructedAmount.value;
      }, 0);
    }, 0);

    if (Math.abs(totalAmount - payment.groupHeader.controlSum) > 0.01) {
      throw new Error(`Control sum mismatch: expected ${payment.groupHeader.controlSum}, got ${totalAmount}`);
    }
  }

  /**
   * Generate pain.001 XML
   */
  private generatePain001XML(payment: CustomerCreditTransferInitiation): string {
    const builder = new XMLBuilder({
      format: true,
      ignoreAttributes: false,
      attributeNamePrefix: '@_',
    });

    const document = {
      '?xml': { '@_version': '1.0', '@_encoding': 'UTF-8' },
      Document: {
        '@_xmlns': 'urn:iso:std:iso:20022:tech:xsd:pain.001.001.03',
        '@_xmlns:xsi': 'http://www.w3.org/2001/XMLSchema-instance',
        CstmrCdtTrfInitn: {
          GrpHdr: {
            MsgId: payment.groupHeader.messageIdentification,
            CreDtTm: payment.groupHeader.creationDateTime.toISOString(),
            NbOfTxs: payment.groupHeader.numberOfTransactions.toString(),
            CtrlSum: payment.groupHeader.controlSum.toFixed(2),
            InitgPty: {
              Nm: payment.groupHeader.initiatingParty.name,
            },
          },
          PmtInf: payment.paymentInformation.map(pmtInf => ({
            PmtInfId: pmtInf.paymentInformationIdentification,
            PmtMtd: pmtInf.paymentMethod,
            ReqdExctnDt: pmtInf.requestedExecutionDate.toISOString().split('T')[0],
            Dbtr: { Nm: pmtInf.debtor.name },
            DbtrAcct: {
              Id: { IBAN: pmtInf.debtorAccount.iban },
            },
            DbtrAgt: {
              FinInstnId: { BIC: pmtInf.debtorAgent.bic },
            },
            CdtTrfTxInf: pmtInf.creditTransferTransactions.map(tx => ({
              PmtId: {
                InstrId: tx.paymentIdentification.instructionIdentification,
                EndToEndId: tx.paymentIdentification.endToEndIdentification,
              },
              Amt: {
                InstdAmt: {
                  '@_Ccy': tx.instructedAmount.currency,
                  '#text': tx.instructedAmount.value.toFixed(2),
                },
              },
              CdtrAgt: {
                FinInstnId: { BIC: tx.creditorAgent.bic },
              },
              Cdtr: { Nm: tx.creditor.name },
              CdtrAcct: {
                Id: { IBAN: tx.creditorAccount.iban },
              },
              RmtInf: tx.remittanceInformation?.unstructured ? {
                Ustrd: tx.remittanceInformation.unstructured,
              } : undefined,
            })),
          })),
        },
      },
    };

    return builder.build(document);
  }

  /**
   * Parse pain.002 XML
   */
  private parsePain002XML(xml: string): PaymentStatusReport {
    const parser = new XMLParser({
      ignoreAttributes: false,
      attributeNamePrefix: '@_',
    });

    const parsed = parser.parse(xml);
    const statusReport = parsed.Document.CstmrPmtStsRpt;

    return {
      messageIdentification: statusReport.GrpHdr.MsgId,
      originalMessageIdentification: statusReport.OrgnlGrpInfAndSts.OrgnlMsgId,
      transactionStatus: Array.isArray(statusReport.OrgnlPmtInfAndSts.TxInfAndSts)
        ? statusReport.OrgnlPmtInfAndSts.TxInfAndSts.map((tx: any) => ({
            statusIdentification: tx.StsId,
            originalEndToEndIdentification: tx.OrgnlEndToEndId,
            transactionStatus: tx.TxSts,
            statusReasonCode: tx.StsRsnInf?.Rsn?.Cd,
            additionalStatusReasonInformation: tx.StsRsnInf?.AddtlInf,
          }))
        : [],
    };
  }

  /**
   * Validate IBAN
   */
  validateIBAN(iban: string): boolean {
    return IBAN.isValid(iban);
  }

  /**
   * Validate BIC
   */
  validateBIC(bic: string): boolean {
    const bicRegex = /^[A-Z]{6}[A-Z0-9]{2}([A-Z0-9]{3})?$/;
    return bicRegex.test(bic) && (bic.length === 8 || bic.length === 11);
  }
}

// ============================================================
// Utility Functions
// ============================================================

export function createPayment(params: {
  messageId: string;
  debtor: { name: string; iban: string; bic: string };
  creditor: { name: string; iban: string; bic: string };
  amount: number;
  currency: string;
  remittanceInfo?: string;
}): CustomerCreditTransferInitiation {
  return {
    groupHeader: {
      messageIdentification: params.messageId,
      creationDateTime: new Date(),
      numberOfTransactions: 1,
      controlSum: params.amount,
      initiatingParty: {
        name: params.debtor.name,
      },
    },
    paymentInformation: [
      {
        paymentInformationIdentification: `${params.messageId}-PMT-001`,
        paymentMethod: 'TRF',
        requestedExecutionDate: new Date(),
        debtor: {
          name: params.debtor.name,
        },
        debtorAccount: {
          iban: params.debtor.iban,
        },
        debtorAgent: {
          bic: params.debtor.bic,
        },
        creditTransferTransactions: [
          {
            paymentIdentification: {
              instructionIdentification: `${params.messageId}-INSTR-001`,
              endToEndIdentification: `${params.messageId}-E2E-001`,
            },
            instructedAmount: {
              value: params.amount,
              currency: params.currency,
            },
            creditor: {
              name: params.creditor.name,
            },
            creditorAccount: {
              iban: params.creditor.iban,
            },
            creditorAgent: {
              bic: params.creditor.bic,
            },
            remittanceInformation: params.remittanceInfo
              ? { unstructured: [params.remittanceInfo] }
              : undefined,
          },
        ],
      },
    ],
  };
}

export default ISO20022PaymentGateway;
