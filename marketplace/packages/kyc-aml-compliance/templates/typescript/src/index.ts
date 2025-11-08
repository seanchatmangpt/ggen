// KYC/AML Compliance - TypeScript Onboarding UI
// Customer identity verification and compliance workflow

export interface IdentityDocument {
  documentType: 'PASSPORT' | 'DRIVERS_LICENSE' | 'NATIONAL_ID';
  documentNumber: string;
  issuer: string;
  expiryDate: Date;
  documentImage?: string;
}

export interface Customer {
  customerId: string;
  fullName: string;
  dateOfBirth?: Date;
  nationality?: string;
  countryOfResidence: string;
  identityDocuments: IdentityDocument[];
}

export interface RiskAssessment {
  customerId: string;
  riskScore: number;
  riskLevel: 'LOW' | 'MEDIUM' | 'HIGH';
  riskFactors: Array<{
    type: string;
    score: number;
    description: string;
  }>;
  assessmentDate: Date;
}

export class KYCClient {
  constructor(private apiUrl: string, private apiKey: string) {}

  async submitIdentityDocument(customerId: string, document: IdentityDocument): Promise<string> {
    const response = await fetch(`${this.apiUrl}/kyc/${customerId}/documents`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        'Authorization': `Bearer ${this.apiKey}`,
      },
      body: JSON.stringify(document),
    });
    const result = await response.json();
    return result.verificationId;
  }

  async getRiskAssessment(customerId: string): Promise<RiskAssessment> {
    const response = await fetch(`${this.apiUrl}/kyc/${customerId}/risk`, {
      headers: { 'Authorization': `Bearer ${this.apiKey}` },
    });
    return response.json();
  }

  async screenSanctions(customerId: string): Promise<any[]> {
    const response = await fetch(`${this.apiUrl}/kyc/${customerId}/sanctions`, {
      headers: { 'Authorization': `Bearer ${this.apiKey}` },
    });
    return response.json();
  }
}

export default KYCClient;
