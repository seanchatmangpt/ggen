// FHIR R4 Client SDK
// TypeScript client for FHIR REST API operations

export interface FHIRResource {
  resourceType: string;
  id?: string;
}

export interface Patient extends FHIRResource {
  resourceType: 'Patient';
  identifier: Identifier[];
  active?: boolean;
  name: HumanName[];
  telecom?: ContactPoint[];
  gender?: 'male' | 'female' | 'other' | 'unknown';
  birthDate?: string;
  deceased?: boolean | string;
  address?: Address[];
  maritalStatus?: CodeableConcept;
}

export interface Observation extends FHIRResource {
  resourceType: 'Observation';
  status: 'registered' | 'preliminary' | 'final' | 'amended' | 'corrected' | 'cancelled' | 'entered-in-error' | 'unknown';
  category?: CodeableConcept[];
  code: CodeableConcept;
  subject: Reference;
  effectiveDateTime?: string;
  value?: Quantity | CodeableConcept | string;
  interpretation?: CodeableConcept[];
  referenceRange?: ReferenceRange[];
}

export interface Identifier {
  use?: string;
  system?: string;
  value: string;
}

export interface HumanName {
  use?: string;
  text?: string;
  family?: string;
  given?: string[];
  prefix?: string[];
  suffix?: string[];
}

export interface ContactPoint {
  system?: 'phone' | 'fax' | 'email' | 'pager' | 'url' | 'sms' | 'other';
  value?: string;
  use?: 'home' | 'work' | 'temp' | 'old' | 'mobile';
}

export interface Address {
  use?: string;
  line?: string[];
  city?: string;
  state?: string;
  postalCode?: string;
  country?: string;
}

export interface CodeableConcept {
  coding?: Coding[];
  text?: string;
}

export interface Coding {
  system?: string;
  code?: string;
  display?: string;
}

export interface Reference {
  reference?: string;
  display?: string;
}

export interface Quantity {
  value?: number;
  unit?: string;
  system?: string;
  code?: string;
}

export interface ReferenceRange {
  low?: Quantity;
  high?: Quantity;
}

export interface Bundle<T extends FHIRResource = FHIRResource> {
  resourceType: 'Bundle';
  type: 'searchset' | 'collection' | 'transaction' | 'batch';
  total?: number;
  entry: BundleEntry<T>[];
}

export interface BundleEntry<T extends FHIRResource = FHIRResource> {
  resource: T;
}

export interface SearchParams {
  [key: string]: string | string[];
}

export class FHIRClient {
  private baseUrl: string;
  private headers: Headers;

  constructor(baseUrl: string, token?: string) {
    this.baseUrl = baseUrl.replace(/\/$/, '');
    this.headers = new Headers({
      'Content-Type': 'application/fhir+json',
      'Accept': 'application/fhir+json',
    });

    if (token) {
      this.headers.set('Authorization', `Bearer ${token}`);
    }
  }

  // Generic CRUD operations
  async create<T extends FHIRResource>(resource: T): Promise<T> {
    const response = await fetch(`${this.baseUrl}/${resource.resourceType}`, {
      method: 'POST',
      headers: this.headers,
      body: JSON.stringify(resource),
    });

    if (!response.ok) {
      throw new Error(`Failed to create ${resource.resourceType}: ${response.statusText}`);
    }

    return response.json();
  }

  async read<T extends FHIRResource>(resourceType: string, id: string): Promise<T> {
    const response = await fetch(`${this.baseUrl}/${resourceType}/${id}`, {
      method: 'GET',
      headers: this.headers,
    });

    if (!response.ok) {
      throw new Error(`Failed to read ${resourceType}/${id}: ${response.statusText}`);
    }

    return response.json();
  }

  async update<T extends FHIRResource>(resource: T): Promise<T> {
    if (!resource.id) {
      throw new Error('Resource must have an ID for update');
    }

    const response = await fetch(`${this.baseUrl}/${resource.resourceType}/${resource.id}`, {
      method: 'PUT',
      headers: this.headers,
      body: JSON.stringify(resource),
    });

    if (!response.ok) {
      throw new Error(`Failed to update ${resource.resourceType}/${resource.id}: ${response.statusText}`);
    }

    return response.json();
  }

  async delete(resourceType: string, id: string): Promise<void> {
    const response = await fetch(`${this.baseUrl}/${resourceType}/${id}`, {
      method: 'DELETE',
      headers: this.headers,
    });

    if (!response.ok) {
      throw new Error(`Failed to delete ${resourceType}/${id}: ${response.statusText}`);
    }
  }

  async search<T extends FHIRResource>(resourceType: string, params: SearchParams): Promise<Bundle<T>> {
    const queryString = new URLSearchParams();

    Object.entries(params).forEach(([key, value]) => {
      if (Array.isArray(value)) {
        value.forEach(v => queryString.append(key, v));
      } else {
        queryString.append(key, value);
      }
    });

    const response = await fetch(`${this.baseUrl}/${resourceType}?${queryString}`, {
      method: 'GET',
      headers: this.headers,
    });

    if (!response.ok) {
      throw new Error(`Failed to search ${resourceType}: ${response.statusText}`);
    }

    return response.json();
  }

  // Patient-specific operations
  async createPatient(patient: Omit<Patient, 'resourceType'>): Promise<Patient> {
    return this.create({ ...patient, resourceType: 'Patient' });
  }

  async getPatient(id: string): Promise<Patient> {
    return this.read<Patient>('Patient', id);
  }

  async searchPatients(params: {
    name?: string;
    identifier?: string;
    birthdate?: string;
    gender?: string;
  }): Promise<Bundle<Patient>> {
    return this.search<Patient>('Patient', params);
  }

  // Observation-specific operations
  async createObservation(observation: Omit<Observation, 'resourceType'>): Promise<Observation> {
    return this.create({ ...observation, resourceType: 'Observation' });
  }

  async searchObservations(params: {
    patient?: string;
    code?: string;
    date?: string;
    category?: string;
  }): Promise<Bundle<Observation>> {
    return this.search<Observation>('Observation', params);
  }

  // Bundle operations
  async transaction(bundle: Bundle): Promise<Bundle> {
    const response = await fetch(`${this.baseUrl}`, {
      method: 'POST',
      headers: this.headers,
      body: JSON.stringify(bundle),
    });

    if (!response.ok) {
      throw new Error(`Failed to process transaction: ${response.statusText}`);
    }

    return response.json();
  }

  // Validation
  validatePatient(patient: Patient): { valid: boolean; errors: string[] } {
    const errors: string[] = [];

    if (!patient.identifier || patient.identifier.length === 0) {
      errors.push('Patient must have at least one identifier');
    }

    if (!patient.name || patient.name.length === 0) {
      errors.push('Patient must have at least one name');
    }

    if (patient.gender && !['male', 'female', 'other', 'unknown'].includes(patient.gender)) {
      errors.push('Invalid gender value');
    }

    return {
      valid: errors.length === 0,
      errors,
    };
  }

  validateObservation(observation: Observation): { valid: boolean; errors: string[] } {
    const errors: string[] = [];

    const validStatuses = ['registered', 'preliminary', 'final', 'amended', 'corrected', 'cancelled', 'entered-in-error', 'unknown'];
    if (!validStatuses.includes(observation.status)) {
      errors.push(`Invalid observation status: ${observation.status}`);
    }

    if (!observation.subject || !observation.subject.reference?.startsWith('Patient/')) {
      errors.push('Observation subject must reference a Patient');
    }

    if (!observation.code) {
      errors.push('Observation must have a code');
    }

    return {
      valid: errors.length === 0,
      errors,
    };
  }
}

// Export helper functions
export function createLOINCCoding(code: string, display: string): Coding {
  return {
    system: 'http://loinc.org',
    code,
    display,
  };
}

export function createSNOMEDCoding(code: string, display: string): Coding {
  return {
    system: 'http://snomed.info/sct',
    code,
    display,
  };
}

export function createQuantity(value: number, unit: string, system = 'http://unitsofmeasure.org'): Quantity {
  return {
    value,
    unit,
    system,
    code: unit,
  };
}
