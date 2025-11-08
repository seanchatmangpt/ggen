// Telemedicine Platform - TypeScript Implementation

export enum ConsultationType {
  Video = 'video',
  Audio = 'audio',
  Chat = 'chat',
}

export enum ConsultationStatus {
  Scheduled = 'scheduled',
  InProgress = 'in-progress',
  Completed = 'completed',
  Cancelled = 'cancelled',
}

export interface Consultation {
  id: string;
  consultationType: ConsultationType;
  providerId: string;
  patientId: string;
  startTime: Date;
  durationMinutes?: number;
  status: ConsultationStatus;
  encrypted: boolean;
}

export interface EPrescription {
  id: string;
  consultationId: string;
  medicationNDC: string;
  dosage: string;
  frequency: string;
  quantity: number;
  pharmacyId: string;
}

export interface Appointment {
  id: string;
  providerId: string;
  patientId: string;
  scheduledTime: Date;
  durationMinutes: number;
  reminderSent: boolean;
}

export class TelemedicinePlatform {
  private consultations: Map<string, Consultation> = new Map();
  private appointments: Map<string, Appointment> = new Map();
  private prescriptions: Map<string, EPrescription> = new Map();

  createConsultation(
    id: string,
    consultationType: ConsultationType,
    providerId: string,
    patientId: string
  ): Consultation {
    const consultation: Consultation = {
      id,
      consultationType,
      providerId,
      patientId,
      startTime: new Date(),
      status: ConsultationStatus.Scheduled,
      encrypted: true,
    };
    this.consultations.set(id, consultation);
    return consultation;
  }

  startConsultation(id: string): void {
    const consultation = this.consultations.get(id);
    if (!consultation) {
      throw new Error('Consultation not found');
    }
    consultation.status = ConsultationStatus.InProgress;
    consultation.startTime = new Date();
  }

  endConsultation(id: string, durationMinutes: number): void {
    const consultation = this.consultations.get(id);
    if (!consultation) {
      throw new Error('Consultation not found');
    }
    consultation.status = ConsultationStatus.Completed;
    consultation.durationMinutes = durationMinutes;
  }

  createPrescription(
    consultationId: string,
    medicationNDC: string,
    dosage: string,
    frequency: string,
    quantity: number,
    pharmacyId: string
  ): string {
    const id = `RX-${Math.random().toString(36).substr(2, 9)}`;
    const prescription: EPrescription = {
      id,
      consultationId,
      medicationNDC,
      dosage,
      frequency,
      quantity,
      pharmacyId,
    };
    this.prescriptions.set(id, prescription);
    return id;
  }

  getActiveConsultations(): Consultation[] {
    return Array.from(this.consultations.values()).filter(
      (c) => c.status === ConsultationStatus.InProgress
    );
  }

  getConsultationsByProvider(providerId: string): Consultation[] {
    return Array.from(this.consultations.values()).filter(
      (c) => c.providerId === providerId
    );
  }
}
