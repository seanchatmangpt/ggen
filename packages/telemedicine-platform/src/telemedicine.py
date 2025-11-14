"""Telemedicine Platform - Python Implementation"""

from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Dict, List, Optional
import uuid


class ConsultationType(Enum):
    VIDEO = "video"
    AUDIO = "audio"
    CHAT = "chat"


class ConsultationStatus(Enum):
    SCHEDULED = "scheduled"
    IN_PROGRESS = "in-progress"
    COMPLETED = "completed"
    CANCELLED = "cancelled"


@dataclass
class Consultation:
    id: str
    consultation_type: ConsultationType
    provider_id: str
    patient_id: str
    start_time: datetime
    duration_minutes: Optional[int] = None
    status: ConsultationStatus = ConsultationStatus.SCHEDULED
    encrypted: bool = True


@dataclass
class EPrescription:
    id: str
    consultation_id: str
    medication_ndc: str
    dosage: str
    frequency: str
    quantity: int
    pharmacy_id: str


@dataclass
class Appointment:
    id: str
    provider_id: str
    patient_id: str
    scheduled_time: datetime
    duration_minutes: int
    reminder_sent: bool = False


class TelemedicinePlatform:
    def __init__(self):
        self.consultations: Dict[str, Consultation] = {}
        self.appointments: Dict[str, Appointment] = {}
        self.prescriptions: Dict[str, EPrescription] = {}

    def create_consultation(
        self,
        consultation_id: str,
        consultation_type: ConsultationType,
        provider_id: str,
        patient_id: str
    ) -> Consultation:
        consultation = Consultation(
            id=consultation_id,
            consultation_type=consultation_type,
            provider_id=provider_id,
            patient_id=patient_id,
            start_time=datetime.now(),
            status=ConsultationStatus.SCHEDULED,
            encrypted=True
        )
        self.consultations[consultation_id] = consultation
        return consultation

    def start_consultation(self, consultation_id: str) -> None:
        consultation = self.consultations.get(consultation_id)
        if not consultation:
            raise ValueError("Consultation not found")
        consultation.status = ConsultationStatus.IN_PROGRESS
        consultation.start_time = datetime.now()

    def end_consultation(self, consultation_id: str, duration_minutes: int) -> None:
        consultation = self.consultations.get(consultation_id)
        if not consultation:
            raise ValueError("Consultation not found")
        consultation.status = ConsultationStatus.COMPLETED
        consultation.duration_minutes = duration_minutes

    def create_prescription(
        self,
        consultation_id: str,
        medication_ndc: str,
        dosage: str,
        frequency: str,
        quantity: int,
        pharmacy_id: str
    ) -> str:
        prescription_id = f"RX-{uuid.uuid4().hex[:8]}"
        prescription = EPrescription(
            id=prescription_id,
            consultation_id=consultation_id,
            medication_ndc=medication_ndc,
            dosage=dosage,
            frequency=frequency,
            quantity=quantity,
            pharmacy_id=pharmacy_id
        )
        self.prescriptions[prescription_id] = prescription
        return prescription_id

    def get_active_consultations(self) -> List[Consultation]:
        return [
            c for c in self.consultations.values()
            if c.status == ConsultationStatus.IN_PROGRESS
        ]

    def get_consultations_by_provider(self, provider_id: str) -> List[Consultation]:
        return [
            c for c in self.consultations.values()
            if c.provider_id == provider_id
        ]
