// Telemedicine Platform - Rust Implementation
use serde::{Deserialize, Serialize};
use std::time::SystemTime;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Consultation {
    pub id: String,
    pub consultation_type: ConsultationType,
    pub provider_id: String,
    pub patient_id: String,
    pub start_time: SystemTime,
    pub duration_minutes: Option<u32>,
    pub status: ConsultationStatus,
    pub encrypted: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ConsultationType {
    Video,
    Audio,
    Chat,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ConsultationStatus {
    Scheduled,
    InProgress,
    Completed,
    Cancelled,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EPrescription {
    pub id: String,
    pub consultation_id: String,
    pub medication_ndc: String,
    pub dosage: String,
    pub frequency: String,
    pub quantity: u32,
    pub pharmacy_id: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Appointment {
    pub id: String,
    pub provider_id: String,
    pub patient_id: String,
    pub scheduled_time: SystemTime,
    pub duration_minutes: u32,
    pub reminder_sent: bool,
}

pub struct TelemedicinePlatform {
    consultations: Vec<Consultation>,
    appointments: Vec<Appointment>,
    prescriptions: Vec<EPrescription>,
}

impl TelemedicinePlatform {
    pub fn new() -> Self {
        Self {
            consultations: Vec::new(),
            appointments: Vec::new(),
            prescriptions: Vec::new(),
        }
    }

    pub fn create_consultation(
        &mut self,
        id: String,
        consultation_type: ConsultationType,
        provider_id: String,
        patient_id: String,
    ) -> Result<&Consultation, String> {
        let consultation = Consultation {
            id: id.clone(),
            consultation_type,
            provider_id,
            patient_id,
            start_time: SystemTime::now(),
            duration_minutes: None,
            status: ConsultationStatus::Scheduled,
            encrypted: true,
        };
        self.consultations.push(consultation);
        self.consultations.last().ok_or("Failed to create consultation".to_string())
    }

    pub fn start_consultation(&mut self, id: &str) -> Result<(), String> {
        let consultation = self.consultations.iter_mut()
            .find(|c| c.id == id)
            .ok_or("Consultation not found")?;
        consultation.status = ConsultationStatus::InProgress;
        consultation.start_time = SystemTime::now();
        Ok(())
    }

    pub fn end_consultation(&mut self, id: &str, duration_minutes: u32) -> Result<(), String> {
        let consultation = self.consultations.iter_mut()
            .find(|c| c.id == id)
            .ok_or("Consultation not found")?;
        consultation.status = ConsultationStatus::Completed;
        consultation.duration_minutes = Some(duration_minutes);
        Ok(())
    }

    pub fn create_prescription(
        &mut self,
        consultation_id: &str,
        medication_ndc: String,
        dosage: String,
        frequency: String,
        quantity: u32,
        pharmacy_id: String,
    ) -> Result<String, String> {
        let prescription_id = format!("RX-{}", uuid::Uuid::new_v4());
        let prescription = EPrescription {
            id: prescription_id.clone(),
            consultation_id: consultation_id.to_string(),
            medication_ndc,
            dosage,
            frequency,
            quantity,
            pharmacy_id,
        };
        self.prescriptions.push(prescription);
        Ok(prescription_id)
    }

    pub fn get_active_consultations(&self) -> Vec<&Consultation> {
        self.consultations.iter()
            .filter(|c| matches!(c.status, ConsultationStatus::InProgress))
            .collect()
    }

    pub fn get_consultations_by_provider(&self, provider_id: &str) -> Vec<&Consultation> {
        self.consultations.iter()
            .filter(|c| c.provider_id == provider_id)
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_consultation() {
        let mut platform = TelemedicinePlatform::new();
        let result = platform.create_consultation(
            "CONS-001".to_string(),
            ConsultationType::Video,
            "PROV-001".to_string(),
            "PAT-001".to_string(),
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_start_consultation() {
        let mut platform = TelemedicinePlatform::new();
        platform.create_consultation(
            "CONS-001".to_string(),
            ConsultationType::Video,
            "PROV-001".to_string(),
            "PAT-001".to_string(),
        ).unwrap();
        let result = platform.start_consultation("CONS-001");
        assert!(result.is_ok());
    }
}
