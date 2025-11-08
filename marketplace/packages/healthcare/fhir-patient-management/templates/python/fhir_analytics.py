"""
FHIR Data Analytics Module
Analyze patient data, observations, and generate health insights
"""

from typing import List, Dict, Any, Optional, Tuple
from dataclasses import dataclass
from datetime import datetime, timedelta
import statistics
import json


@dataclass
class Patient:
    resource_type: str = "Patient"
    id: Optional[str] = None
    identifier: List[Dict[str, str]] = None
    name: List[Dict[str, Any]] = None
    gender: Optional[str] = None
    birth_date: Optional[str] = None


@dataclass
class Observation:
    resource_type: str = "Observation"
    id: Optional[str] = None
    status: str = ""
    code: Dict[str, Any] = None
    subject: Dict[str, str] = None
    effective_date_time: Optional[str] = None
    value: Any = None


class FHIRAnalytics:
    """Analytics engine for FHIR data"""

    def __init__(self):
        self.patients: Dict[str, Patient] = {}
        self.observations: Dict[str, Observation] = {}

    def load_patient(self, patient_data: Dict[str, Any]) -> Patient:
        """Load patient from FHIR JSON"""
        patient = Patient(
            id=patient_data.get('id'),
            identifier=patient_data.get('identifier', []),
            name=patient_data.get('name', []),
            gender=patient_data.get('gender'),
            birth_date=patient_data.get('birthDate')
        )
        if patient.id:
            self.patients[patient.id] = patient
        return patient

    def load_observation(self, obs_data: Dict[str, Any]) -> Observation:
        """Load observation from FHIR JSON"""
        observation = Observation(
            id=obs_data.get('id'),
            status=obs_data.get('status'),
            code=obs_data.get('code'),
            subject=obs_data.get('subject'),
            effective_date_time=obs_data.get('effectiveDateTime'),
            value=obs_data.get('valueQuantity') or obs_data.get('valueCodeableConcept')
        )
        if observation.id:
            self.observations[observation.id] = observation
        return observation

    def get_patient_age(self, patient_id: str) -> Optional[int]:
        """Calculate patient age from birth date"""
        patient = self.patients.get(patient_id)
        if not patient or not patient.birth_date:
            return None

        birth_date = datetime.strptime(patient.birth_date, '%Y-%m-%d')
        today = datetime.now()
        age = today.year - birth_date.year

        if (today.month, today.day) < (birth_date.month, birth_date.day):
            age -= 1

        return age

    def get_observations_by_patient(self, patient_id: str) -> List[Observation]:
        """Get all observations for a patient"""
        patient_ref = f"Patient/{patient_id}"
        return [
            obs for obs in self.observations.values()
            if obs.subject and obs.subject.get('reference') == patient_ref
        ]

    def get_observations_by_code(self, loinc_code: str) -> List[Observation]:
        """Get observations by LOINC code"""
        results = []
        for obs in self.observations.values():
            if obs.code and obs.code.get('coding'):
                for coding in obs.code['coding']:
                    if coding.get('system') == 'http://loinc.org' and coding.get('code') == loinc_code:
                        results.append(obs)
                        break
        return results

    def calculate_vital_trends(
        self,
        patient_id: str,
        loinc_code: str,
        days: int = 30
    ) -> Dict[str, Any]:
        """Calculate trends for vital signs"""
        observations = self.get_observations_by_patient(patient_id)

        # Filter by code and date range
        cutoff_date = datetime.now() - timedelta(days=days)
        filtered_obs = []

        for obs in observations:
            if not obs.code or not obs.code.get('coding'):
                continue

            has_code = any(
                c.get('system') == 'http://loinc.org' and c.get('code') == loinc_code
                for c in obs.code['coding']
            )

            if has_code and obs.effective_date_time:
                obs_date = datetime.fromisoformat(obs.effective_date_time.replace('Z', '+00:00'))
                if obs_date >= cutoff_date:
                    filtered_obs.append(obs)

        if not filtered_obs:
            return {
                'count': 0,
                'values': [],
                'trend': 'no_data'
            }

        # Extract numeric values
        values = []
        for obs in filtered_obs:
            if isinstance(obs.value, dict) and 'value' in obs.value:
                values.append(float(obs.value['value']))

        if not values:
            return {
                'count': len(filtered_obs),
                'values': [],
                'trend': 'no_numeric_data'
            }

        # Calculate statistics
        mean_val = statistics.mean(values)
        median_val = statistics.median(values)
        min_val = min(values)
        max_val = max(values)

        # Determine trend
        if len(values) >= 2:
            first_half = values[:len(values)//2]
            second_half = values[len(values)//2:]

            first_avg = statistics.mean(first_half)
            second_avg = statistics.mean(second_half)

            if second_avg > first_avg * 1.1:
                trend = 'increasing'
            elif second_avg < first_avg * 0.9:
                trend = 'decreasing'
            else:
                trend = 'stable'
        else:
            trend = 'insufficient_data'

        return {
            'count': len(values),
            'mean': round(mean_val, 2),
            'median': round(median_val, 2),
            'min': min_val,
            'max': max_val,
            'values': values,
            'trend': trend
        }

    def detect_abnormal_results(
        self,
        patient_id: str,
        reference_ranges: Dict[str, Tuple[float, float]]
    ) -> List[Dict[str, Any]]:
        """Detect abnormal lab results based on reference ranges"""
        observations = self.get_observations_by_patient(patient_id)
        abnormal = []

        for obs in observations:
            if not obs.code or not isinstance(obs.value, dict):
                continue

            # Get LOINC code
            loinc_code = None
            for coding in obs.code.get('coding', []):
                if coding.get('system') == 'http://loinc.org':
                    loinc_code = coding.get('code')
                    break

            if not loinc_code or loinc_code not in reference_ranges:
                continue

            value = obs.value.get('value')
            if value is None:
                continue

            low, high = reference_ranges[loinc_code]
            if value < low or value > high:
                abnormal.append({
                    'observation_id': obs.id,
                    'code': loinc_code,
                    'value': value,
                    'reference_low': low,
                    'reference_high': high,
                    'status': 'low' if value < low else 'high',
                    'date': obs.effective_date_time
                })

        return abnormal

    def generate_patient_summary(self, patient_id: str) -> Dict[str, Any]:
        """Generate comprehensive patient summary"""
        patient = self.patients.get(patient_id)
        if not patient:
            return {'error': 'Patient not found'}

        observations = self.get_observations_by_patient(patient_id)

        # Get patient name
        name = ''
        if patient.name and len(patient.name) > 0:
            name_obj = patient.name[0]
            given = ' '.join(name_obj.get('given', []))
            family = name_obj.get('family', '')
            name = f"{given} {family}".strip()

        # Count observations by category
        categories = {}
        for obs in observations:
            if obs.code and obs.code.get('coding'):
                for coding in obs.code['coding']:
                    system = coding.get('system', 'unknown')
                    categories[system] = categories.get(system, 0) + 1

        return {
            'patient_id': patient_id,
            'name': name,
            'age': self.get_patient_age(patient_id),
            'gender': patient.gender,
            'total_observations': len(observations),
            'observations_by_system': categories,
            'latest_observation_date': max(
                (obs.effective_date_time for obs in observations if obs.effective_date_time),
                default=None
            )
        }

    def cohort_analysis(
        self,
        gender: Optional[str] = None,
        age_range: Optional[Tuple[int, int]] = None
    ) -> Dict[str, Any]:
        """Perform cohort analysis on patient population"""
        filtered_patients = []

        for patient_id, patient in self.patients.items():
            # Gender filter
            if gender and patient.gender != gender:
                continue

            # Age filter
            if age_range:
                age = self.get_patient_age(patient_id)
                if age is None or not (age_range[0] <= age <= age_range[1]):
                    continue

            filtered_patients.append(patient_id)

        # Calculate statistics
        ages = [self.get_patient_age(pid) for pid in filtered_patients]
        ages = [a for a in ages if a is not None]

        total_observations = sum(
            len(self.get_observations_by_patient(pid))
            for pid in filtered_patients
        )

        return {
            'cohort_size': len(filtered_patients),
            'gender_filter': gender,
            'age_range_filter': age_range,
            'average_age': round(statistics.mean(ages), 1) if ages else None,
            'total_observations': total_observations,
            'avg_observations_per_patient': round(total_observations / len(filtered_patients), 1) if filtered_patients else 0
        }


# Example usage
if __name__ == '__main__':
    analytics = FHIRAnalytics()

    # Load sample patient
    patient_data = {
        'id': '123',
        'identifier': [{'value': 'MRN12345'}],
        'name': [{'given': ['John'], 'family': 'Doe'}],
        'gender': 'male',
        'birthDate': '1980-01-01'
    }
    analytics.load_patient(patient_data)

    # Load sample observation
    obs_data = {
        'id': 'obs1',
        'status': 'final',
        'code': {
            'coding': [{
                'system': 'http://loinc.org',
                'code': '8867-4',
                'display': 'Heart rate'
            }]
        },
        'subject': {'reference': 'Patient/123'},
        'effectiveDateTime': '2024-01-15T10:30:00Z',
        'valueQuantity': {'value': 72, 'unit': 'beats/min'}
    }
    analytics.load_observation(obs_data)

    # Generate summary
    summary = analytics.generate_patient_summary('123')
    print(json.dumps(summary, indent=2))
