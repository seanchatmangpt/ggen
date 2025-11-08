"""
HL7 v2 to FHIR Transformation Module
Transform HL7 v2 messages to FHIR resources and vice versa
"""

from typing import Dict, List, Any, Optional
from datetime import datetime
import re


class HL7Segment:
    def __init__(self, segment_id: str, fields: List[str]):
        self.id = segment_id
        self.fields = fields

    def get_field(self, index: int, component: Optional[int] = None) -> Optional[str]:
        """Get field value by index, optionally with component"""
        if index >= len(self.fields):
            return None

        field_value = self.fields[index]

        if component is not None:
            components = field_value.split('^')
            if component - 1 < len(components):
                return components[component - 1]
            return None

        return field_value


class HL7Message:
    def __init__(self):
        self.message_type = ''
        self.trigger_event = ''
        self.segments: List[HL7Segment] = []

    def get_segment(self, segment_id: str) -> Optional[HL7Segment]:
        """Get first segment with given ID"""
        for segment in self.segments:
            if segment.id == segment_id:
                return segment
        return None

    def get_all_segments(self, segment_id: str) -> List[HL7Segment]:
        """Get all segments with given ID"""
        return [s for s in self.segments if s.id == segment_id]


class HL7Transformer:
    """Transform between HL7 v2 and FHIR"""

    def __init__(self):
        self.field_separator = '|'
        self.component_separator = '^'
        self.repetition_separator = '~'
        self.escape_character = '\\'
        self.subcomponent_separator = '&'

    def parse_message(self, message_text: str) -> HL7Message:
        """Parse HL7 message from text"""
        lines = [l.strip() for l in message_text.split('\n') if l.strip()]

        if not lines:
            raise ValueError('Empty message')

        if not lines[0].startswith('MSH'):
            raise ValueError('Message must start with MSH segment')

        # Extract encoding characters from MSH-2
        encoding_chars = lines[0][4:9]
        self.component_separator = encoding_chars[0]
        self.repetition_separator = encoding_chars[1]
        self.escape_character = encoding_chars[2]
        self.subcomponent_separator = encoding_chars[3]

        message = HL7Message()

        for line in lines:
            segment = self._parse_segment(line)
            message.segments.append(segment)

        # Extract message type
        msh = message.segments[0]
        if len(msh.fields) > 8:
            msh9 = msh.fields[8]
            parts = msh9.split(self.component_separator)
            message.message_type = parts[0] if len(parts) > 0 else ''
            message.trigger_event = parts[1] if len(parts) > 1 else ''

        return message

    def _parse_segment(self, line: str) -> HL7Segment:
        """Parse single segment"""
        segment_id = line[0:3]

        if segment_id == 'MSH':
            # MSH is special
            fields = [segment_id, self.field_separator]
            rest = line[4:]
            fields.extend(rest.split(self.field_separator))
        else:
            fields = line.split(self.field_separator)

        return HL7Segment(segment_id, fields)

    def pid_to_fhir_patient(self, message: HL7Message) -> Dict[str, Any]:
        """Convert HL7 PID segment to FHIR Patient resource"""
        pid = message.get_segment('PID')
        if not pid:
            raise ValueError('No PID segment in message')

        patient = {
            'resourceType': 'Patient',
            'identifier': [],
            'name': [],
            'telecom': [],
            'address': []
        }

        # PID-3: Patient ID
        if len(pid.fields) > 3:
            patient_id = pid.get_field(3, 1)
            if patient_id:
                patient['identifier'].append({
                    'value': patient_id,
                    'system': 'http://hospital.org/mrn'
                })

        # PID-5: Patient Name
        if len(pid.fields) > 5:
            family_name = pid.get_field(5, 1)
            given_name = pid.get_field(5, 2)
            middle_name = pid.get_field(5, 3)

            name = {}
            if family_name:
                name['family'] = family_name
            if given_name:
                name['given'] = [given_name]
                if middle_name:
                    name['given'].append(middle_name)

            if name:
                patient['name'].append(name)

        # PID-7: Date of Birth
        if len(pid.fields) > 7:
            dob = pid.get_field(7)
            if dob:
                patient['birthDate'] = self._format_fhir_date(dob)

        # PID-8: Administrative Sex
        if len(pid.fields) > 8:
            sex = pid.get_field(8)
            if sex:
                gender_map = {'M': 'male', 'F': 'female', 'O': 'other', 'U': 'unknown'}
                patient['gender'] = gender_map.get(sex.upper(), 'unknown')

        # PID-11: Patient Address
        if len(pid.fields) > 11:
            street = pid.get_field(11, 1)
            city = pid.get_field(11, 3)
            state = pid.get_field(11, 4)
            postal_code = pid.get_field(11, 5)

            address = {}
            if street:
                address['line'] = [street]
            if city:
                address['city'] = city
            if state:
                address['state'] = state
            if postal_code:
                address['postalCode'] = postal_code

            if address:
                patient['address'].append(address)

        # PID-13: Phone Number
        if len(pid.fields) > 13:
            phone = pid.get_field(13, 1)
            if phone:
                patient['telecom'].append({
                    'system': 'phone',
                    'value': phone
                })

        # Remove empty arrays
        patient = {k: v for k, v in patient.items() if v != []}

        return patient

    def obx_to_fhir_observation(self, message: HL7Message) -> List[Dict[str, Any]]:
        """Convert HL7 OBX segments to FHIR Observation resources"""
        obr = message.get_segment('OBR')
        obx_segments = message.get_all_segments('OBX')

        if not obx_segments:
            return []

        observations = []

        for obx in obx_segments:
            observation = {
                'resourceType': 'Observation',
                'status': 'final',
                'code': {},
                'subject': {}
            }

            # OBX-3: Observation Identifier
            if len(obx.fields) > 3:
                code = obx.get_field(3, 1)
                text = obx.get_field(3, 2)
                system = obx.get_field(3, 3)

                observation['code'] = {
                    'coding': [{
                        'code': code,
                        'display': text,
                        'system': self._map_coding_system(system) if system else 'http://loinc.org'
                    }]
                }

            # OBX-2: Value Type
            value_type = obx.get_field(2)

            # OBX-5: Observation Value
            if len(obx.fields) > 5:
                value = obx.get_field(5)

                if value_type == 'NM':  # Numeric
                    observation['valueQuantity'] = {'value': float(value)}

                    # OBX-6: Units
                    if len(obx.fields) > 6:
                        units = obx.get_field(6)
                        if units:
                            observation['valueQuantity']['unit'] = units

                elif value_type == 'ST' or value_type == 'TX':  # String/Text
                    observation['valueString'] = value

            # OBX-8: Abnormal Flags
            if len(obx.fields) > 8:
                abnormal_flag = obx.get_field(8)
                if abnormal_flag:
                    interpretation_map = {
                        'L': 'Low',
                        'H': 'High',
                        'LL': 'Critical low',
                        'HH': 'Critical high',
                        'N': 'Normal',
                        'A': 'Abnormal'
                    }
                    interpretation = interpretation_map.get(abnormal_flag, abnormal_flag)
                    observation['interpretation'] = [{
                        'text': interpretation
                    }]

            # OBX-14: Date/Time of Observation
            if len(obx.fields) > 14:
                obs_date = obx.get_field(14)
                if obs_date:
                    observation['effectiveDateTime'] = self._format_fhir_datetime(obs_date)

            observations.append(observation)

        return observations

    def fhir_patient_to_pid(self, fhir_patient: Dict[str, Any]) -> HL7Segment:
        """Convert FHIR Patient to HL7 PID segment"""
        fields = ['PID', '1', '']

        # PID-3: Patient Identifier
        if 'identifier' in fhir_patient and fhir_patient['identifier']:
            mrn = fhir_patient['identifier'][0].get('value', '')
            fields.append(f"{mrn}^^^MRN")
        else:
            fields.append('')

        fields.append('')  # PID-4

        # PID-5: Patient Name
        if 'name' in fhir_patient and fhir_patient['name']:
            name = fhir_patient['name'][0]
            family = name.get('family', '')
            given = name.get('given', [''])[0] if 'given' in name else ''
            middle = name.get('given', ['', ''])[1] if 'given' in name and len(name['given']) > 1 else ''
            fields.append(f"{family}^{given}^{middle}")
        else:
            fields.append('')

        fields.append('')  # PID-6

        # PID-7: Date of Birth
        if 'birthDate' in fhir_patient:
            dob = fhir_patient['birthDate'].replace('-', '')
            fields.append(dob)
        else:
            fields.append('')

        # PID-8: Gender
        if 'gender' in fhir_patient:
            gender_map = {'male': 'M', 'female': 'F', 'other': 'O', 'unknown': 'U'}
            gender = gender_map.get(fhir_patient['gender'], 'U')
            fields.append(gender)
        else:
            fields.append('')

        return HL7Segment('PID', fields)

    def _format_fhir_date(self, hl7_date: str) -> str:
        """Convert HL7 date (YYYYMMDD) to FHIR date (YYYY-MM-DD)"""
        if len(hl7_date) == 8:
            return f"{hl7_date[0:4]}-{hl7_date[4:6]}-{hl7_date[6:8]}"
        return hl7_date

    def _format_fhir_datetime(self, hl7_datetime: str) -> str:
        """Convert HL7 datetime (YYYYMMDDHHMMSS) to FHIR datetime"""
        if len(hl7_datetime) >= 8:
            formatted = f"{hl7_datetime[0:4]}-{hl7_datetime[4:6]}-{hl7_datetime[6:8]}"
            if len(hl7_datetime) >= 12:
                formatted += f"T{hl7_datetime[8:10]}:{hl7_datetime[10:12]}:"
                formatted += hl7_datetime[12:14] if len(hl7_datetime) >= 14 else "00"
                formatted += "Z"
            return formatted
        return hl7_datetime

    def _map_coding_system(self, hl7_system: str) -> str:
        """Map HL7 coding system to FHIR URI"""
        system_map = {
            'LN': 'http://loinc.org',
            'SNM': 'http://snomed.info/sct',
            'ICD10': 'http://hl7.org/fhir/sid/icd-10',
            'CPT': 'http://www.ama-assn.org/go/cpt'
        }
        return system_map.get(hl7_system, 'http://loinc.org')


# Example usage
if __name__ == '__main__':
    transformer = HL7Transformer()

    # Sample ADT message
    adt_message = """MSH|^~\\&|SENDING_APP|SENDING_FACILITY|RECEIVING_APP|RECEIVING_FACILITY|20240115120000||ADT^A01|MSG00001|P|2.5
PID|1||12345^^^MRN||Doe^John^A||19800101|M|||123 Main St^^Springfield^IL^62701||(555)123-4567
PV1|1|O|||||||||||||||||V1234^^^VN"""

    message = transformer.parse_message(adt_message)
    patient = transformer.pid_to_fhir_patient(message)

    print('FHIR Patient:', patient)
