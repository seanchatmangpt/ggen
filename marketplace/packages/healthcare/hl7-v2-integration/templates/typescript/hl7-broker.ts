// HL7 v2 Message Broker
// Route, transform, and process HL7 v2 messages

export interface HL7Segment {
  id: string;
  fields: string[];
}

export interface HL7Message {
  messageType: string;
  triggerEvent: string;
  segments: HL7Segment[];
}

export interface MessageHandler {
  (message: HL7Message): Promise<void>;
}

export class HL7MessageBroker {
  private handlers: Map<string, MessageHandler[]> = new Map();
  private fieldSeparator = '|';
  private componentSeparator = '^';
  private repetitionSeparator = '~';
  private escapeCharacter = '\\';
  private subcomponentSeparator = '&';

  // Register message handler
  registerHandler(messageType: string, handler: MessageHandler): void {
    const key = messageType.toUpperCase();
    if (!this.handlers.has(key)) {
      this.handlers.set(key, []);
    }
    this.handlers.get(key)!.push(handler);
  }

  // Parse HL7 message
  parse(messageText: string): HL7Message {
    const lines = messageText.split(/\r?\n/).filter(l => l.trim());

    if (lines.length === 0) {
      throw new Error('Empty message');
    }

    const firstLine = lines[0];
    if (!firstLine.startsWith('MSH')) {
      throw new Error('Message must start with MSH segment');
    }

    // Extract encoding characters from MSH-2
    const encodingChars = firstLine.substring(4, 9);
    this.componentSeparator = encodingChars[0];
    this.repetitionSeparator = encodingChars[1];
    this.escapeCharacter = encodingChars[2];
    this.subcomponentSeparator = encodingChars[3];

    const segments: HL7Segment[] = lines.map(line => this.parseSegment(line));

    const [messageType, triggerEvent] = this.extractMessageType(segments);

    return {
      messageType,
      triggerEvent,
      segments
    };
  }

  private parseSegment(line: string): HL7Segment {
    const segmentId = line.substring(0, 3);

    let fields: string[];
    if (segmentId === 'MSH') {
      // MSH is special - field separator is between MSH and encoding characters
      fields = [segmentId, this.fieldSeparator];
      const rest = line.substring(4);
      fields.push(...rest.split(this.fieldSeparator));
    } else {
      fields = line.split(this.fieldSeparator);
    }

    return {
      id: segmentId,
      fields
    };
  }

  private extractMessageType(segments: HL7Segment[]): [string, string] {
    const msh = segments[0];
    if (!msh || msh.id !== 'MSH') {
      throw new Error('No MSH segment found');
    }

    // MSH-9 contains message type
    const msh9 = msh.fields[8] || '';
    const parts = msh9.split(this.componentSeparator);

    return [parts[0] || '', parts[1] || ''];
  }

  // Get field value
  getField(message: HL7Message, path: string): string | undefined {
    const [segmentId, fieldSpec] = path.split('-');

    const segment = message.segments.find(s => s.id === segmentId);
    if (!segment) {
      return undefined;
    }

    const [fieldNumStr, componentNumStr] = fieldSpec.split('.');
    const fieldNum = parseInt(fieldNumStr, 10);

    if (fieldNum >= segment.fields.length) {
      return undefined;
    }

    const fieldValue = segment.fields[fieldNum];

    if (!componentNumStr) {
      return fieldValue;
    }

    const componentNum = parseInt(componentNumStr, 10);
    const components = fieldValue.split(this.componentSeparator);

    return components[componentNum - 1];
  }

  // Generate HL7 message
  generate(message: HL7Message): string {
    const lines = message.segments.map(segment => {
      if (segment.id === 'MSH') {
        return `MSH${this.fieldSeparator}${segment.fields.slice(2).join(this.fieldSeparator)}`;
      }
      return segment.fields.join(this.fieldSeparator);
    });

    return lines.join('\r\n');
  }

  // Process incoming message
  async processMessage(messageText: string): Promise<string> {
    try {
      const message = this.parse(messageText);

      // Validate message
      const errors = this.validate(message);
      if (errors.length > 0) {
        return this.generateNACK(message, errors.join(', '));
      }

      // Route to handlers
      const key = `${message.messageType}^${message.triggerEvent}`;
      const handlers = this.handlers.get(key) || [];

      await Promise.all(handlers.map(h => h(message)));

      // Generate ACK
      return this.generateACK(message, 'AA', 'Message processed successfully');

    } catch (error) {
      console.error('Error processing message:', error);
      const errorMessage = error instanceof Error ? error.message : 'Unknown error';
      return this.generateErrorACK(errorMessage);
    }
  }

  // Validate message structure
  validate(message: HL7Message): string[] {
    const errors: string[] = [];

    if (!message.segments.length || message.segments[0].id !== 'MSH') {
      errors.push('Message must start with MSH segment');
    }

    // Validate based on message type
    switch (`${message.messageType}^${message.triggerEvent}`) {
      case 'ADT^A01':
      case 'ADT^A04':
        if (!message.segments.some(s => s.id === 'PID')) {
          errors.push('ADT message must contain PID segment');
        }
        if (!message.segments.some(s => s.id === 'PV1')) {
          errors.push('ADT message must contain PV1 segment');
        }
        break;

      case 'ORM^O01':
        if (!message.segments.some(s => s.id === 'ORC')) {
          errors.push('ORM message must contain ORC segment');
        }
        break;

      case 'ORU^R01':
        if (!message.segments.some(s => s.id === 'OBR')) {
          errors.push('ORU message must contain OBR segment');
        }
        if (!message.segments.some(s => s.id === 'OBX')) {
          errors.push('ORU message must contain OBX segment');
        }
        break;
    }

    return errors;
  }

  // Generate ACK message
  private generateACK(originalMessage: HL7Message, ackCode: string, textMessage: string): string {
    const originalMSH = originalMessage.segments[0];

    const timestamp = this.formatTimestamp(new Date());
    const messageControlId = this.generateMessageControlId();

    const mshFields = [
      'MSH',
      this.fieldSeparator,
      `${this.componentSeparator}${this.repetitionSeparator}${this.escapeCharacter}${this.subcomponentSeparator}`,
      originalMSH.fields[4], // Sending App <- Receiving App
      originalMSH.fields[5], // Sending Facility
      originalMSH.fields[2], // Receiving App <- Sending App
      originalMSH.fields[3], // Receiving Facility
      timestamp,
      '',
      'ACK',
      messageControlId,
      originalMSH.fields[11], // Processing ID
    ];

    const msaFields = [
      'MSA',
      ackCode,
      originalMSH.fields[9], // Message Control ID from original
      textMessage
    ];

    const ackMessage: HL7Message = {
      messageType: 'ACK',
      triggerEvent: '',
      segments: [
        { id: 'MSH', fields: mshFields },
        { id: 'MSA', fields: msaFields }
      ]
    };

    return this.generate(ackMessage);
  }

  private generateNACK(originalMessage: HL7Message, errorMessage: string): string {
    return this.generateACK(originalMessage, 'AE', errorMessage);
  }

  private generateErrorACK(errorMessage: string): string {
    const timestamp = this.formatTimestamp(new Date());
    const messageControlId = this.generateMessageControlId();

    return `MSH|^~\\&|SYSTEM||SYSTEM||${timestamp}||ACK|${messageControlId}|P|2.5\r\nMSA|AR|UNKNOWN|${errorMessage}`;
  }

  private formatTimestamp(date: Date): string {
    const pad = (n: number) => n.toString().padStart(2, '0');
    return `${date.getFullYear()}${pad(date.getMonth() + 1)}${pad(date.getDate())}${pad(date.getHours())}${pad(date.getMinutes())}${pad(date.getSeconds())}`;
  }

  private generateMessageControlId(): string {
    return `MSG${Date.now()}${Math.floor(Math.random() * 1000)}`;
  }

  // Convert HL7 to FHIR Patient
  hl7ToFHIRPatient(message: HL7Message): any {
    const patientId = this.getField(message, 'PID-3');
    const lastName = this.getField(message, 'PID-5.1');
    const firstName = this.getField(message, 'PID-5.2');
    const dob = this.getField(message, 'PID-7');
    const gender = this.getField(message, 'PID-8');

    const fhirGender = gender?.toLowerCase() === 'm' ? 'male' :
                       gender?.toLowerCase() === 'f' ? 'female' : 'unknown';

    return {
      resourceType: 'Patient',
      identifier: patientId ? [{ value: patientId }] : [],
      name: [{
        family: lastName,
        given: firstName ? [firstName] : []
      }],
      gender: fhirGender,
      birthDate: dob ? this.formatFHIRDate(dob) : undefined
    };
  }

  private formatFHIRDate(hl7Date: string): string {
    // HL7 format: YYYYMMDD
    if (hl7Date.length === 8) {
      return `${hl7Date.substring(0, 4)}-${hl7Date.substring(4, 6)}-${hl7Date.substring(6, 8)}`;
    }
    return hl7Date;
  }
}

// Export helper functions
export function createADTMessage(
  patientId: string,
  lastName: string,
  firstName: string,
  dob: string,
  gender: string
): HL7Message {
  const timestamp = new Date().toISOString().replace(/[-:]/g, '').substring(0, 14);

  return {
    messageType: 'ADT',
    triggerEvent: 'A01',
    segments: [
      {
        id: 'MSH',
        fields: [
          'MSH', '|', '^~\\&', 'SENDING_APP', 'SENDING_FACILITY',
          'RECEIVING_APP', 'RECEIVING_FACILITY', timestamp, '',
          'ADT^A01', `MSG${Date.now()}`, 'P', '2.5'
        ]
      },
      {
        id: 'PID',
        fields: [
          'PID', '1', '', `${patientId}^^^MRN`, '', `${lastName}^${firstName}`,
          '', dob, gender
        ]
      },
      {
        id: 'PV1',
        fields: [
          'PV1', '1', 'O', '', '', '', '', '', '', '', '', '', '',
          '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''
        ]
      }
    ]
  };
}
