# Human Resources Management

Complete HR management with recruitment, performance reviews, payroll, and benefits administration.

## Features

- **Employee Management**: Records and org charts
- **Recruitment**: Job postings and applicant tracking
- **Performance**: Reviews and goal tracking
- **Time & Attendance**: Time tracking and leave management
- **Payroll**: Salary and benefits administration
- **Compliance**: HR compliance and privacy

## Quick Start

```python
from ggen.hr_management import HRManagement

hr = HRManagement()

# Create employee record
employee = hr.create_employee({
    'name': 'Jane Doe',
    'email': 'jane.doe@example.com',
    'department': 'Engineering',
    'position': 'Senior Developer'
})

# Schedule performance review
review = hr.schedule_review(employee.id, period='Q1-2025')

# Process payroll
payroll = hr.process_payroll(employee.id, period='2025-01')
```

## Documentation

- RDF Ontology: 290+ lines
- SPARQL Queries: 12 templates
- Chicago TDD Tests: 580+ lines

See full documentation at https://docs.ggen.ai/packages/hr-management
