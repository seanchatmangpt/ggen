/**
 * Initial schema for onboarding platform
 * Creates all tables for customers, setup, values, approvals, receipts, alerts, and support
 */

-- Create enum types
CREATE TYPE customer_status AS ENUM (
  'ONBOARDING',
  'VALUE_DEFINITION',
  'APPROVAL_PENDING',
  'APPROVED',
  'GO_LIVE_READY',
  'LIVE',
  'PAUSED'
);

CREATE TYPE setup_step_status AS ENUM (
  'NOT_STARTED',
  'IN_PROGRESS',
  'COMPLETED',
  'FAILED'
);

CREATE TYPE value_definition_status AS ENUM (
  'DRAFT',
  'ACTIVE',
  'ARCHIVED'
);

CREATE TYPE approval_stage AS ENUM (
  'FINANCE_REVIEW',
  'TECHNICAL_REVIEW',
  'FINAL_APPROVAL'
);

CREATE TYPE approval_status AS ENUM (
  'PENDING',
  'APPROVED',
  'REJECTED'
);

CREATE TYPE alert_type AS ENUM (
  'VALUE_THRESHOLD',
  'ANOMALY',
  'MEASUREMENT_ERROR',
  'APPROVAL_NEEDED'
);

CREATE TYPE alert_severity AS ENUM (
  'INFO',
  'WARNING',
  'CRITICAL'
);

CREATE TYPE ticket_status AS ENUM (
  'OPEN',
  'IN_PROGRESS',
  'WAITING_FOR_CUSTOMER',
  'RESOLVED',
  'CLOSED'
);

CREATE TYPE ticket_priority AS ENUM (
  'LOW',
  'MEDIUM',
  'HIGH',
  'URGENT'
);

CREATE TYPE ticket_category AS ENUM (
  'TECHNICAL',
  'FINANCIAL',
  'INTEGRATION',
  'OTHER'
);

CREATE TYPE approver_role AS ENUM (
  'CFO',
  'CTO',
  'FINANCE_MANAGER',
  'TECHNICAL_LEAD'
);

CREATE TYPE approver_status AS ENUM (
  'INVITED',
  'ACCEPTED',
  'DECLINED'
);

CREATE TYPE receipt_type AS ENUM (
  'SETUP',
  'VALUE_CALCULATION',
  'APPROVAL',
  'GO_LIVE',
  'MEASUREMENT'
);

CREATE TYPE integration_type AS ENUM (
  'SALESFORCE',
  'HUBSPOT',
  'SEGMENT',
  'CUSTOM_WEBHOOK',
  'DATADOG',
  'STRIPE'
);

CREATE TYPE user_role AS ENUM (
  'ADMIN',
  'CUSTOMER_ADMIN',
  'FINANCE',
  'TECHNICAL'
);

CREATE TYPE auth_provider AS ENUM (
  'OAUTH2',
  'JWT'
);

-- Customers table
CREATE TABLE customers (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  name VARCHAR(255) NOT NULL,
  email VARCHAR(255) NOT NULL UNIQUE,
  status customer_status NOT NULL DEFAULT 'ONBOARDING',
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
  updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
  INDEX idx_customers_email (email),
  INDEX idx_customers_status (status),
  INDEX idx_customers_created_at (created_at)
);

-- Users table
CREATE TABLE users (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  email VARCHAR(255) NOT NULL UNIQUE,
  name VARCHAR(255) NOT NULL,
  role user_role NOT NULL,
  customer_id UUID REFERENCES customers(id) ON DELETE CASCADE,
  auth_provider auth_provider NOT NULL,
  password_hash VARCHAR(255),
  last_login_at TIMESTAMP WITH TIME ZONE,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
  INDEX idx_users_email (email),
  INDEX idx_users_customer_id (customer_id)
);

-- Setup Steps table
CREATE TABLE setup_steps (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  customer_id UUID NOT NULL REFERENCES customers(id) ON DELETE CASCADE,
  step INTEGER NOT NULL CHECK (step >= 1 AND step <= 5),
  title VARCHAR(255) NOT NULL,
  description TEXT NOT NULL,
  status setup_step_status NOT NULL DEFAULT 'NOT_STARTED',
  validation_url VARCHAR(2048) NOT NULL,
  completed_at TIMESTAMP WITH TIME ZONE,
  error TEXT,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
  updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
  UNIQUE (customer_id, step),
  INDEX idx_setup_steps_customer_id (customer_id),
  INDEX idx_setup_steps_status (status)
);

-- Value Metrics table
CREATE TABLE value_metrics (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  name VARCHAR(255) NOT NULL,
  unit VARCHAR(255) NOT NULL,
  description TEXT NOT NULL,
  formula TEXT,
  data_source VARCHAR(255) NOT NULL,
  refresh_interval INTEGER NOT NULL CHECK (refresh_interval > 0),
  weights JSONB,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now()
);

-- Value Definitions table
CREATE TABLE value_definitions (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  customer_id UUID NOT NULL REFERENCES customers(id) ON DELETE CASCADE,
  name VARCHAR(255) NOT NULL,
  description TEXT NOT NULL,
  metrics JSONB NOT NULL,
  baseline_value DECIMAL(15, 2) NOT NULL CHECK (baseline_value >= 0),
  target_value DECIMAL(15, 2) NOT NULL CHECK (target_value > 0),
  scoring_model VARCHAR(50) NOT NULL DEFAULT 'LINEAR',
  custom_formula TEXT,
  status value_definition_status NOT NULL DEFAULT 'DRAFT',
  validated_at TIMESTAMP WITH TIME ZONE,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
  updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
  INDEX idx_value_definitions_customer_id (customer_id),
  INDEX idx_value_definitions_status (status)
);

-- Value Calculations table (immutable ledger)
CREATE TABLE value_calculations (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  customer_id UUID NOT NULL REFERENCES customers(id) ON DELETE CASCADE,
  value_definition_id UUID NOT NULL REFERENCES value_definitions(id) ON DELETE CASCADE,
  timestamp TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
  period VARCHAR(50) NOT NULL,
  metric_values JSONB NOT NULL,
  calculated_value DECIMAL(15, 2) NOT NULL CHECK (calculated_value >= 0),
  previous_value DECIMAL(15, 2) NOT NULL CHECK (previous_value >= 0),
  change_percentage DECIMAL(10, 4) NOT NULL,
  receipt_hash VARCHAR(64) NOT NULL,
  receipt_signature VARCHAR(1024) NOT NULL,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
  INDEX idx_value_calculations_customer_id (customer_id),
  INDEX idx_value_calculations_timestamp (timestamp),
  INDEX idx_value_calculations_receipt_hash (receipt_hash)
);

-- Receipts table (cryptographic ledger)
CREATE TABLE receipts (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  customer_id UUID NOT NULL REFERENCES customers(id) ON DELETE CASCADE,
  type receipt_type NOT NULL,
  data JSONB NOT NULL,
  hash VARCHAR(64) NOT NULL UNIQUE,
  signature VARCHAR(1024) NOT NULL,
  previous_hash VARCHAR(64),
  timestamp TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
  verified_at TIMESTAMP WITH TIME ZONE,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
  INDEX idx_receipts_customer_id (customer_id),
  INDEX idx_receipts_hash (hash),
  INDEX idx_receipts_timestamp (timestamp)
);

-- Approvers table
CREATE TABLE approvers (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  name VARCHAR(255) NOT NULL,
  email VARCHAR(255) NOT NULL,
  role approver_role NOT NULL,
  status approver_status NOT NULL DEFAULT 'INVITED',
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now()
);

-- Approvals table
CREATE TABLE approvals (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  customer_id UUID NOT NULL REFERENCES customers(id) ON DELETE CASCADE,
  stage approval_stage NOT NULL,
  approver_ids UUID[] NOT NULL,
  status approval_status NOT NULL DEFAULT 'PENDING',
  comments TEXT,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
  updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
  approved_at TIMESTAMP WITH TIME ZONE,
  rejected_at TIMESTAMP WITH TIME ZONE,
  INDEX idx_approvals_customer_id (customer_id),
  INDEX idx_approvals_status (status),
  INDEX idx_approvals_stage (stage)
);

-- Alerts table
CREATE TABLE alerts (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  customer_id UUID NOT NULL REFERENCES customers(id) ON DELETE CASCADE,
  type alert_type NOT NULL,
  severity alert_severity NOT NULL,
  message TEXT NOT NULL,
  data JSONB NOT NULL,
  is_resolved BOOLEAN NOT NULL DEFAULT FALSE,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
  resolved_at TIMESTAMP WITH TIME ZONE,
  INDEX idx_alerts_customer_id (customer_id),
  INDEX idx_alerts_is_resolved (is_resolved),
  INDEX idx_alerts_severity (severity)
);

-- Support Tickets table
CREATE TABLE support_tickets (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  customer_id UUID NOT NULL REFERENCES customers(id) ON DELETE CASCADE,
  title VARCHAR(255) NOT NULL,
  description TEXT NOT NULL,
  status ticket_status NOT NULL DEFAULT 'OPEN',
  priority ticket_priority NOT NULL DEFAULT 'MEDIUM',
  category ticket_category NOT NULL,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
  updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
  resolved_at TIMESTAMP WITH TIME ZONE,
  INDEX idx_support_tickets_customer_id (customer_id),
  INDEX idx_support_tickets_status (status),
  INDEX idx_support_tickets_priority (priority)
);

-- Ticket Messages table
CREATE TABLE ticket_messages (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  ticket_id UUID NOT NULL REFERENCES support_tickets(id) ON DELETE CASCADE,
  author VARCHAR(255) NOT NULL,
  message TEXT NOT NULL,
  attachments VARCHAR(2048)[],
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
  INDEX idx_ticket_messages_ticket_id (ticket_id),
  INDEX idx_ticket_messages_created_at (created_at)
);

-- API Keys table
CREATE TABLE api_keys (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  customer_id UUID NOT NULL REFERENCES customers(id) ON DELETE CASCADE,
  name VARCHAR(255) NOT NULL,
  key VARCHAR(255) NOT NULL UNIQUE,
  secret_hash VARCHAR(255) NOT NULL,
  scopes VARCHAR(255)[] NOT NULL,
  is_active BOOLEAN NOT NULL DEFAULT TRUE,
  last_used_at TIMESTAMP WITH TIME ZONE,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
  expires_at TIMESTAMP WITH TIME ZONE,
  INDEX idx_api_keys_customer_id (customer_id),
  INDEX idx_api_keys_key (key),
  INDEX idx_api_keys_is_active (is_active)
);

-- Integration Configs table
CREATE TABLE integration_configs (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  customer_id UUID NOT NULL REFERENCES customers(id) ON DELETE CASCADE,
  type integration_type NOT NULL,
  name VARCHAR(255) NOT NULL,
  config JSONB NOT NULL,
  is_active BOOLEAN NOT NULL DEFAULT TRUE,
  last_sync_at TIMESTAMP WITH TIME ZONE,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
  updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
  INDEX idx_integration_configs_customer_id (customer_id),
  INDEX idx_integration_configs_type (type),
  INDEX idx_integration_configs_is_active (is_active)
);

-- Measurement Accuracy Feedback table
CREATE TABLE measurement_accuracy_feedback (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  customer_id UUID NOT NULL REFERENCES customers(id) ON DELETE CASCADE,
  value_calculation_id UUID NOT NULL REFERENCES value_calculations(id) ON DELETE CASCADE,
  accuracy DECIMAL(5, 2) NOT NULL CHECK (accuracy >= 0 AND accuracy <= 100),
  feedback TEXT NOT NULL,
  suggested_adjustment JSONB,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
  INDEX idx_feedback_customer_id (customer_id),
  INDEX idx_feedback_value_calc_id (value_calculation_id)
);

-- Go-Live Switch Requests table
CREATE TABLE go_live_switch_requests (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  customer_id UUID NOT NULL REFERENCES customers(id) ON DELETE CASCADE,
  requested_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
  requested_by VARCHAR(255) NOT NULL,
  safety_checks JSONB NOT NULL,
  readiness_score DECIMAL(5, 2) NOT NULL CHECK (readiness_score >= 0 AND readiness_score <= 100),
  estimated_live_date TIMESTAMP WITH TIME ZONE NOT NULL,
  status VARCHAR(50) NOT NULL DEFAULT 'PENDING',
  approved_at TIMESTAMP WITH TIME ZONE,
  approved_by VARCHAR(255),
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
  INDEX idx_go_live_customer_id (customer_id),
  INDEX idx_go_live_status (status)
);

-- Audit Log table
CREATE TABLE audit_log (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  customer_id UUID NOT NULL REFERENCES customers(id) ON DELETE CASCADE,
  user_id UUID,
  action VARCHAR(255) NOT NULL,
  resource_type VARCHAR(255) NOT NULL,
  resource_id VARCHAR(255),
  changes JSONB,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT now(),
  INDEX idx_audit_log_customer_id (customer_id),
  INDEX idx_audit_log_user_id (user_id),
  INDEX idx_audit_log_created_at (created_at)
);

-- Create indexes for common queries
CREATE INDEX idx_customers_updated_at ON customers(updated_at DESC);
CREATE INDEX idx_value_calculations_value_def_id ON value_calculations(value_definition_id);
CREATE INDEX idx_approvals_updated_at ON approvals(updated_at DESC);
CREATE INDEX idx_alerts_created_at ON alerts(created_at DESC);
CREATE INDEX idx_tickets_updated_at ON support_tickets(updated_at DESC);

-- Create materialized views for dashboards
CREATE MATERIALIZED VIEW customer_onboarding_summary AS
SELECT
  c.id,
  c.name,
  c.email,
  c.status,
  COUNT(CASE WHEN ss.status = 'COMPLETED' THEN 1 END)::FLOAT / 5 * 100 as setup_progress,
  COUNT(DISTINCT a.id) as approval_count,
  COUNT(DISTINCT al.id) FILTER (WHERE NOT al.is_resolved) as open_alert_count,
  COUNT(DISTINCT st.id) FILTER (WHERE st.status != 'CLOSED') as open_ticket_count,
  MAX(vc.timestamp) as last_value_calc,
  MAX(vc.calculated_value) FILTER (ORDER BY vc.timestamp DESC LIMIT 1) as current_value,
  c.created_at,
  c.updated_at
FROM customers c
LEFT JOIN setup_steps ss ON c.id = ss.customer_id
LEFT JOIN approvals a ON c.id = a.customer_id
LEFT JOIN alerts al ON c.id = al.customer_id
LEFT JOIN support_tickets st ON c.id = st.customer_id
LEFT JOIN value_calculations vc ON c.id = vc.customer_id
GROUP BY c.id;

CREATE INDEX idx_customer_summary_status ON customer_onboarding_summary(status);

-- Triggers for automatic timestamp updates
CREATE OR REPLACE FUNCTION update_updated_at_column()
RETURNS TRIGGER AS $$
BEGIN
  NEW.updated_at = now();
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER update_customers_updated_at
BEFORE UPDATE ON customers
FOR EACH ROW
EXECUTE FUNCTION update_updated_at_column();

CREATE TRIGGER update_setup_steps_updated_at
BEFORE UPDATE ON setup_steps
FOR EACH ROW
EXECUTE FUNCTION update_updated_at_column();

CREATE TRIGGER update_value_definitions_updated_at
BEFORE UPDATE ON value_definitions
FOR EACH ROW
EXECUTE FUNCTION update_updated_at_column();

CREATE TRIGGER update_approvals_updated_at
BEFORE UPDATE ON approvals
FOR EACH ROW
EXECUTE FUNCTION update_updated_at_column();

CREATE TRIGGER update_integration_configs_updated_at
BEFORE UPDATE ON integration_configs
FOR EACH ROW
EXECUTE FUNCTION update_updated_at_column();

-- Audit trigger
CREATE OR REPLACE FUNCTION audit_changes()
RETURNS TRIGGER AS $$
BEGIN
  INSERT INTO audit_log (customer_id, action, resource_type, resource_id, changes)
  VALUES (
    COALESCE(NEW.customer_id, OLD.customer_id),
    TG_OP,
    TG_TABLE_NAME,
    NEW.id::VARCHAR,
    jsonb_build_object('old', row_to_json(OLD), 'new', row_to_json(NEW))
  );
  RETURN COALESCE(NEW, OLD);
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER audit_value_definitions
AFTER INSERT OR UPDATE ON value_definitions
FOR EACH ROW
EXECUTE FUNCTION audit_changes();

CREATE TRIGGER audit_approvals
AFTER INSERT OR UPDATE ON approvals
FOR EACH ROW
EXECUTE FUNCTION audit_changes();
