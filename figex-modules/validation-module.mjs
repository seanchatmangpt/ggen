import { z } from 'zod';

export function validateRequest(schema, data) {
  const result = schema.safeParse(data);
  return {
    valid: result.success,
    errors: result.error ? result.error.issues.map(issue => issue.message) : []
  };
}

export function validateEmail(email) {
  const emailSchema = z.string().email();
  const result = emailSchema.safeParse(email);
  return {
    valid: result.success,
    errors: result.error ? result.error.issues.map(issue => issue.message) : []
  };
}

export function validateURL(url) {
  const urlSchema = z.string().url();
  const result = urlSchema.safeParse(url);
  return {
    valid: result.success,
    errors: result.error ? result.error.issues.map(issue => issue.message) : []
  };
}