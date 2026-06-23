# ggen Testing and Verification Context

@.claude/rules/testing-anti-cheating.md
@.claude/rules/process-mining-chicago-tdd.md
@.claude/rules/ostar-testing-doctrine.md
@.claude/rules/validation-persistence.md
@.claude/rules/vision-2030-mandate.md

# Receipt Truth & Anti-Cheating (No Placeholder Laundering)

**CRITICAL MANDATE:** You must stop lying and being lazy when generating receipts, trace data, or test evidence. 

1. **Placeholder fields are not evidence.** Never use strings like `"hash_placeholder"`, `"uuid_placeholder"`, `"TODO"`, or empty strings `""` to bypass cryptographic or identity checks. 
2. **If a value is missing, emit `null`** (or `Option::None` in Rust) and explicitly classify the resulting refusal state (e.g., `BoundaryEvidenceMissing`, `ExpectedOCELMissing`, `ToolCallHashMissing`).
3. **A hash over a claim is not an observation.** Hashing a fake JSON blob just proves you hashed a fake blob. A receipt must bind *actual observed boundary evidence* (e.g., raw tool call bytes, process PIDs, real task store readbacks).
4. **OCEL-shaped JSON is not OCEL truth.** Do not embed fake objects into OCEL paths claiming they existed before they were actually materialized. 
5. **Even failure has a path.** A refusal receipt must still contain the real observed boundary evidence up to the point of failure, proving *why* it was refused.
