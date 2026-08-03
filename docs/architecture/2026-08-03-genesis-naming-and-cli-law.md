# Genesis: Naming, Identity, and the CLI Surface

## Status

This document defines the naming law for the clean-room successor to the current `ggen` system.

It does not assign the final product name.

It defines how names are discovered, compressed, admitted, and exposed through the command-line interface.

The current `ggen` name remains a historical reference and a bootstrap coordinate.

---

# 1. The naming problem

The system is being restarted from first principles.

The restart must not begin with branding.

It must begin with distinction.

Genesis presents naming after separation and observation. Light is distinguished from darkness before the two are called Day and Night. The human observes the animals and gives them names after they are brought before him.

The architectural sequence is therefore:

```text
observe
→ distinguish
→ bound
→ identify invariants
→ determine authority
→ assign purpose
→ name
→ record
```

Naming before distinction creates attachment to an implementation.

Naming after distinction creates a symbol for an invariant.

---

# 2. A name is an identity checksum

Let `U` be the undifferentiated universe of possible systems.

Let `P` be the predicate that identifies the properties that must remain true across implementations.

Then the identity class is:

\[
C = \{x \in U \mid P(x)\}
\]

A valid name is a map:

\[
N : C \rightarrow \Sigma
\]

where `Σ` is the namespace of admissible names.

The name is valid only if it is invariant under implementation substitution:

\[
x \sim y \Longrightarrow N(x)=N(y)
\]

where `x ~ y` means that `x` and `y` preserve the same constitutional invariants.

The inverse expectation must also hold strongly enough to prevent ambiguity:

\[
N(x)=N(y) \Longrightarrow P(x)\approx P(y)
\]

A name is therefore not decoration.

It is a compressed witness of identity.

---

# 3. Chesterton's fence for `ggen`

The current name was not arbitrary.

It follows a compact two-word naming rule:

```text
graph generation
↓
g + gen
↓
ggen
```

The first letter of the first concept is joined to the first three letters of the second concept.

This document names that rule:

```text
1+3 compression
```

Formally, for a two-concept identity phrase `(a,b)`:

\[
C_{1+3}(a,b)
=
\operatorname{head}_1(a)
\cdot
\operatorname{head}_3(b)
\]

For example:

| Identity phrase | Compression | Token |
|---|---:|---|
| graph generation | `g` + `gen` | `ggen` |
| capability manufacture | `c` + `man` | `cman` |
| evidence replay | `e` + `rep` | `erep` |
| policy planning | `p` + `pla` | `ppla` |
| standing verification | `s` + `ver` | `sver` |

The old name must not be discarded without preserving the function of this rule.

Its value is not only brevity.

It creates a deterministic bridge from a two-concept invariant to a four-character command token.

---

# 4. Why four characters matter

The four-character token is the minimum practical CLI namespace for this system because it provides:

- one-hand typability;
- rapid recognition;
- low visual noise;
- deterministic derivation;
- enough entropy to distinguish core operations;
- resistance to prose-shaped hallucination;
- compatibility with multilingual state tokens;
- suitability for high-tempo operator use.

The token is not an acronym in the conventional sense.

It is a compressed semantic address.

The CLI surface should therefore be generated from an admitted concept pair rather than manually invented.

```text
concept pair
→ invariant definition
→ 1+3 compression
→ collision check
→ authority assignment
→ CLI token
```

---

# 5. The Genesis Naming Protocol

Every canonical name must pass the following sequence.

## 5.1 Observe

Record what the subject actually does.

Do not begin with desired marketing language.

Required observations include:

- inputs;
- outputs;
- authority;
- irreversible consequences;
- customer;
- evidence;
- replay behavior;
- failure and refusal boundaries.

## 5.2 Distinguish

Separate the subject from adjacent systems.

Answer:

- What can this system do that Hygen cannot?
- What can Hygen do that this system deliberately excludes?
- What belongs to planning rather than manufacture?
- What belongs to manufacture rather than execution?
- What belongs to execution rather than verification?

## 5.3 Bound

Define the admitted identity boundary.

A name cannot identify an unbounded aspiration.

It must identify a bounded object with explicit exclusions.

## 5.4 Identify invariants

Remove implementation details.

Do not use names based on:

- Rust;
- RDF;
- Tera;
- Claude;
- PPDDL;
- GitHub;
- a current repository layout;
- a temporary transport;
- a specific vendor.

Retain only the properties that survive implementation substitution.

## 5.5 Determine authority

Every named object must carry an authority ceiling:

- `SELECT`;
- `CONSTRUCT`;
- `AUTHORIZE`;
- `DO` through BRCE;
- `VERIFY`;
- `REFUSE`.

Two commands that differ only by authority must not share a name.

## 5.6 Assign purpose

State the customer-facing consequence.

The purpose must be observable.

Avoid names whose meaning depends on intention alone.

## 5.7 Compose the identity phrase

Choose exactly two canonical concepts.

The first concept identifies the subject or domain.

The second concept identifies the transformation or role.

Pattern:

```text
<subject> <operation>
```

Examples:

```text
graph generation
capability manufacture
policy planning
receipt replay
standing verification
```

## 5.8 Compress

Apply:

\[
C_{1+3}(a,b)
=
\operatorname{head}_1(a)
\cdot
\operatorname{head}_3(b)
\]

Lowercase ASCII is the default CLI representation.

The full multilingual protocol token may remain available as metadata or an alias.

## 5.9 Check collisions

A candidate is refused if it collides with:

- an existing canonical token;
- a shell command likely to create ambiguity;
- a standing token;
- a protocol namespace;
- an unsafe or misleading industry term.

Collision resolution must not be arbitrary.

It must return to the concept pair and refine the distinction.

## 5.10 Record

Every admitted name must have a machine-readable registry entry containing:

```yaml
token: ggen
subject_concept: graph
operation_concept: generation
identity_phrase: graph generation
compression_rule: 1+3
authority: CONSTRUCT
purpose: project local graph driven artifact generation
status: historical
aliases: []
replaced_by: null
```

---

# 6. CLI grammar

The clean CLI should use a small grammatical surface.

```text
<system> <command> [object] [options]
```

The system token identifies the factory.

The command token is generated through the 1+3 law.

The object identifies the admitted subject.

Options refine a bounded operation but may not silently alter authority.

Example shape:

```text
<root> ppla refactor --problem bootstrap
<root> cman capability --id receipt-edge
<root> erep receipt.json
<root> sver capability-id
```

The exact root token remains unnamed until the system invariant is admitted.

---

# 7. Candidate command families

The following are provisional examples, not final commands.

| Identity phrase | Token | Layer | Authority |
|---|---|---|---|
| policy planning | `ppla` | 策 | SELECT |
| domain modeling | `dmod` | 策 | SELECT |
| goal admission | `gadm` | 策 / 標準作業 | AUTHORIZE |
| capability manufacture | `cman` | 標準作業 | CONSTRUCT |
| artifact projection | `apro` | 標準作業 | CONSTRUCT |
| write planning | `wpla` | 標準作業 | CONSTRUCT |
| broker execution | `bexe` | 실행 | DO through BRCE |
| consequence observation | `cobs` | 실행 / evidence | OBSERVE |
| receipt creation | `rcre` | evidence | CONSTRUCT |
| receipt replay | `rrep` | evidence | VERIFY |
| standing verification | `sver` | inspector | VERIFY |
| line stopping | `lsto` | andon | REFUSE / STOP |
| defect repair | `drep` | kaizen | CONSTRUCT |
| self hosting | `shos` | bootstrap | VERIFY |

These examples demonstrate the grammar.

They do not establish the final ontology.

---

# 8. CLI names are typed protocol tokens

A command token must resolve to a schema.

For example:

```yaml
token: ppla
identity_phrase: policy planning
protocol: 策
input_schema: ppddl-problem.schema.json
output_schema: policy.schema.json
authority: SELECT_ONLY
forbidden_tools:
  - Write
  - Edit
  - Bash
terminal_states:
  - PARTIAL_ALIVE
  - BLOCKED
  - UNSUPPORTED
  - REFUSED_*
```

And:

```yaml
token: bexe
identity_phrase: broker execution
protocol: 실행
input_schema: brce-grant.schema.json
output_schema: consequence-receipt.schema.json
authority: BRCE_ONLY
requires:
  - exact_subject
  - active_wip_slot
  - expected_consequence
  - stop_condition
  - receipt_destination
```

The token cannot be satisfied by prose.

It is valid only when its schema, authority, and transition law close.

---

# 9. Multilingual namespace relationship

The 1+3 ASCII CLI surface does not replace the multilingual protocol.

The two systems serve different purposes.

| Surface | Purpose |
|---|---|
| Chinese high-level token | compressed planning type and possibility-space semantics |
| Japanese production term | standardized work and automation law |
| Korean execution term | bounded operator action and cadence |
| 1+3 ASCII CLI token | deterministic shell address |

Example mapping:

```text
策 / policy planning / ppla
標準作業 / capability manufacture / cman
실행 / broker execution / bexe
証 / standing verification / sver
止 / line stopping / lsto
```

The multilingual symbol carries semantic type.

The ASCII token carries operational address.

The full English phrase remains explanatory metadata rather than canonical state.

---

# 10. Working Backwards naming

A new CLI command must begin with a future customer consequence.

The Working Backwards sequence is:

```text
future consequence
→ customer FAQ
→ capability invariant
→ authority
→ identity phrase
→ 1+3 token
→ skill
→ agent
→ permissions
→ hooks
→ PPDDL action
→ verifier
```

A command is not admitted because it sounds useful.

It is admitted because the future consequence requires a distinct production operation.

Example:

```text
Future consequence:
A capability can be independently replayed from its receipt.

Capability invariant:
Receipt-bound deterministic replay.

Identity phrase:
receipt replay

CLI token:
rrep
```

---

# 11. PPDDL integration

CLI tokens should map directly to PPDDL actions where appropriate.

```lisp
(:action rrep
  :parameters (?r - receipt ?a - artifact)
  :precondition
    (and
      (receipt-valid ?r)
      (receipt-binds ?r ?a)
      (replay-authorized ?r))
  :effect
    (probabilistic
      0.99 (replay-matched ?r)
      0.01 (replay-mismatch ?r)))
```

The planning layer reasons over canonical action identities.

The CLI invokes the same identity.

The receipt records the same identity.

This preserves:

```text
PPDDL action
↔ CLI command
↔ Claude skill
↔ authority profile
↔ receipt edge
↔ replay event
```

No layer invents an independent verb.

---

# 12. Combinatorial Minimalism for the CLI

The CLI surface must be minimal.

Let `K` be the set of candidate commands.

Choose:

\[
K^*
=
\operatorname*{arg\,min}_{K'\subseteq K}|K'|
\]

subject to:

\[
\operatorname{CapabilityClosure}(K')=\text{true}
\]

\[
\operatorname{AuthoritySeparation}(K')=\text{true}
\]

\[
\operatorname{PlanningClosure}(K')=\text{true}
\]

\[
\operatorname{ReceiptReplayClosure}(K')=\text{true}
\]

A command is unnecessary if it can be expressed as:

- an option;
- an object type;
- a PPDDL policy choice;
- a generated skill;
- a composition of existing lawful commands.

Do not add a command merely to make a feature visible.

---

# 13. Naming refusals

The naming protocol must produce typed refusals.

```text
REFUSED_NAME_BEFORE_IDENTITY
REFUSED_IMPLEMENTATION_BOUND_NAME
REFUSED_AUTHORITY_AMBIGUITY
REFUSED_1PLUS3_COLLISION
REFUSED_MARKETING_ONLY_NAME
REFUSED_UNBOUNDED_PURPOSE
REFUSED_DUPLICATE_OPERATION
REFUSED_EUPHEMISTIC_STATE
REFUSED_UNRECORDED_ALIAS
REFUSED_MULTILINGUAL_TYPE_DRIFT
```

A refused name returns to distinction.

It does not proceed through creative brainstorming.

---

# 14. The clean-room successor

The new system should be named only after the following are closed:

1. One canonical customer consequence.
2. One minimal capability ontology.
3. One PPDDL domain.
4. One lawful production sequence.
5. One BRCE execution boundary.
6. One receipt and replay path.
7. One independently verified reference product.
8. One Claude operator that reconstructs the system.
9. One minimal CLI grammar.
10. One admitted invariant phrase.

At that point the root name is manufactured using the same law as its commands.

For a candidate identity phrase `(a,b)`:

\[
\operatorname{Name}=C_{1+3}(a,b)
\]

The final name must be discovered from the invariant pair.

---

# 15. Genesis law

The constitutional naming law is:

> Do not name the implementation before the identity is distinguished. Observe the subject, separate it from adjacent systems, identify the invariant pair, determine its authority, and derive its four-character CLI token through 1+3 compression.

In compact form:

```text
分
→ 名
→ 命
```

Distinguish.

Name.

Command.

And in system notation:

\[
O
\rightarrow
O^*
\rightarrow
I
\rightarrow
N
\rightarrow
C_{1+3}
\rightarrow
CLI
\]

where:

- `O` is partial observation;
- `O*` is admitted observation;
- `I` is invariant identity;
- `N` is the full identity phrase;
- `C₁₊₃` is deterministic compression;
- `CLI` is the operational address.

The command surface is therefore not designed after the architecture.

It is the compressed visible projection of the architecture's admitted identities.
