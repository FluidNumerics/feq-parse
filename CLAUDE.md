# AGENTS.md
AI Agent Operating Rules for This Repository

This file defines **hard constraints** and **preferred behaviors** for AI coding
agents (e.g., Claude, Codex) working in this repository. These rules exist to protect
numerical correctness, performance, and long-term maintainability of a
a dynamic equation parser Fortran library codebase.

Agents MUST follow all rules below unless explicitly instructed otherwise by a
human maintainer.

---

## 1. Project Purpose

This project implements and infix tokenizer, infix to postfix conversion utilities, and postfix calculator 
that works with scalars and higher rank arrays in Fortran for dynamic equation parsing and evaluation in Fortran.

Primary goals:
- Users should be able to provide valid infix equations as strings and evaluate them reliably without error
- Numerical correctness
- High performance on CPUs

Changes must NOT degrade:
- Correctness

---

## 2. Fortran Standards & Toolchain

### Language Standard
- Target: **Fortran 2008**
- Code must remain compatible with:
  - gfortran ≥ 11
  - ifx
  - nvfortran
  - amdflang

### Prohibited Language Features
Do NOT introduce:
- Coarrays
- Fortran 2018+ features
- Compiler-specific extensions
- Automatic polymorphism in performance-critical paths

### Formatting & Conventions
- Free-form source
- `implicit none` required in all program units
- Explicit `intent(in|out|inout)` on all dummy arguments
- Lowercase keywords preferred
- Line length ≤ 100 characters

---

## 3. Numerical & Algorithmic Constraints

### Hard Rules
- Do NOT change the mathematical formulation without approval
- Do NOT change discretization order
- Do NOT change basis, quadrature, or nodal ordering
- Do NOT reorder floating-point reductions
- Do NOT alter time integration schemes

### Floating-Point Behavior
- Bitwise reproducibility may be required
- Preserve operation ordering in loops
- Avoid algebraic “simplifications” unless mathematically justified

### Array Semantics
- Do NOT replace explicit loops with array syntax unless equivalence is proven
- Avoid implicit temporaries

---

## 4. Performance Rules (Critical)

This is an HPC codebase. Performance regressions are unacceptable.

### Memory
- Avoid temporary allocations in hot paths
- No automatic arrays in tight loops
- No hidden allocations via array slicing

### Loops
- Preserve loop ordering for cache locality
- Do NOT replace DO loops with WHERE / FORALL
- Vectorization-friendly structure must be preserved

### Abstraction
- Do NOT introduce runtime polymorphism in kernels
- Avoid excessive modularization inside hot loops

---

## 5. Code Organization & APIs

### File & Module Structure
- Do NOT rename modules
- Do NOT move files between directories
- Do NOT change public interfaces without approval
- Preserve module dependency order

### Public APIs
- Public procedures are considered **stable**
- Backward compatibility is required unless stated otherwise

---

## 6. Testing & Validation

### Required
- All existing regression tests must pass
- Do NOT modify reference output files
- Numerical differences must be justified

### New Code
- New features require:
  - A test case
  - Clear validation criteria

---

## 7. Documentation & Comments

### Preserve Scientific Meaning
- Do NOT remove comments describing:
  - Equations
  - Algorithms
  - Numerical assumptions

### New Routines
Must include:
- Mathematical description
- Variable meaning and units
- Expected input ranges

---

## 8. Prohibited Actions (Explicit)

Agents MUST NOT:
- Rewrite code in another language
- Convert procedural code to OO Fortran
- Replace MPI with coarrays
- Introduce external dependencies
- “Modernize” syntax without benefit
- Delete legacy code without explanation

---

## 10. Preferred Agent Behavior

Agents SHOULD:
- Ask before changing algorithms
- Explain numerical and performance implications
- Provide minimal diffs
- Reference existing patterns in the codebase
- Flag any uncertainty explicitly

Agents SHOULD NOT:
- Make large refactors unless requested
- Assume intent beyond the explicit request

---

## 11. When in Doubt

If a change could affect:
- Numerical accuracy
- Stability
- Performance
- Parallel behavior

STOP and ask for clarification.

---
## 12. Local Dev Commands (Required)

Agents MUST use the commands below for formatting and tests.
If these commands fail due to missing Docker, agents should not invent alternatives.

### Lint / Format (Fortran)
Run fprettify on the repository:

```bash
./scripts/lint.sh
```
To apply formatting changes:
```
./scripts/format.sh
```

### Unit/Regression tests

```
./scripts/test.sh
```
---

End of AGENTS.md

