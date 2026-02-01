# lean_c — Fiscal Sponsorship Prospectus

## Summary
lean_c is an open-source research and engineering project to define Lean-level objects and proofs that generate portable, predictable C and enable machine-checked reasoning about code transformations and properties. The project targets small binaries suitable for embedded and constrained environments, with formal guarantees on syntax correctness, type preservation, memory safety, resource bounds, runtime bounds, and absence of undefined behavior.

- License: Apache-2.0
- Repository: https://github.com/mkatkov/lean_c
- Maintainer: Mikhail Katkov (Weizmann Institute of Science)
- Governance: transparent, open-source; decisions via GitHub Issues; Code of Conduct and contributing guidelines published.

## Public Benefit & Alignment
- Advances verified software engineering and reproducibility.
- Produces portable C modules with formal guarantees for critical applications.
- Educational value via examples, docs, and tutorials.
- Aligns with typical 501(c)(3) missions in research, education, and open technology.

## Objectives & Deliverables (12 months)
Aligned to existing roadmap milestones, refined as sponsor-facing deliverables:

1. Lean IR for types and values (Milestone 1)
   - C types, memory block types (Array, Slice), literals (strings, numbers)
   - Syntactic correctness definitions and theorems

2. Expression/statement semantics (Milestone 2)
   - Types suitable for theorem statements
   - Proofs for type preservation, memory safety, runtime bounds

3. Functions and modules (Milestone 3)
   - Theorems for memory/resource safety of functions
   - Generators that emit readable, portable C modules

4. Basic data structures (Milestone 4)
   - Verified containers: list, map, stack, queue, heap
   - Generated C + validation tests
   - Performance/cost model with complexity classes

### Success Metrics
- Proofs: number completed vs planned per milestone
- Generated C modules: count and validation status
- Binary size targets achieved on example programs
- Documentation completeness: tutorials and examples

## Budget (12 months)
Target: $100,000 (finalized; adjustable with sponsor input).

- Personnel (design, implementation, verification): $72,000
- Documentation and tutorials: $6,000
- CI infrastructure, tooling, hosting: $5,000
- Community & governance (triage, reviews, moderation): $4,000
- Fiscal sponsor admin fee (est. 8%): $8,000
- Contingency (5%): $5,000

Total: $100,000

Notes:
- Assumes 0.6–0.8 FTE engineering + verification; scales up/down with funding.
- No proprietary dependencies; CI costs kept minimal.
- Travel/events not required; can be added if sponsor requests outreach.

## Governance & Policies
- Maintainer-led execution with open review via GitHub PRs.
- Code of Conduct and CONTRIBUTING present.
- Conflict of Interest: short-form COI policy published in [doc/coi.md](coi.md).
- Financial controls: budget tracked via sponsor’s accounting; regular reporting.

## IP & Licensing
- Code under Apache-2.0 (permissive, patent grant).
- Documentation under a permissive license (e.g., CC-BY 4.0) if requested.
- Trademarks: none claimed; project name used descriptively.

## Fundraising & Stewardship Plan
- Primary sponsor: fiscal sponsor program (e.g., SFC, SPI, NumFOCUS, Open Collective Foundation).
- Secondary channels: GitHub Sponsors or grants compatible with sponsor policies.
- Funds usage tightly scoped to deliverables and documented in monthly/quarterly reports.

## Risk & Compliance
- Security: focus on absence of undefined behavior and memory safety.
- Data: no user data collection; repository only.
- Insurance: not required for typical software R&D; can follow sponsor guidance.
- Accessibility: docs and site maintained for broad audiences.

## Reporting Cadence
- Monthly status notes: progress vs milestones, risks, next steps.
- Quarterly summaries: metrics, budget status, roadmap adjustments.

## Application Package Checklist
- Project overview and public benefit statement ✓
- Roadmap & deliverables ✓
- Use of funds ✓
- License & IP ✓
- Maintainer and governance ✓
- Prospectus (this document) ✓
- Budget (finalized $100,000) ✓
- COI policy short-form ✓ (see [doc/coi.md](coi.md))

## Contact
- Email: mikhail.katkov@gmail.com
- Issues: https://github.com/mkatkov/lean_c/issues

## Potential Sponsors
Examples: Software Freedom Conservancy (SFC), NumFOCUS, Software in the Public Interest (SPI), Open Collective Foundation.

---

Appendix: Budget Details (to be finalized)

- Personnel breakdown (hours, rates, roles)
- CI/infra: runners, storage, tooling fees
- Docs: time and tooling
- Community operations: moderation time
- Contingency: 10% buffer
