# lean_c Governance

## Purpose
Define how decisions are made, how roles are assigned, and how the project remains transparent, inclusive, and accountable to its community and sponsors.

## Values
- Open collaboration and reproducible research
- Safety, respect, and inclusion (see CODE_OF_CONDUCT.md)
- Public benefit through open-source licensing (Apache-2.0)

## Roles
- Maintainer: responsible for technical direction, releases, security coordination, and final decisions when consensus is unclear. Initial maintainer: Mikhail Katkov.
- Core Contributors: contributors with sustained, high-impact contributions who help triage, review, and plan roadmap items. Nominated by the Maintainer via PR and confirmed by lazy consensus of Core Contributors.
- Contributors: anyone submitting issues, discussions, docs, or code via PRs.
- Advisors (optional): subject-matter or community advisors who provide non-binding input on strategy, governance, or funding. Advisors may be suggested by the Maintainer and confirmed by lazy consensus.

## Decision Making
- Lazy consensus on issues and pull requests after a reasonable review window (generally 5 business days for non-urgent changes).
- For changes with broad impact (roadmap, interfaces, licensing, governance), open a request-for-comment (RFC) issue and allow extended discussion (generally 10 business days).
- If consensus is not reached, the Maintainer decides, documenting rationale in the issue/PR.
- Conflicts of Interest are disclosed and managed per doc/coi.md.

## Roadmap and Releases
- The roadmap is tracked in GitHub issues and docs/roadmap.md. Quarterly planning issues summarize priorities and milestones.
- Releases are tagged with semantic versioning once the project reaches an API surface where semver is meaningful; prior to that, milestone tags are used.
- Each release includes summarized changes, any migration notes, and links to verification artifacts or tests.

## Security and Responsible Disclosure
- Potential security issues or proof counterexamples affecting safety guarantees should be reported privately to the Maintainer (see SECURITY section in CONTRIBUTING or email in CODE_OF_CONDUCT.md). A coordinated disclosure timeline will be agreed upon with reporters.

## Community Operations
- Meetings are asynchronous by default (issues/PRs). Ad-hoc community calls may be scheduled and summarized in the repository.
- All governance, budget summaries (if applicable), and major decisions are documented in the repo.

## Changes to this Governance
- Proposed via pull request. Accepted by lazy consensus of Core Contributors with Maintainer approval. In the absence of Core Contributors, Maintainer approval is sufficient during the early project phase.
