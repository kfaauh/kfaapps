# Technical status — 2026-10-30

## Delivered

- Additive `monitorering/` infrastructure and an independent GitHub Actions workflow; no existing application or scraper file changed.
- DuckDB initialization, typed immutable consumption snapshots, atomic idempotent CSV/manifest import, source-time revisions, overlap resolution, deletion/tombstone semantics, coverage reporting, externally evidenced attestations, hierarchical ATC summaries and Shiny-readable SQL views.
- Generic compact browser exporter using the supplied validated query, descriptor-based DSR decoding, transient token capture/manual fallback, monthly plans for ATC1–5, sequential requests, retries, stop/checkpoint/resume, SHA-256 manifests and safe re-partitioning of truncated scopes.
- Tariff import from existing Excel/RDS or normalized CSV, exact raw archives, nominal/effective/observation/import times, typed prices with DDD provenance, duplicate validation, full property history and added/removed/changed comparisons.
- Opt-in wrapper reusing the existing fetch once, with explicit new environment credentials and source-period/group checks.
- Daily historical price joins, full-month completeness checks, ATC mismatch detection, actual subsidy kept separate from modelled price-basis scenarios, plus latest observed price scenario view.
- CLI, inbox import, logging, process writer lock, checkpointed backup including archives, checksum-verified restore, Ubuntu commands and an optional local-import systemd timer.

## Executed local verification

Runtime: Ubuntu 24.04 container, R **4.3.3**, DuckDB R **2.0.0**, DBI 1.2.2, jsonlite 1.8.8, digest 0.6.34, filelock 1.0.3, readxl 1.6.0; Node test runner. R was installed into a disposable local runtime solely for verification. The target server's R 4.6.1 was not available locally; a CI matrix includes it.

- Eight browser test cases passed: genuine CSV numerical totals; synthetic DSR dictionaries/repeat/null masks; malformed/error/duplicate/truncated responses; ATC plans; bounded sequential retries; auth failure; stop; explicit empty capture; RestartToken re-partitioning. Existing `powerbi/tests/export.test.cjs` also passed.
- R integration passed: actual Test 8 import, repeat import, missing one metric retained as NULL, late/overlapping revisions, source-time ordering, broad-scope removal, failed transaction rollback, tariff revisions and removed products, unverified/zero DDD, missing daily coverage, observed-only snapshots excluded from history, backup and relocated restore.
- Test 8 numerical checks: **1,143 long rows**, **29 ATC5 codes**, **576 DDD rows**, **567 subsidy rows**, **25,198,980 DDD**, **80,632,838 DKK subsidy**, methylphenidate January **3,340,703 DDD**. Wide view has **578 product/ATC/month rows**; missing metric rows are not zero-filled.
- A separate actual working DuckDB database was initialized and the unmodified Test 8 files imported and validated locally. Runtime database and raw archives are excluded from git; deployment reconstructs them with the README command.

The automated CLI suite passed, including cross-process writer exclusion, command exit status, inbox import, browser-v2 CSV checksum compatibility, export, backup and restore. A synthetic raw Excel workbook was also parsed and mapped with Danish decimal formatting and an unverified DDD denominator left NULL. CI/server status is separate from these local runs.

## Limits and next steps

1. No production server was touched. Live PowerBI collection and authenticated tariff wrapper require a small acceptance run using the user's authorized session/account. Month labels outside January and the actual querydata model remain live dependencies. No broad national backfill was run.
2. Test 4–7 aggregates are recorded in `tests/fixtures/known_results.json`, with a real-data `scripts/check_reference.R` comparator. Their source observation files and the five-code scope for Test 5–6 were not supplied. Those historical numerical regressions cannot honestly be reproduced from Test 8; no artificial matching fixtures were generated. Original-vs-compact and one-kroner differences remain unexplained source/rounding/revision differences, not accepted as a proven rounding mechanism.
3. RestartToken detection and safe disjoint splitting are tested offline; replaying a token is not implemented. ATC5/month truncation, request-budget exhaustion or unsupported empty subgroups stop with pending work intact. Browser download delivery cannot be verified programmatically; operators must check files before accepting a checkpoint.
4. Tariff tests use explicit synthetic price intervals. No real tariff source files or historical source validity evidence were attached. Existing clean RDS DDD is conservatively unverified. Daily historical valuation remains NULL until genuine daily interval/DDD evidence is imported. The wrapper creates observed-only snapshots; it does not certify those captures as full historical day intervals.
5. ATC matches cannot independently verify DDD-definition compatibility. Current-price scenarios expose capture date and require review for stale/future captures. Actual regional subsidy must never be replaced with DDD times subsidy price.
6. Run the acceptance commands on Ubuntu/R 4.6.1, verify a newly collected small month against the report and verify one authorized tariff capture. Establish supported bulk access/capacity with the publisher before scaling. Then add a read-only publication copy for the later Shiny app, schedule imports/backups, and collect genuine tariff snapshots forward in time. Historical backfill is limited to original archived or otherwise verified source files.
