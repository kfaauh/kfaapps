# Source interfaces, access and evidence

Reviewed 2026-10-30. No new live querydata extraction or authenticated tariff scrape was made during development.

## PowerBI / Sundhedsdatabanken

- [Official description of Sundhedsdatabanken](https://sundhedsdatabank.dk/om-databanken): PowerBI is an interactive publication format; the page does not document `explore/querydata` as a supported automation API or give a service capacity contract.
- [Official release calendar](https://sundhedsdatabank.dk/udgivelseskalender): check each publication's stated latest data month before collecting. Do not assume the latest calendar month has been published.
- Repository reviewed at baseline commit `8d0db1775190d4d10f46db7381a2ca06a8d06868`: `powerbi/app.R`, `powerbi/export_template.js`, `powerbi/tests/export.test.cjs`, `powerbi/README.md`, `data-scripts/fetch_data2.R`, `lmgrupper2/app.R`, directory tree and ignore files. No AGENTS.md was present.
- Supplied Test 8 script is the source of the compact query expressions (two calculation items, national filters, server-side nonzero condition). Its CSV and original manifest are committed unmodified as fixtures. Existing fetch/XHR token capture is adapted from `powerbi/export_template.js`; descriptor-driven field mapping is retained.

Unattended server authentication and a service-specific bulk-use permission/capacity limit have **not** been established. The reviewed public pages provide no quantitative querydata quota. The implementation therefore uses the user's current authorized browser session, minimum pauses, a request budget, no concurrency, and immediate stop on authorization/schema failures. These engineering limits are not a grant of access or a documented provider quota. Contact the publisher to establish an appropriate supported bulk route before scaling to long national backfills. No token persistence, browser-profile copying, login bypass, automated renewal or use of embedded repository credentials is implemented.

RestartToken wire binding has not been validated against an actual paged response. It is deliberately not guessed: the entire truncated scope is re-requested as exhaustive disjoint child prefixes; no continuation fragment is imported as complete. Raw DSR responses for the historical eight tests were not provided, so DSR regression tests use synthetic encodings of the genuine Test 8 observations.

## Medicine tariffs

- [Official erhverv.medicinpriser.dk guide](https://erhverv.medicinpriser.dk/vejledning.aspx): describes Excel exports, substitution groups, the previous eight price periods, daily subsidy prices (up to two years), and archive material. The history facilities exist, but their authenticated bulk interfaces were not supplied or tested here; they are potential legitimate backfill sources, not implemented assumptions.
- [Official service description](https://erhverv.medicinpriser.dk/default.aspx): ordinary prices update every 14 days; subsidy information updates daily. Nominal period and effective daily prices must be stored separately.
- [DKMA medicine prices for business](https://laegemiddelstyrelsen.dk/da/tilskud/priser/medicinpriser-for-erhverv): subscription and publication context. Use only the user's actual authorized account and access terms.

The legacy fetch already writes the combined raw workbook and clean RDS, but names the workbook with the collection date and can skip failed delivery groups. It computes DDD from ESP / a rounded ESP-per-DDD value. The new wrapper records the source banner, detects skipped groups, uses explicit environment credentials, and archives the already collected workbook. It does not modify or execute the Shiny apps. Daily effective subsidy history requires genuine archived daily files or separately verified historical exports; it is never reconstructed by applying today's prices to an old month.

## DuckDB installation and concurrency

- [Official duckdb-r installation instructions](https://github.com/duckdb/duckdb-r#installation-from-the-posit-public-package-manager): Linux binary installation URL used by `scripts/install.R`.
- [DuckDB concurrency documentation](https://duckdb.org/docs/stable/connect/concurrency): coordinate multiple processes; use a published read-only database copy for a future continuously running Shiny consumer.
