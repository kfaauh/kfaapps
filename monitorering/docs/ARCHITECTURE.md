# Data model and calculation contract

One DuckDB file contains independent consumption and tariff histories. Only `monitorering/` and its own CI workflow are added. Existing apps and the legacy fetch source remain byte-for-byte unchanged.

## Consumption

`consumption_extract` holds one prefix/month source snapshot, its raw-file hashes, source time, import time, query-complete flag and original manifest. `consumption_observation` is long format with primary key `(extract_id, atc5, varenummer, month, metric)`. Values use DECIMAL(28,8); product identifiers are constrained six-character text. Unknown/missing metrics remain absent, then `NULL` in the wide view. Negative nonzero source values are allowed (e.g. corrections), never silently clamped.

Imports validate the complete CSV against its manifest before writes, archive exact bytes by SHA-256, and commit extract plus observations in one transaction. An identical pair is a no-op. A revised source extract receives a distinct immutable ID. The original Test 8 v1 manifest has no source checksum: hashes are computed at receipt, so it has weaker integrity assurance than exporter v2. No import is automatically treated as population-verified.

The current view selects the newest *source-time* snapshot covering each ATC5/month before joining its rows. Thus a later broad snapshot can remove an entire code, a later narrow snapshot can revise part of a broad scope, and missing rows in the winning snapshot cannot resurrect from older extracts. Same-time overlapping captures are rejected as ambiguous. Source dataset revision and DDD-definition version are not exposed by this internal endpoint; manifests/source timestamps record the available evidence without inventing an upstream version.

Coverage reports distinguish missing, partial subscopes, query-complete/population-unverified, and externally attested. Exhaustive subdivision can establish scope response completeness only when every next-level prefix, including empty groups, was imported. Observed ATC codes alone do not establish an exhaustive denominator. Coverage attestations preserve explicit external evidence separately. Historic extracts remain available for audit. A query-complete flag is never proof of full source population coverage.

## Tariff

`tariff_snapshot` stores exact source captures, their nominal price period, observation/import times, completeness evidence and optional *documented* effective interval. `tariff_product` stores all raw properties in JSON plus typed prices, DDD provenance, quality flags and a row hash. Changed/added/removed products compare any two snapshots using `snapshot_id`; `v_tariff_changes` provides consecutive comparisons. Missing/invalid/negative prices become NULL, retaining raw values and flags. Identical duplicate products are collapsed; conflicting duplicates reject the snapshot.

The existing raw `Taksten_*.xlsx` is preferred to `clean_data2.rds` because it retains more properties. For clean RDS, DDD and TSP/DDD were derived from ESP / rounded ESP-per-DDD; these denominators are unverified and not used to fabricate a subsidy-per-DDD. Published source ESP-per-DDD can be retained as such. Normalized tariff CSV may supply positive `ddd_per_pack` and `ddd_provenance=source|verified` together with written `ddd_evidence`; only then are ESP/DDD and TSP/DDD derived. Original published/derived fields remain preserved separately.

A 14-day nominal price period does not prove 14 days of unchanged subsidy prices: they may change daily. `observed_only` is therefore the default for existing fetch captures. Such captures support current-price scenarios and change tracking but are excluded from historical daily valuation. A `source_interval` requires documented effective dates inside the nominal period; daily source files can supply one-day intervals. No future capture is backdated and no last-observation-carried-forward is inferred. Missing daily histories remain explicit gaps.

The opt-in wrapper executes the existing fetch once, substitutes only its parsed credential assignments with explicitly supplied environment variables, and reuses its workbook and already fetched page banner. It adds no scraper requests. Skipped groups force incomplete status. It refuses a missing/different source period and a run crossing Danish midnight. The wrapper does not turn an incompletely observed source into an authoritative history. Subscription-protected raw tariff files must stay outside git.

## Time-dependent linkage and economic meaning

`v_tariff_daily` resolves interval revisions by source observation time at the snapshot level, then product, so removed products do not reappear. `v_consumption_price_month` matches six-digit product number for every calendar day of a consumption month. ATC mismatch is flagged. A calendar-weighted price and modelled subsidy basis are returned only with valid compatible prices on every day. Missing tariff/DDD days yield NULL, never a shortened-denominator average. The daily volume distribution is unknown: uniform daily dispensing is an explicit assumption, not a measured daily expenditure.

`actual_regional_subsidy_kr` is the observed PowerBI outcome. `modelled_subsidy_basis_kr = DDD × calendar-weighted TSP/DDD` is an illustrative reimbursement price basis, not actual regional reimbursement. Patient thresholds, eligibility, dispensing dates and package mix can differ. No clinical/DDD compatibility is proven by matching ATC alone; prices and DDD-definition changes require review. `v_consumption_current_price` uses the latest complete observed capture, exposes its date and validity status, and must be inspected for stale/future prices before forecasting. No advanced forecasts are included.

For later decomposition, compare a fixed compatible product set and fixed DDD definitions: quantity effect `(Q1-Q0)*P0`, price effect `Q0*(P1-P0)`, interaction `(Q1-Q0)*(P1-P0)`. Keep unmatched products and definition changes separate. These are price-basis scenarios, not decomposition of actual subsidy claims.

## Operations

Each writer takes a persistent OS file lock before opening DuckDB. DuckDB additionally enforces its process lock. Interrupted transactions roll back; a crash between raw archiving and commit can leave a harmless unreferenced content-addressed file. Never delete a live lock file to unlock it. There is no automatic archive garbage collection. JSONL logs exclude tokens and raw response bodies. Inbox pairs are processed independently; any rejected pair causes a failing job exit without rolling back already committed pairs.

DuckDB does not support a separate continuously open read-only Shiny process alongside a writer process. A future app must use a published read-only database copy refreshed between connections, or coordinate brief reads with maintenance windows. Avoid sharing the writable file over network filesystems. Backup checkpoints the database, closes it, and copies database plus archives while holding the writer lock. Restore verifies all hashes, requires a new destination, relocates only archive pointers, and revalidates. A failed restore leaves that new destination for inspection; remove it before retrying. Never overwrite an existing production database as part of restore.
