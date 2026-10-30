-- A complete scope snapshot replaces that scope, including observations now absent.
-- Resolve overlap BEFORE joining observations, by source time (never import order).
CREATE OR REPLACE VIEW v_consumption_scope AS
WITH universe AS (SELECT DISTINCT atc5,month FROM consumption_observation)
SELECT u.atc5,u.month,e.extract_id,e.source,e.extracted_at,e.imported_at,e.import_seq,
       e.atc_prefix, a.extract_id IS NOT NULL AS population_verified
FROM universe u JOIN consumption_extract e
 ON u.month=e.month AND starts_with(u.atc5,e.atc_prefix)
LEFT JOIN coverage_attestation a ON a.extract_id=e.extract_id
QUALIFY row_number() OVER (PARTITION BY u.atc5,u.month ORDER BY e.extracted_at DESC,e.import_seq DESC)=1;
CREATE OR REPLACE VIEW v_consumption_long AS
SELECT o.*,s.source,s.extracted_at,s.imported_at,s.import_seq,s.population_verified
FROM consumption_observation o JOIN v_consumption_scope s
 ON o.extract_id=s.extract_id AND o.atc5=s.atc5 AND o.month=s.month;
CREATE OR REPLACE VIEW v_consumption AS
SELECT atc5,varenummer,month,extract_id,source,extracted_at,imported_at,
 max(value) FILTER(WHERE metric='ddd') AS ddd,
 max(value) FILTER(WHERE metric='regionalt_tilskud_kr') AS regionalt_tilskud_kr,
 count(*) FILTER(WHERE metric='ddd')=1 AS ddd_observed,
 count(*) FILTER(WHERE metric='regionalt_tilskud_kr')=1 AS subsidy_observed
FROM v_consumption_long GROUP BY ALL;
CREATE OR REPLACE VIEW v_consumption_atc AS
SELECT level, left(atc5,code_length::INTEGER) AS atc,month,metric,
 sum(value) AS observed_sum,count(*) AS observed_rows,
 count(DISTINCT extract_id) AS extract_count
FROM v_consumption_long CROSS JOIN (VALUES (1,1),(2,3),(3,4),(4,5),(5,7)) l(level,code_length)
GROUP BY ALL;
-- Last full snapshot at a given source timestamp, including removed products.
CREATE OR REPLACE VIEW v_tariff_latest AS
SELECT p.*,s.observed_at,s.nominal_from,s.nominal_to,s.validity_kind,s.effective_from,s.effective_to
FROM tariff_product p JOIN tariff_snapshot s USING(snapshot_id)
WHERE s.complete AND s.snapshot_id=(SELECT snapshot_id FROM tariff_snapshot WHERE complete
 ORDER BY observed_at DESC,import_seq DESC LIMIT 1);
-- Daily interval resolution. Observed-only captures are deliberately excluded.
CREATE OR REPLACE VIEW v_tariff_daily AS
SELECT day::DATE AS price_date,p.*,s.observed_at,s.nominal_from,s.nominal_to
FROM tariff_snapshot s JOIN tariff_product p USING(snapshot_id),
 LATERAL generate_series(s.effective_from,s.effective_to,INTERVAL 1 DAY) g(day)
WHERE s.complete AND s.validity_kind='source_interval'
 AND s.snapshot_id=(SELECT z.snapshot_id FROM tariff_snapshot z
 WHERE z.complete AND z.validity_kind='source_interval'
 AND day::DATE BETWEEN z.effective_from AND z.effective_to
 ORDER BY z.observed_at DESC,z.import_seq DESC LIMIT 1);
CREATE OR REPLACE VIEW v_tariff_changes AS
WITH pairs AS (
 SELECT snapshot_id,lag(snapshot_id) OVER(ORDER BY observed_at,import_seq) AS previous_id
 FROM tariff_snapshot WHERE complete
), keys AS (
 SELECT x.*,p.varenummer FROM pairs x JOIN tariff_product p ON p.snapshot_id=x.snapshot_id
 UNION SELECT x.*,p.varenummer FROM pairs x JOIN tariff_product p ON p.snapshot_id=x.previous_id
)
SELECT k.*,a.esp AS esp,a.tsp AS tsp,b.esp AS previous_esp,b.tsp AS previous_tsp,
 CASE WHEN b.varenummer IS NULL THEN 'added' WHEN a.varenummer IS NULL THEN 'removed'
 WHEN a.row_hash<>b.row_hash THEN 'changed' ELSE 'unchanged' END AS change
FROM keys k LEFT JOIN tariff_product a ON a.snapshot_id=k.snapshot_id AND a.varenummer=k.varenummer
LEFT JOIN tariff_product b ON b.snapshot_id=k.previous_id AND b.varenummer=k.varenummer;
-- Equal daily consumption is an assumption, not an observed dispensing pattern.
CREATE OR REPLACE VIEW v_consumption_price_month AS
WITH linked AS (
 SELECT c.*,day::DATE AS price_date,t.snapshot_id,t.atc AS tariff_atc,
 t.tsp_per_ddd,t.esp_per_ddd,t.ddd_provenance,
 date_diff('day',c.month,c.month+INTERVAL 1 MONTH)::INTEGER AS month_days
 FROM v_consumption c,
 LATERAL generate_series(c.month,c.month+INTERVAL 1 MONTH-INTERVAL 1 DAY,INTERVAL 1 DAY) g(day)
 LEFT JOIN v_tariff_daily t ON t.varenummer=c.varenummer AND t.price_date=day::DATE
)
SELECT atc5,varenummer,month,extract_id,max(ddd) AS ddd,
 max(regionalt_tilskud_kr) AS actual_regional_subsidy_kr,max(month_days) AS month_days,
 count(snapshot_id) AS price_days,
 count(tsp_per_ddd) FILTER(WHERE tariff_atc=atc5) AS compatible_tsp_days,
 count(*) FILTER(WHERE tariff_atc IS NOT NULL AND tariff_atc<>atc5) AS atc_mismatch_days,
 CASE WHEN count(tsp_per_ddd) FILTER(WHERE tariff_atc=atc5)=max(month_days)
 THEN sum(tsp_per_ddd)/max(month_days) END AS calendar_weighted_tsp_per_ddd,
 CASE WHEN count(esp_per_ddd) FILTER(WHERE tariff_atc=atc5)=max(month_days)
 THEN sum(esp_per_ddd)/max(month_days) END AS calendar_weighted_esp_per_ddd,
 max(ddd)*calendar_weighted_tsp_per_ddd AS modelled_subsidy_basis_kr,
 'uniform_daily_volume, DDD_definition_not_independently_verified' AS assumptions
FROM linked GROUP BY atc5,varenummer,month,extract_id;
CREATE OR REPLACE VIEW v_consumption_current_price AS
SELECT c.*,t.snapshot_id AS price_snapshot_id,t.observed_at AS price_observed_at,
 t.validity_kind,t.nominal_from,t.nominal_to,t.atc AS tariff_atc,
 t.tsp_per_ddd,t.ddd_provenance,
 CASE WHEN c.atc5=t.atc THEN c.ddd*t.tsp_per_ddd END AS current_price_subsidy_basis_kr,
 CASE WHEN t.snapshot_id IS NULL THEN 'missing_product' WHEN t.atc IS DISTINCT FROM c.atc5 THEN 'atc_mismatch'
 WHEN t.tsp_per_ddd IS NULL THEN 'no_valid_ddd_price' ELSE 'review_ddd_definition_and_price_date' END AS linkage_status
FROM v_consumption c LEFT JOIN v_tariff_latest t USING(varenummer);
