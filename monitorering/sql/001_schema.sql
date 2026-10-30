CREATE TABLE IF NOT EXISTS schema_version (version INTEGER PRIMARY KEY);
INSERT INTO schema_version VALUES (2) ON CONFLICT DO NOTHING;
CREATE SEQUENCE IF NOT EXISTS import_sequence START 1;
CREATE TABLE IF NOT EXISTS consumption_extract (
 extract_id VARCHAR PRIMARY KEY,
 import_seq BIGINT UNIQUE DEFAULT nextval('import_sequence'),
 source VARCHAR NOT NULL,
 extracted_at TIMESTAMPTZ NOT NULL,
 imported_at TIMESTAMPTZ NOT NULL DEFAULT current_timestamp,
 atc_prefix VARCHAR NOT NULL,
 month DATE NOT NULL CHECK(day(month)=1),
 csv_sha256 VARCHAR NOT NULL,
 manifest_sha256 VARCHAR NOT NULL,
 row_count BIGINT NOT NULL CHECK(row_count>=0),
 query_complete BOOLEAN NOT NULL CHECK(query_complete),
 population_coverage VARCHAR NOT NULL CHECK(population_coverage IN ('unverified','verified')),
 manifest_json VARCHAR NOT NULL,
 archive_csv VARCHAR NOT NULL,
 archive_manifest VARCHAR NOT NULL
);
CREATE TABLE IF NOT EXISTS consumption_observation (
 extract_id VARCHAR REFERENCES consumption_extract(extract_id),
 atc5 VARCHAR NOT NULL CHECK(regexp_full_match(atc5,'[A-Z][0-9]{2}[A-Z]{2}[0-9]{2}')),
 varenummer VARCHAR NOT NULL CHECK(regexp_full_match(varenummer,'[0-9]{6}')),
 month DATE NOT NULL CHECK(day(month)=1),
 metric VARCHAR NOT NULL CHECK(metric IN ('ddd','regionalt_tilskud_kr')),
 value DECIMAL(28,8) NOT NULL,
 PRIMARY KEY(extract_id,atc5,varenummer,month,metric)
);
CREATE TABLE IF NOT EXISTS coverage_attestation (
 extract_id VARCHAR PRIMARY KEY REFERENCES consumption_extract(extract_id),
 evidence VARCHAR NOT NULL,
 verified_at TIMESTAMPTZ NOT NULL DEFAULT current_timestamp
);
CREATE TABLE IF NOT EXISTS tariff_snapshot (
 snapshot_id VARCHAR PRIMARY KEY,
 import_seq BIGINT UNIQUE DEFAULT nextval('import_sequence'),
 source VARCHAR NOT NULL,
 observed_at TIMESTAMPTZ NOT NULL,
 imported_at TIMESTAMPTZ NOT NULL DEFAULT current_timestamp,
 nominal_from DATE,
 nominal_to DATE CHECK(nominal_to>=nominal_from),
 CHECK((nominal_from IS NULL AND nominal_to IS NULL) OR (nominal_from IS NOT NULL AND nominal_to IS NOT NULL)),
 effective_from DATE,
 effective_to DATE,
 validity_kind VARCHAR NOT NULL CHECK(validity_kind IN ('source_interval','observed_only')),
 complete BOOLEAN NOT NULL,
 raw_sha256 VARCHAR NOT NULL,
 metadata_json VARCHAR NOT NULL,
 archive_raw VARCHAR NOT NULL,
 archive_metadata VARCHAR NOT NULL,
 CHECK((validity_kind='observed_only' AND effective_from IS NULL AND effective_to IS NULL) OR
       (validity_kind='source_interval' AND nominal_from IS NOT NULL AND effective_from IS NOT NULL AND effective_to>=effective_from))
);
CREATE TABLE IF NOT EXISTS tariff_product (
 snapshot_id VARCHAR REFERENCES tariff_snapshot(snapshot_id),
 varenummer VARCHAR NOT NULL CHECK(regexp_full_match(varenummer,'[0-9]{6}')),
 atc VARCHAR,
 name VARCHAR, strength VARCHAR, form VARCHAR, pack VARCHAR,
 marketing_status VARCHAR, delivery_status VARCHAR, withdrawal_date VARCHAR,
 substitution_group VARCHAR, legacy_substitution_group VARCHAR,
 esp DECIMAL(20,6), tsp DECIMAL(20,6),
 ddd_per_pack DOUBLE,
 ddd_provenance VARCHAR NOT NULL,
 esp_per_ddd DOUBLE, tsp_per_ddd DOUBLE,
 source_esp_per_ddd DOUBLE, source_tsp_per_ddd DOUBLE,
 quality_flags VARCHAR NOT NULL,
 row_hash VARCHAR NOT NULL,
 raw_json VARCHAR NOT NULL,
 PRIMARY KEY(snapshot_id,varenummer)
);
CREATE INDEX IF NOT EXISTS consumption_month_atc ON consumption_observation(month,atc5);
CREATE INDEX IF NOT EXISTS tariff_varenummer ON tariff_product(varenummer);
