-- One capture can cover several regions, including regions with no observed rows.
CREATE TABLE consumption_extract_region (
 extract_id VARCHAR REFERENCES consumption_extract(extract_id),
 region VARCHAR CHECK(region IN ('DK','H','SJ','SD','MJ','NJ')),
 row_count BIGINT NOT NULL CHECK(row_count>=0),
 PRIMARY KEY(extract_id,region)
);
INSERT INTO consumption_extract_region
 SELECT extract_id,'DK',row_count FROM consumption_extract;
CREATE TABLE consumption_observation_with_region (
 extract_id VARCHAR,
 atc5 VARCHAR NOT NULL CHECK(regexp_full_match(atc5,'[A-Z][0-9]{2}[A-Z]{2}[0-9]{2}')),
 varenummer VARCHAR NOT NULL CHECK(regexp_full_match(varenummer,'[0-9]{6}')),
 month DATE NOT NULL CHECK(day(month)=1),
 metric VARCHAR NOT NULL CHECK(metric IN ('ddd','regionalt_tilskud_kr')),
 value DECIMAL(28,8) NOT NULL,
 region VARCHAR,
 PRIMARY KEY(extract_id,atc5,varenummer,month,metric,region),
 FOREIGN KEY(extract_id,region) REFERENCES consumption_extract_region(extract_id,region)
);
INSERT INTO consumption_observation_with_region
 SELECT extract_id,atc5,varenummer,month,metric,value,'DK' FROM consumption_observation;
DROP TABLE consumption_observation;
ALTER TABLE consumption_observation_with_region RENAME TO consumption_observation;
CREATE INDEX consumption_month_atc ON consumption_observation(month,atc5);
DELETE FROM schema_version;
INSERT INTO schema_version VALUES (3);
