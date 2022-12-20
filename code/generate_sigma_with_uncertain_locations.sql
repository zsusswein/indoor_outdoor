/* 

Get the maximum observed value in 2019 from the summary file. Do this for indoor
locations, outdoor locations, and both of the previous with "unclear" locations 
included to check robustness of the downstream sigma estimates to these locations.

This query is meant to be passed to a local DuckDB in-memory database, but would 
probably require minimal tweaking to run with SQLite or Postgres.

*/
COPY (
WITH maximums AS (

SELECT 

    date,
    countyFIPS,
    MAX(raw_visitor_counts_Indoors) FILTER (WHERE EXTRACT('year' FROM date) = 2019)
         OVER (PARTITION BY countyFIPS)
         AS indoor_max_2019,
    MAX(raw_visitor_counts_Outdoor) FILTER (WHERE EXTRACT('year' FROM date) = 2019)
        OVER (PARTITION BY countyFIPS)
        AS outdoor_max_2019,
    MAX(raw_visitor_counts_Indoors + raw_visitor_counts_Unclear) FILTER
        (WHERE EXTRACT('year' FROM date) = 2019)
        OVER (PARTITION BY countyFIPS)
        AS indoor_with_unclear_max_2019,
    MAX(raw_visitor_counts_Outdoor + raw_visitor_counts_Unclear) FILTER
        (WHERE EXTRACT('year' FROM date) = 2019)
        OVER (PARTITION BY countyFIPS)
        AS outdoor_with_unclear_max_2019,
    raw_visitor_counts_Indoors AS indoor_raw,
    raw_visitor_counts_Outdoor AS outdoor_raw,
    raw_visitor_counts_Unclear AS unclear_raw

FROM 'data/mobility_summarized.parquet'

), raw_sigma AS (

SELECT
    date,
    countyFIPS,
    (indoor_raw / indoor_max_2019) / (outdoor_raw / outdoor_max_2019) AS sigma,
    ((indoor_raw + unclear_raw) / indoor_with_unclear_max_2019) / 
        ((outdoor_raw / outdoor_max_2019)) AS sigma_indoor_with_unclear,
    (indoor_raw / indoor_max_2019) / 
        ((outdoor_raw + unclear_raw) / outdoor_max_2019) AS sigma_outdoor_with_unclear,

FROM maximums

), centered_sigma AS (

SELECT 
    date,
    countyFIPS,
    sigma / MEAN(sigma) OVER (PARTITION BY countyFIPS) AS sigma_centered,
    sigma_indoor_with_unclear 
        / MEAN(sigma_indoor_with_unclear) 
        OVER (PARTITION BY countyFIPS) AS sigma_indoor_with_unclear_centered,
    sigma_outdoor_with_unclear
        / MEAN(sigma_outdoor_with_unclear)
        OVER (PARTITION BY countyFIPS) AS sigma_outdoor_with_unclear_centered

FROM raw_sigma

)

SELECT 
    *,
    sigma_centered - sigma_indoor_with_unclear_centered AS indoor_with_unclear_diff,
    sigma_centered - sigma_outdoor_with_unclear_centered AS outdoor_with_unclear_diff
FROM centered_sigma

) TO 'data/sigma_with_unclear_locations.parquet' (FORMAT PARQUET)



