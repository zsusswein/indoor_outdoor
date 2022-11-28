import duckdb
import glob
import os

con = duckdb.connect(":memory:")
files = glob.glob("data/*.csv")

for file in files:
    newfile = os.path.splitext(file)[0] + ".parquet"

    con.execute(f"""

    COPY (

    SELECT * FROM read_csv_auto('{file}', header=True)
    
    )

    TO '{newfile}' (FORMAT PARQUET)
    """)

con.execute("""

DESCRIBE (SELECT * FROM  'data/indoor_outdoor_ratio_unsmoothed.parquet')

            """)
con.fetchall()


con.execute("""

            COPY (

SELECT week :: DATE AS week,
       fips :: VARCHAR AS fips,
       TRY_CAST(r_raw AS DOUBLE) AS r_raw,
       TRY_CAST(r_weighted_dwell AS DOUBLE) AS r_weighted_dwell
FROM 'data/indoor_outdoor_ratio_unsmoothed.parquet'
            ) 

    TO 'data/indoor_outdoor_ratio_unsmoothed.parquet' (FORMAT PARQUET)

            """)
       
con.execute("""

SELECT * FROM 


