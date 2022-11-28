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

