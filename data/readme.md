# Data for reproducibility

The files in this directory fall into two categories: (1) Input files needed to 
generate results for the manuscript and (2) Output files from the various 
analysis scripts. All are snappy compressed parquet files that can be read with 
any standard data analysis package, like `pandas` and `pyarrow` or `{arrow}`.

## 1. Input files

- The raw Safegraph data can be accessed with a data access contract. It can be 
  transformed into county-level estimates of $\sigma$ using the method described 
  in the manuscript. The resulting estimates are in 
  `indoor_outdoor_ratio_unsmoothed_WITHIN_CENTERED.parquet`
- Files from NOAA that describe county-level temperature and humidity over time.  
  These can be accessed 
  [here](https://psl.noaa.gov/data/gridded/data.narr.monolevel.html)
- The parquet file listing county centroids in lat/long comes from [this 
  site](/Users/zsusswein/Downloads/simplemaps_uscounties_basicv1/uscounties.csv)
