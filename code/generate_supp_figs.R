# This file compares the indoor outdoor metric, sigma, and county-level measurements 
#of temperature and specific humidity

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(DBI)
library(duckdb)

here::i_am('code/generate_supp_figs.R')

# Load in data ------------------------------------------------------------

con = dbConnect(duckdb::duckdb(), dbdir=":memory:")

# Pull the county cluster assignments into memory
dbExecute(con,
          "
          
          CREATE TABLE clustid AS 
          SELECT  
            node AS fips,
            modularity_class AS cluster_id
          FROM 'data/fips_modulclass.parquet'
          ")

# Join temp, specific humidity, and indoor/outdoor metric ("sigma") together and
# z-score each so they can be directly compared (i.e., all centered on 0 and deviations
# are reported on the same scale.
dbExecute(con,
          "
          CREATE OR REPLACE TABLE df AS
          SELECT 
            sigma.fips,
            sigma.week,
            clustid.cluster_id,
            sigma.r_raw,
            temp.temp,
            SH.SH,
            (r_raw - AVG(r_raw) OVER ()) / STDDEV_POP(r_raw) OVER () AS z_r_raw,
            (temp - AVG(temp) OVER ()) / STDDEV_POP(temp) OVER () AS z_temp,
            (SH - AVG(SH) OVER ()) / STDDEV_POP(SH) OVER () AS z_SH
          FROM 'data/indoor_outdoor_ratio_unsmoothed_WITHIN_CENTERED.parquet' sigma
          INNER JOIN (
          
            SELECT 
              fips,
              date,
              temp
            FROM 'data/county_level_temp_kelvin_2018_2019.parquet'
            
            ) temp
          ON sigma.week = temp.date 
          AND sigma.fips = temp.fips
          INNER JOIN (
          
            SELECT
              fips,
              date,
              SH
            FROM 
              'data/county_level_specific_humidity_2018_2019.parquet'
              
          ) SH
          ON sigma.week = SH.date
          AND sigma.fips = SH.fips
          LEFT JOIN clustid ON clustid.fips = sigma.fips;


          ")

# Repeat the procedure from above, but pre-compute the z-scores ("z_raw_original")
# trim off any that are above 5. This is meant as a robustness check to ensure that
# any correlations or estimates of central tendency aren't driven by a few outliers
# or some weird hard to spot tail behavior.
dbExecute(con,
           "
           
          CREATE OR REPLACE TABLE trimmed_df AS 
           
          -- Z score indoor/outdoor metric, humidity and temp
          SELECT 
            sigma.fips,
            sigma.week,
            clustid.cluster_id,
            sigma.r_raw,
            temp.temp,
            SH.SH,
            (r_raw - AVG(r_raw) OVER ()) / STDDEV_POP(r_raw) OVER () AS z_r_raw,
            (temp - AVG(temp) OVER ()) / STDDEV_POP(temp) OVER () AS z_temp,
            (SH - AVG(SH) OVER ()) / STDDEV_POP(SH) OVER () AS z_SH
          FROM 
            (
            
                       
           SELECT
            *,
            (r_raw - AVG(r_raw) OVER ()) / STDDEV_POP(r_raw) OVER () AS z_r_raw_original
           FROM 'data/indoor_outdoor_ratio_unsmoothed_WITHIN_CENTERED.parquet'
           
            ) sigma
          INNER JOIN (
          
            SELECT 
              fips,
              date,
              temp
            FROM 'data/county_level_temp_kelvin_2018_2019.parquet'
            
            ) temp
          ON sigma.week = temp.date 
          AND sigma.fips = temp.fips
          INNER JOIN (
          
            SELECT
              fips,
              date,
              SH
            FROM 
              'data/county_level_specific_humidity_2018_2019.parquet'
              
          ) SH
          ON sigma.week = SH.date
          AND sigma.fips = SH.fips
          LEFT JOIN clustid ON clustid.fips = sigma.fips
          WHERE abs(z_r_raw_original) < 5 ;
           
           ")

# Get the pearson correlation between all the combinations of the three metrics
dbExecute(con,
          "
          
          CREATE OR REPLACE TABLE summaries AS
          SELECT
            cluster_id,
            CORR(z_r_raw, z_temp),
            CORR(z_r_raw, z_SH),
            CORR(z_temp, z_SH) 
          FROM trimmed_df
          GROUP BY cluster_id;
          
          ")

# Pull the processed results into memory
df <- dbGetQuery(con, "SELECT * FROM df WHERE cluster_id IS NOT NULL;")
summaries <- dbGetQuery(con, "SELECT * FROM summaries;")
dbGetQuery(con, "SUMMARIZE summaries;") %>% 
  select(column_name, min, max, avg, std, q25, q50, q75) %>% 
  filter(column_name != "fips")


p1 <- df %>% 
  ggplot()+
  geom_smooth(aes(week, z_r_raw, group = cluster_id, color = cluster_id), linetype = 2, se = F)+
  geom_smooth(aes(week, z_temp, group = cluster_id, color = cluster_id), linetype = 1, se = F)+
  theme_bw()+
  labs(x = element_blank(),
       y = 'GAM fit to Z scores by cluster',
       color = 'Cluster ID')

ggsave('figures/S1.png', p1)
  

df <- dbGetQuery(con, 
                 "
SELECT
    s.week,
    c.clustid,
    AVG(s.pred_error**2) AS rmse,
    APPROX_QUANTILE(s.pred_error**2, 0.975) AS q975,
    APPROX_QUANTILE(s.pred_error**2, 0.025) AS q025)
FROM read_csv_auto('data/sine_curve_cluster_preds.parquet') s
JOIN clustid c
ON c.fips = s.fips
GROUP BY c.clustid

")

p <- ggplot(df, aes(week, rmse, group = clustid, color = as.factor(clustid))+
        geom_line()+
        geom_ribbon(aes(x = week,
                        ymin = q025,
                        ymax = q975,
                        fill = as.factor(clustid),
                        group = clustid))+
        theme_bw()+
        labs(x = element_blank(),
             y = 'Mean RMSE of sine curve fit',
             color = 'Cluster',
             fill = 'Cluster')

ggsave('figures/sine_curve_rmse.png', p)
