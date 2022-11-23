# This file compares the indoor outdoor metric, sigma, and county-level measurements of temperature and specific humidity


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(DBI)
library(duckdb)
library(gt)

here::i_am('generate_supp_figs.R')

# Load in data ------------------------------------------------------------


con = dbConnect(duckdb::duckdb(), dbdir=":memory:")

dbExecute(con,
          "
          
          CREATE TABLE clustid AS 
          SELECT  
            node AS fips,
            modularity_class AS cluster_id
          FROM 'data/fips_modulclass.parquet'
          ")

 dbExecute(con,
           "
           
          CREATE OR REPLACE TABLE trimmed_df AS 
           
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
           FROM 'data/indoor_outdoor_ratio_unsmoothed.parquet'
           
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
          FROM 'data/indoor_outdoor_ratio_unsmoothed.parquet' sigma
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

df <- dbGetQuery(con, "SELECT * FROM df WHERE cluster_id IS NOT NULL;")

summaries <- dbGetQuery(con, "SELECT * FROM summaries;")

dbGetQuery(con, "SUMMARIZE summaries;") %>% 
  select(column_name, min, max, avg, std, q25, q50, q75) %>% 
  filter(column_name != "fips") %>% 
  gt()


p1 <- df %>% 
  ggplot()+
  geom_smooth(aes(week, z_r_raw, group = cluster_id, color = cluster_id), linetype = 2, se = F)+
  geom_smooth(aes(week, z_temp, group = cluster_id, color = cluster_id), linetype = 1, se = F)+
  theme_bw()+
  labs(x = element_blank(),
       y = 'GAM fit to trimmed Z scores by cluster',
       color = 'Cluster ID')

ggsave('figures/S1.png', p1)
  


ggplot(df)+
  geom_line(aes(z_SH, z_temp, group = fips, color = z_r_raw), alpha = .01)+
  geom_abline()



ggplot(summaries)+
  geom_point(aes(`corr(z_r_raw, z_temp)`, 
                 `corr(z_r_raw, "z_SH")`, 
                 color = `corr(z_temp, "z_SH")`))

ggplot(summaries)+
  geom_point(aes(`corr(z_r_raw, z_temp)`, 
                 `corr(z_temp, "z_SH")`,
                 color =  `corr(z_r_raw, z_temp)`))





