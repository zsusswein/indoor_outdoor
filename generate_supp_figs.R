# This file compares the indoor outdoor metric, sigma, and county-level measurements of temperature and specific humidity


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(DBI)
library(duckdb)
library(gt)

# Load in data ------------------------------------------------------------


con = dbConnect(duckdb::duckdb(), dbdir=":memory:")

dbExecute(con,
          "
          CREATE TABLE df AS
          SELECT 
            sigma.fips,
            sigma.week,
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
          AND sigma.fips = SH.fips;

          ")

dbExecute(con,
          "
          
          CREATE OR REPLACE TABLE summaries AS
          SELECT
            fips,
            CORR(z_r_raw, z_temp),
            CORR(z_r_raw, z_SH),
            CORR(z_temp, z_SH) 
          FROM df
          GROUP BY fips;
          
          ")

df <- dbGetQuery(con, "SELECT * FROM df;")

summaries <- dbGetQuery(con, "SELECT * FROM summaries;")

dbGetQuery(con, "SUMMARIZE summaries;") %>% 
  select(column_name, min, max, avg, std, q25, q50, q75) %>% 
  filter(column_name != "fips") %>% 
  gt()


df %>% 
  filter(fips < 2000) %>% 
  ggplot()+
  geom_line(aes(week, z_r_raw, group = fips, color = fips), linetype = 2)+
  geom_line(aes(week, z_temp, group = fips, color = fips), linetype = 1)
  


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





