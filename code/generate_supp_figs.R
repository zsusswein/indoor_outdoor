# This file compares the indoor outdoor metric, sigma, and county-level measurements 
#of temperature and specific humidity

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(DBI)
library(duckdb)
library(arrow)
library(lubridate)
library(gt)

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
          FROM 'data/fips_modulclass.parquet'")

# Plot the differences between clusters
df <- read_parquet('data/sine_curve_cluster_fits.parquet') %>%
    select(modularity_class, term, estimate) %>%
    filter(modularity_class < 2) %>%
    mutate(modularity_class = case_when(
            modularity_class == 0 ~ 'Southern',
            modularity_class == 1 ~ 'Northern'),
          term = case_when(
            term == 'a' ~ "Amplitude",
            term == 'omega' ~ "Period",
            term == 'phi' ~ 'Phase')) %>%
    filter(!is.na(term))

p <- ggplot(df, aes(modularity_class, estimate))+
    geom_col()+
    facet_wrap(~term, scale = 'free')+
    theme_bw()+
    labs(x = element_blank(),
         y = 'Fitted value (note independent Y axes)')

ggsave('figures/sine_params.png', p)

df %>%
    pivot_wider(names_from = modularity_class, values_from = estimate) %>%
    mutate(diff = Northern - Southern) %>%
    gt() %>%
    tab_header('Estimates and cluster differences') %>%
    gtsave(filename = 'diff_table.html', 'figures/')


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

p <- df %>%
    select(-r_raw, -temp, -SH) %>%
    pivot_longer(c('z_temp', 'z_SH', 'z_r_raw')) %>%
    mutate(cluster_id = case_when(
                                  cluster_id == '0' ~ 'Southern',
                                  cluster_id == '1' ~ 'Northern',
                                  cluster_id == '2' ~ 'Tourism'),
           name = case_when(

                            name == 'z_SH' ~ 'Specific Humidity',
                            name == 'z_temp' ~ 'Temperature',
                            name == 'z_r_raw' ~ 'Sigma')) %>%
    filter(!is.na(cluster_id)) %>%
    group_by (week, cluster_id, name) %>%
    summarize(mean = mean(value, na.rm = T)) %>%
    ggplot(aes(week, mean, color = name, group = name))+
    geom_line()+
    facet_wrap(~cluster_id)+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    labs(x = element_blank(),
         y = 'Mean of Z scored value',
         color = 'Variable type')

ggsave('figures/environmental_vars.png', p, width = 10, height = 8)

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
    chr(modularity_class::integer + 65) AS clustid,
    week,
    AVG(SQRT(pow(s.pred_error, 2))) AS rmse,
    APPROX_QUANTILE(SQRT(POW(s.pred_error, 2)), 0.975) AS q975,
    APPROX_QUANTILE(SQRT(POW(s.pred_error, 2)), 0.025) AS q025
FROM 'data/sine_curve_cluster_preds.parquet' s
WHERE EXTRACT('year' FROM week) < 2020
GROUP BY 1, 2

")

df

p <- ggplot(df, aes(week, rmse, group = clustid, color = as.factor(clustid)))+
        geom_ribbon(aes(x = week,
                        ymin = q025,
                        ymax = q975,
                        fill = as.factor(clustid),
                        group = clustid), alpha = 0.2)+
        geom_line()+
        annotate(xmin = as.POSIXct(ymd('2019-05-01')), xmax = as.POSIXct(ymd('2019-9-1')), ymin = -Inf, ymax = Inf, geom = 'rect', alpha = 0.2)+
        theme_bw()+
        labs(x = element_blank(),
             y = 'Mean RMSE of sine curve fit',
             color = 'Cluster',
             fill = 'Cluster')+
        facet_wrap(~clustid)

ggsave('figures/sine_curve_rmse.png', p)

p <- df %>% 
    filter(clustid %in% c('A', 'B')) %>%
    mutate(clustid = if_else(clustid == 'A', 'Southern', 'Northern')) %>%
    ggplot( aes(week, rmse, group = clustid, color = as.factor(clustid)),
           )+
        geom_line()+
        annotate(xmin = as.POSIXct(ymd('2019-05-01')), xmax = as.POSIXct(ymd('2019-9-1')), ymin = -Inf, ymax = Inf, geom = 'rect', alpha = 0.2)+
        annotate(xmin = as.POSIXct(ymd('2018-05-01')), xmax = as.POSIXct(ymd('2018-9-1')), ymin = -Inf, ymax = Inf, geom = 'rect', alpha = 0.2)+
        theme_bw()+
        labs(x = element_blank(),
             y = 'Mean RMSE of sine curve fit',
             color = element_blank(),
             fill = element_blank())

ggsave('figures/sine_curve_rmse_north_south_only.png', p)



# Check the robustness of the estimates to the unclear locations
# File is generated by generate_sigma_with_uncertain_locations.sql
df <- read_parquet('data/sigma_with_unclear_locations.parquet')

p <- ggplot(df)+
    geom_line(aes(date, 
               indoor_with_unclear_diff,
               group = countyFIPS),
              alpha = 0.1)+
    geom_smooth(aes(date, 
                indoor_with_unclear_diff),
                se=F)+
    geom_hline(yintercept = 0, color = 'red')+
    theme_bw()

ggsave('figures/sigma_indoor_and_uncertain_combined.png', p)

duckdb::duckdb_shutdown(drv)
