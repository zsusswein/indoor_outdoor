# This file generates the fitted curves to each of the four identified clusters

##################
# Libraries

library(tidyverse)
library(lubridate)
library(lme4)
library(nlme)
library(broom.mixed)
library(arrow)

here::i_am('code/generate_modularity_class_fits.R')

##################s
# Read in data

clusters <- read_parquet('data/community_structure_2019_90_imputeddata_Nov10.parquet') %>% 
  rename(fips = node) %>% 
  mutate(fips = as.double(fips))

df.fips <- read_parquet('data/state_and_county_fips_master.parquet') %>% 
  mutate(fips = ifelse(fips == 02270, 02158, fips),
         fips = ifelse(fips == 46113, 46102, fips))

df.full <- read_parquet('data/indoor_outdoor_ratio_unsmoothed_WITHIN_CENTERED.parquet') %>% 
  left_join(df.fips) %>% 
  left_join(clusters) %>% 
  filter(!is.na(fips), !is.na(state), !is.na(week), !is.na(modularity_class)) %>% 
  group_by(fips) %>% 
  arrange(week) %>%
  mutate(t = row_number()) %>% 
  ungroup() %>% 
  mutate(state = as.factor(state))

###########
# Fit sine curve estimates 

params <- df.full %>% 
  mutate(r_raw = as.double(r_raw)) %>% 
  nest(data = -modularity_class) %>% 
  mutate(fit = map(data, ~ nls(r_raw ~ a*sin(omega*(t+phi))+c, 
                               data=.x, 
                               start=c(a=.25,omega=.127,phi=1,c=.98))),
         tidied = map(fit, tidy),
         preds = map(fit, predict, newdata = tibble(t=1:182)))

###########
# Save sine curve fits

params %>% 
  select(modularity_class, tidied) %>% 
  unnest(tidied) %>% 
  write_parquet('data/sine_curve_cluster_fits.parquet')

params %>% 
  select(modularity_class, preds) %>% 
  unnest(preds) %>% 
  group_by(modularity_class) %>% 
  mutate(t = row_number()) %>% 
  ungroup() %>% 
  left_join(df.full %>% select(week, t, r_raw) %>% unique()) %>% 
  mutate(pred_error = preds - r_raw) %>%
  write_parquet('data/sine_curve_cluster_preds.parquet')

