# This file generates weekly estimates of the indoor/outdoor ratio

###########
# Libraries

library(mgcv)
library(broom)
library(purrr)
library(tidyverse)
library(lubridate)

############
# Data

df <- read_csv('data/weekly_patterns_countylevel_summarized2020.csv') %>% 
  rename(fips = countyFIPS) %>% 
  full_join(read_csv('/Volumes/GoogleDrive/.shortcut-targets-by-id/12V72oXyWtm3AbaVo2SKmwnTsnzJvFRii/COVID_BansalLab/DATA/Safegraph_data/PROCESSED_DATA/weekly_patterns_county_indooroutdoor_April2021/weekly_patterns_countylevel_summarized2021.csv') %>% 
              rename(fips = countyFIPS)) %>% 
  mutate(date = round_date(date, unit = 'week')) %>% 
  group_by(date, fips) %>% 
  mutate(raw_visitor_counts_Indoors = mean(raw_visitor_counts_Indoors, na.rm=T),
         raw_visitor_counts_Outdoor = mean(raw_visitor_counts_Outdoor, na.rm=T)) %>% 
  ungroup() %>% 
  group_by(fips) %>% 
  fill(indoor_max_2019, outdoor_max_2019, .direction = 'updown')

df.fips <- read_csv('data/state_and_county_fips_master.csv')


# county-neighbor pairs, pairs exists both i -> j and j -> i
df.neighbors <- read_csv('data/county_neighbors_fips.txt',
                         col_names = c('fips', 'neighb'),
                         col_types = 'ii')

############
# set up df for fips codes missing from neighbors dataset

a <- read_csv('data/county_neighbors_fips.txt',
              col_names = c('fips', 'neighb'),
              col_types = 'ii') %>% 
  pull(fips) 

all_state_neighbors <- df.fips %>% filter(!is.na(state), !(fips %in% a)) %>% 
  mutate(neighb  = fips) %>% 
  group_by(state) %>% 
  expand(neighb, fips) %>% 
  ungroup()


# Select fips codes to impute from nearby counties due to small sample size
f <- df %>% 
  full_join(df.fips %>% filter(!is.na(state))) %>% 
  filter(raw_visitor_counts_Indoors < 100 | 
           raw_visitor_counts_Outdoor < 80 | 
           is.na(raw_visitor_counts_Indoors) | 
           is.na(raw_visitor_counts_Outdoor) | 
           is.na(indoor_max_2019) |
           is.na(outdoor_max_2019) |
           outdoor_max_2019 < 100 |
           indoor_max_2019 < 100) %>% 
  pull(fips) %>% 
  unique()

# county-neighbor pairs, pairs exists both i -> j and j -> i, do imputation
df.neighbors <- read_csv('data/county_neighbors_fips.txt',
                         col_names = c('fips', 'neighb'),
                         col_types = 'ii') %>% 
  full_join(all_state_neighbors) %>% 
  filter(fips %in% f) %>% 
  left_join(df %>% select(date, fips, raw_visitor_counts_Indoors, raw_visitor_counts_Outdoor, indoor_max_2019, outdoor_max_2019) %>% rename(neighb = fips)) %>% 
  group_by(fips, date) %>% 
  summarize(num_indoor = mean(raw_visitor_counts_Indoors, na.rm=T),
            num_outdoor = mean(raw_visitor_counts_Outdoor, na.rm=T),
            denom_indoor = mean(indoor_max_2019, na.rm=T),
            denom_outdoor = mean(outdoor_max_2019, na.rm =T)) %>% 
  mutate(r = (num_indoor / denom_indoor) / (num_outdoor / denom_outdoor)) %>% 
  filter(!is.na(date))

# calculate r on large sample size counties, add small sample size estimates, replace with state mean if county-time obs z-score > 3
df.r <- df %>% 
  filter(!(fips %in% f)) %>% 
  mutate(r = (raw_visitor_counts_Indoors/indoor_max_2019)/(raw_visitor_counts_Outdoor/outdoor_max_2019)) %>% 
  full_join(df.neighbors) %>% 
  mutate(r = r / mean(r, na.rm=T)) %>% 
  full_join(df.fips %>% filter(!is.na(state)))%>% 
  complete(date, fips) %>%
  select(-state, -name) %>% 
  left_join(df.fips) %>% 
  #group_by(date) %>% 
  #mutate(z = (r - mean(r, na.rm=T)) / sd(r, na.rm=T)) %>% 
  select(date, fips, r) %>% 
  filter(!is.na(date), !is.na(fips))

df.r.smooth <- df.r %>% 
  filter(!is.na(r)) %>% 
  mutate(date_num = as.numeric(factor(date))) %>% 
  nest(data = -fips) %>% 
  mutate(test = map(data, ~ gam(.x$r ~ 1 + s(.x$date_num))),
         pred = map(test, ~predict(.x, se = T))) %>% 
  unnest_wider(c(pred)) %>% 
  unnest(data, fit, se.fit)


df.r.weighted.dwell <- df %>% 
  filter(!(fips %in% f)) %>% 
  mutate(r = (raw_visitor_counts_Indoors*median_dwell_Indoors/indoor_max_2019)/(raw_visitor_counts_Outdoor*median_dwell_Outdoor/outdoor_max_2019)) %>% 
  full_join(df.neighbors) %>% 
  mutate(r = r / mean(r, na.rm=T)) %>% 
  full_join(df.fips %>% filter(!is.na(state)))%>% 
  complete(date, fips) %>%
  select(-state, -name) %>% 
  left_join(df.fips) %>% 
  #group_by(date) %>% 
  #mutate(z = (r - mean(r, na.rm=T)) / sd(r, na.rm=T)) %>% 
  select(date, fips, r) %>% 
  filter(!is.na(date), !is.na(fips)) %>% 
  filter(!is.na(r)) %>% 
  mutate(date_num = as.numeric(factor(date))) %>% 
  nest(data = -fips) %>% 
  mutate(test = map(data, ~ gam(.x$r ~ 1 + s(.x$date_num))),
         pred = map(test, ~predict(.x, se = T))) %>% 
  unnest_wider(c(pred)) %>% 
  unnest(data, fit, se.fit) %>% 
  select(date, fips, r, fit, se.fit)

############

write_csv(df.r, 'data/indoor_outdoor_ratio.csv')
write_csv(df.r.smooth %>% select(fips, date, fit, se.fit), 'data/indoor_outdoor_ratio_smoothed.csv')
write_csv(df.r.weighted.dwell, 'data/indoor_outdoor_ratio_dwell_smoothed.csv')

###########
