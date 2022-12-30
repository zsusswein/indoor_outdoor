# This file generates county-week estimates of the indoor/outdoor ratio. These are the 
# estimates pre-smoothing. 

###########
# Libraries

library(tidyverse)
library(lubridate)
library(arrow)

############
# Set path to raw data from SafeGraph

df <- read_parquet('data/mobility_summarized.parquet') %>%
    rename(fips = countyFIPS) %>% 
    mutate(week = round_date(date, unit = 'week')) %>% 
    group_by(week, fips) %>% 
    summarize(raw_visitor_counts_Indoors = mean(raw_visitor_counts_Indoors, na.rm=T),  # collapse to county-week
           raw_visitor_counts_Outdoor = mean(raw_visitor_counts_Outdoor, na.rm=T),
           indoor_max_2019,
           outdoor_max_2019,
           median_dwell_Indoors,
           median_dwell_Outdoor,
           .groups = 'drop') %>% 
    group_by(fips) %>% 
    fill(indoor_max_2019, outdoor_max_2019, .direction = 'updown') %>% 
    ungroup()
 

df.fips <- read_parquet('data/state_and_county_fips_master.parquet')

# county-neighbor pairs, pairs exists both i -> j and j -> i
df.neighbors <- read_csv('data/county_neighbors_fips.txt',
                         col_names = c('fips', 'neighb'),
                         col_types = 'ii')

############
# set up df for fips codes missing from neighbors dataset

# pull out all the fips codes in the neighbors dataset
a <- read_csv('data/county_neighbors_fips.txt',
              col_names = c('fips', 'neighb'),
              col_types = 'ii') %>% 
  select(fips) %>% 
  unique() %>% 
  pull(fips) 

# find the fips codes in the master list that are missing from the neighbors list
all_state_neighbors <- df.fips %>% 
  filter(!is.na(state), !(fips %in% a)) %>% 
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
  left_join(df %>% rename(neighb = fips)) %>% 
  group_by(fips, week) %>% 
  summarize(num_indoor = mean(raw_visitor_counts_Indoors, na.rm=T),
            num_outdoor = mean(raw_visitor_counts_Outdoor, na.rm=T),
            denom_indoor = mean(indoor_max_2019, na.rm=T),
            denom_outdoor = mean(outdoor_max_2019, na.rm =T)) %>% 
  mutate(r = (num_indoor / denom_indoor) / (num_outdoor / denom_outdoor)) %>% 
  filter(!is.na(week))

#############
# Calculate r, the ratio

# calculate r on large sample size counties, add small sample size estimates
df.r <- df %>% 
  filter(!(fips %in% f)) %>% 
  mutate(r = (raw_visitor_counts_Indoors/indoor_max_2019)/(raw_visitor_counts_Outdoor/outdoor_max_2019)) %>% 
  full_join(df.neighbors) %>% 
  full_join(df.fips %>% filter(!is.na(state)))%>% 
  complete(week, fips) %>%
  select(-state, -name) %>% 
  left_join(df.fips) %>% 
  #group_by(date) %>% 
  #mutate(z = (r - mean(r, na.rm=T)) / sd(r, na.rm=T)) %>% 
  select(week, fips, r) %>% 
  filter(!is.na(week), !is.na(fips)) %>% 
  group_by(fips) %>% 
  mutate(r = r / mean(r, na.rm=T)) %>% 
  ungroup()

df.r.weighted.dwell <- df %>% 
  filter(!(fips %in% f)) %>% 
  mutate(r = (raw_visitor_counts_Indoors*median_dwell_Indoors/indoor_max_2019)/(raw_visitor_counts_Outdoor*median_dwell_Outdoor/outdoor_max_2019)) %>% 
  full_join(df.neighbors) %>% 
  mutate(r = r / mean(r, na.rm=T)) %>% 
  full_join(df.fips %>% filter(!is.na(state)))%>% 
  complete(week, fips) %>%
  select(-state, -name) %>% 
  left_join(df.fips) %>% 
  #group_by(date) %>% 
  #mutate(z = (r - mean(r, na.rm=T)) / sd(r, na.rm=T)) %>% 
  select(week, fips, r) %>% 
  filter(!is.na(week), !is.na(fips))

###############

df.final <- full_join(df.r %>% rename(r_raw = r), df.r.weighted.dwell %>% rename(r_weighted_dwell = r))

write_parquet(df.final, 'data/indoor_outdoor_ratio_unsmoothed_WITHIN_CENTERED.parquet')

