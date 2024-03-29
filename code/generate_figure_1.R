# This file generates figure 1

################

library(tidyverse)
library(arrow)

here::i_am('code/generate_figure_1.R')

################

d <- read_parquet('data/indoor_outdoor_ratio_unsmoothed_WITHIN_CENTERED.parquet')

lat <- read_parquet('data/uscounties.parquet') %>% 
  select(county_fips, lat) %>% 
  rename(fips = county_fips) %>% 
  mutate(fips = as.double(fips)) %>% 
  mutate(lat = rank(lat),
         fips = as.factor(fips))


p1 <- d %>% 
  filter(!is.na(week), !is.na(r_raw)) %>% 
  # fix the top value of r_raw at 4 for the colorscale in Fig. 1
  mutate(r_raw = if_else(r_raw > 4, 4, r_raw),
         fips = as.factor(fips)) %>% 
  left_join(lat) %>% 
  mutate(fips = as.factor(fips)) %>% 
  ggplot()+
  geom_raster(aes(week, fips, fill = r_raw))+
  scale_fill_viridis_c()+
  labs(x='', y = 'County', fill = 'Phi')+
  theme(axis.text.y=element_blank())

####################

ggsave('figures/heatmap_draft.jpeg', p1)
