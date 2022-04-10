# This file generates figure 1

################

library(tidyverse)

################

d <- read_csv('data/indoor_outdoor_ratio_unsmoothed.csv',
              col_types = 'Tfdd')

lat <- read_csv('~/Downloads/simplemaps_uscounties_basicv1.71/uscounties.csv') %>% 
  select(county_fips, lat) %>% 
  rename(fips = county_fips) %>% 
  mutate(fips = as.double(fips)) %>% 
  mutate(lat = rank(lat),
         fips = as.factor(fips))


d %>% 
  filter(!is.na(week), !is.na(r_raw)) %>% 
  mutate(r_raw = if_else(r_raw > 4, 4, r_raw)) %>% 
  left_join(lat) %>% 
  mutate(fips = as.factor(fips)) %>% 
  group_by(fips) %>% 
  #arrange(lat) %>% 
  ggplot()+
  geom_raster(aes(week, fips, fill = r_raw))+
  scale_fill_viridis_c()+
  labs(x='', y = 'County', fill = 'Phi')+
  theme(axis.text.y=element_blank())
ggsave('~/Desktop/heatmap_draft.jpeg')
