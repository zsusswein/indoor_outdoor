



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
