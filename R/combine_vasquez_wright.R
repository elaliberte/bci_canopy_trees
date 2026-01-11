library(tidyverse)

wright <- read_csv('./data/processed/wright_final.csv')
vasquez <- read_csv('./data/processed/vasquez_final.csv')

combined <- bind_rows(wright,
                      vasquez) %>% 
  group_by(wcvp_accepted_name) %>% 
  summarise(mean_rel_abun = mean(rel_abun)) %>% 
  mutate(rel_abun = mean_rel_abun / sum(mean_rel_abun)) %>% 
  select(-mean_rel_abun) %>% 
  arrange(desc(rel_abun))

# how many species
combined %>% 
  select(wcvp_accepted_name) %>% 
  n_distinct() # 212

write_csv(combined, "./data/processed/wright_vasquez_combined.csv")
