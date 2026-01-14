library(tidyverse)
library(TNRS)


rasha <- read_csv('./data/raw/rasha_res_2026_01_12/crossval_class_report_with_counts.csv')

rasha_spp <- rasha %>% 
  mutate(class_name_short = str_remove(class_name, "-.*$")) %>% 
  rename(sp = class_name_short)

rasha_spp_to_tnrs <- rasha_spp %>% 
  mutate(id = 1:nrow(.)) %>% 
  select(id, sp) %>% 
  as.data.frame()

# TNRS match
rasha_taxa_wcvp <- TNRS(rasha_spp_to_tnrs, sources = 'wcvp')  

rasha_taxa_wcvp_clean <- rasha_taxa_wcvp %>% 
  select(Name_submitted, Accepted_species) %>% 
  distinct()

# match to original table and clean
rasha_clean <- rasha_spp %>% 
  left_join(rasha_taxa_wcvp_clean, by = c("sp" = "Name_submitted")) %>% 
  select(wcvp_accepted_name = Accepted_species,
         f1)

# add f1-score
abun_table <- read_csv('./data/processed/species_abun_photos.csv') %>% 
  left_join(rasha_clean)

write_csv(abun_table, './data/processed/species_abun_crowns_f1.csv', na = '')
