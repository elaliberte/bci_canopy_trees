library(tidyverse)
library(readxl)
library(TNRS)

species <- read_excel('./data/raw/panama_sp_list/PanamaSpCombined_2025-11-18HM.xlsx') %>% 
  mutate(sp = str_to_upper(sp6)) %>% 
  select(sp,
         accepted_name = Accepted_name)

# get species
plot10ha_spp <- read_delim('./data/raw/wright_plots/10ha_WorkingDraft_20150304.txt') %>% 
  select(sp) %>%
  distinct() %>% 
  left_join(species) %>% 
  arrange(accepted_name) %>% 
  drop_na()

species_to_tnrs <- plot10ha_spp %>% 
  select(-sp) %>% 
  mutate(id = 1:nrow(.)) %>% 
  select(id, accepted_name) %>% 
  as.data.frame()

species_tnrs <- TNRS(species_to_tnrs)

species_tnrs_accepted <- species_tnrs %>% 
  as_tibble() %>% 
  select(Name_submitted, Accepted_species)

plot10ha_spp_accepted <- plot10ha_spp %>% 
  left_join(species_tnrs_accepted, by = c("accepted_name" = "Name_submitted")) %>% 
  select(-accepted_name) %>% 
  rename(accepted_name = Accepted_species)


plot10ha <- read_delim('./data/raw/wright_plots/10ha_WorkingDraft_20150304.txt') %>% 
  select(tag,
         sp,
         dbh = dbh14) %>%
  left_join(plot10ha_spp_accepted) %>% 
  filter(dbh >= 300) %>% 
  mutate(basal_area = pi * ((dbh/2)^2)) %>% 
  select(wcvp_accepted_name = accepted_name, basal_area) %>% 
  filter(!is.na(wcvp_accepted_name)) %>% 
  group_by(wcvp_accepted_name) %>% 
  summarise(sum_basal_area = sum(basal_area, na.rm = T)) %>% 
  mutate(rel_abun = sum_basal_area / sum(sum_basal_area, na.rm = T),
         dataset = "wright10ha") %>% 
  select(-sum_basal_area) %>% 
  arrange(desc(rel_abun))
  

plot25ha <- read_delim('./data/raw/wright_plots/25ha_WorkingDraft_20141001.txt') %>% 
  select(tag,
         sp,
         dbh = dbh14) %>% 
  mutate(dbh = as.numeric(dbh)) %>% 
  left_join(plot10ha_spp_accepted) %>% 
  filter(dbh >= 300) %>% 
  mutate(basal_area = pi * ((dbh/2)^2)) %>% 
  select(wcvp_accepted_name = accepted_name, basal_area) %>% 
  filter(!is.na(wcvp_accepted_name)) %>% 
  group_by(wcvp_accepted_name) %>% 
  summarise(sum_basal_area = sum(basal_area, na.rm = T)) %>% 
  mutate(rel_abun = sum_basal_area / sum(sum_basal_area, na.rm = T),
         dataset = "wright25ha") %>% 
  select(-sum_basal_area) %>% 
  arrange(desc(rel_abun))


plotava <- read_delim('./data/raw/wright_plots/AVA_WorkingDraft_20150605.txt') %>% 
  select(tag,
         sp,
         dbh = dbh14) %>% 
  left_join(plot10ha_spp_accepted) %>%
  filter(dbh >= 300) %>% 
  mutate(basal_area = pi * ((dbh/2)^2)) %>% 
  select(wcvp_accepted_name = accepted_name, basal_area) %>% 
  filter(!is.na(wcvp_accepted_name)) %>% 
  group_by(wcvp_accepted_name) %>% 
  summarise(sum_basal_area = sum(basal_area, na.rm = T)) %>% 
  mutate(rel_abun = sum_basal_area / sum(sum_basal_area, na.rm = T),
         dataset = "wrightava") %>% 
  select(-sum_basal_area) %>% 
  arrange(desc(rel_abun))


plotdrayton <- read_delim('./data/raw/wright_plots/Drayton_WorkingDraft_20141107.txt') %>% 
  select(tag,
         sp,
         dbh = dbh14) %>% 
  left_join(plot10ha_spp_accepted) %>% 
  filter(dbh >= 300) %>% 
  mutate(basal_area = pi * ((dbh/2)^2)) %>% 
  select(wcvp_accepted_name = accepted_name, basal_area) %>% 
  filter(!is.na(wcvp_accepted_name)) %>% 
  group_by(wcvp_accepted_name) %>% 
  summarise(sum_basal_area = sum(basal_area, na.rm = T)) %>% 
  mutate(rel_abun = sum_basal_area / sum(sum_basal_area, na.rm = T),
         dataset = "wrightdrayton") %>% 
  select(-sum_basal_area) %>% 
  arrange(desc(rel_abun))


plotpearson <- read_delim('./data/raw/wright_plots/Pearson_WorkingDraft_20141107.txt') %>% 
  select(tag,
         sp,
         dbh = dbh14) %>% 
  left_join(plot10ha_spp_accepted) %>% 
  filter(dbh >= 300) %>% 
  mutate(basal_area = pi * ((dbh/2)^2)) %>% 
  select(wcvp_accepted_name = accepted_name, basal_area) %>% 
  filter(!is.na(wcvp_accepted_name)) %>% 
  group_by(wcvp_accepted_name) %>% 
  summarise(sum_basal_area = sum(basal_area, na.rm = T)) %>% 
  mutate(rel_abun = sum_basal_area / sum(sum_basal_area, na.rm = T),
         dataset = "wrightpearson") %>% 
  select(-sum_basal_area) %>% 
  arrange(desc(rel_abun))


plotzetek <- read_delim('./data/raw/wright_plots/Zetek_WorkingDraft_20141107.txt') %>% 
  select(tag,
         sp,
         dbh = dbh14) %>% 
  left_join(plot10ha_spp_accepted) %>% 
  filter(dbh >= 300) %>% 
  mutate(basal_area = pi * ((dbh/2)^2)) %>% 
  select(wcvp_accepted_name = accepted_name, basal_area) %>% 
  filter(!is.na(wcvp_accepted_name)) %>% 
  group_by(wcvp_accepted_name) %>% 
  summarise(sum_basal_area = sum(basal_area, na.rm = T)) %>% 
  mutate(rel_abun = sum_basal_area / sum(sum_basal_area, na.rm = T),
         dataset = "wrightzetek") %>% 
  select(-sum_basal_area) %>% 
  arrange(desc(rel_abun))


plotcombined <- bind_rows(
  plot10ha,
  plot25ha,
  plotava,
  plotdrayton,
  plotpearson,
  plotzetek) %>% 
  group_by(wcvp_accepted_name) %>% 
  summarise(mean_rel_abun = mean(rel_abun)) %>% 
  mutate(rel_abun = mean_rel_abun / sum(mean_rel_abun)) %>% 
  select(-mean_rel_abun) %>% 
  arrange(desc(rel_abun))

write_csv(plotcombined, "./data/processed/wright_final.csv")
