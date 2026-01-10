library(sf)
library(tidyverse)
library(TNRS)

vasquez2020 <- read_sf('./data/raw/vasquez_crownmaps/BCI_50ha_2020_08_01_crownmap_improved.shp')
vasquez2022 <- read_sf('./data/raw/vasquez_crownmaps/BCI_50ha_2022_09_29_crownmap_improved.shp')

vasquez2020trees <- vasquez2020 %>%
  select(tag,
         scientific_name = Latin,
         crown_area = crownArea) %>%
  filter(!is.na(scientific_name)) %>% 
  st_drop_geometry()

vasquez2022trees <- vasquez2022 %>%
  select(tag,
         scientific_name = latin,
         crown_area = crown_area) %>%
  filter(!is.na(scientific_name)) %>% 
  st_drop_geometry() %>% 
  mutate(tag = as.numeric(tag))

# find unique taxa across both lists
vasquez_taxa <- bind_rows(vasquez2020trees,
                          vasquez2022trees) %>% 
  select(scientific_name) %>% 
  distinct() %>% 
  arrange(scientific_name)

# data frame for TNRS
vasquez_taxa_df <- vasquez_taxa %>% 
  mutate(id = 1:nrow(.)) %>% 
  select(id, scientific_name) %>% 
  as.data.frame()

# TNRS match
vasquez_taxa_wcvp <- TNRS(vasquez_taxa_df, sources = 'wcvp')

# TNRS clean
vasquez_taxa_wcvp_clean <- vasquez_taxa_wcvp %>% 
  filter(Accepted_name_rank == 'species') %>% 
  select(scientific_name = Name_submitted,
         wcvp_accepted_name = Accepted_species)

# join the WCVP names to Vasquez
vasquez2020trees_wcvp <- vasquez2020trees %>% 
  right_join(vasquez_taxa_wcvp_clean) %>% 
  select(wcvp_accepted_name, crown_area) %>% 
  group_by(wcvp_accepted_name) %>% 
  summarise(crown_area_sum = sum(crown_area)) %>% 
  mutate(rel_abun = crown_area_sum / sum(crown_area_sum)) %>% 
  arrange(desc(rel_abun)) %>% 
  select(-crown_area_sum) %>% 
  mutate(dataset = "vasquez2020")

vasquez2022trees_wcvp <- vasquez2022trees %>% 
  right_join(vasquez_taxa_wcvp_clean) %>% 
  select(wcvp_accepted_name, crown_area) %>% 
  filter(!is.na(crown_area)) %>% 
  group_by(wcvp_accepted_name) %>% 
  summarise(crown_area_sum = sum(crown_area)) %>% 
  mutate(rel_abun = crown_area_sum / sum(crown_area_sum)) %>% 
  arrange(desc(rel_abun)) %>% 
  select(-crown_area_sum) %>% 
  mutate(dataset = "vasquez2022")

# put both together, calc average rel abun, and save as csv
vasquez_final <- bind_rows(vasquez2020trees_wcvp,
                           vasquez2022trees_wcvp) %>% 
  group_by(wcvp_accepted_name) %>% 
  summarise(abun = mean(rel_abun)) %>%
  mutate(rel_abun = abun / sum(abun)) %>% 
  arrange(desc(rel_abun)) %>% 
  select(-abun)

write_csv(vasquez_final, "./data/processed/vasquez_final.csv")
