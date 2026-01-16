library(TNRS)
library(tidyverse)
library(rgbif)

labelbox_taxa <- read_csv("https://raw.githubusercontent.com/PanamaForestGEO/Panama_plant_species_lists/refs/heads/master/labelbox_lists/labelbox_bci_completelist.csv")

labelbox_taxa_name <- labelbox_taxa %>% 
  select(gbif_accepted_scientific_name) %>% 
  distinct() %>% 
  arrange(gbif_accepted_scientific_name)
  
taxa_to_tnrs_df <- labelbox_taxa_name %>% 
  mutate(id = 1:nrow(.)) %>% 
  select(id, gbif_accepted_scientific_name) %>% 
  as.data.frame()

taxa_tnrs <- TNRS(taxa_to_tnrs_df, sources = 'wcvp')

taxa_tnrs_clean <- taxa_tnrs %>% 
  select(gbif_accepted_scientific_name = Name_submitted,
         wcvp_accepted_name = Accepted_name,
         taxonomic_rank = Accepted_name_rank,
         wcvp_url = Accepted_name_url,
         source = Source)

labelbox_wcvp <- labelbox_taxa %>% 
  left_join(taxa_tnrs_clean)

write_csv(labelbox_wcvp, "./data/processed/labelbox_wcvp.csv", na = "")
