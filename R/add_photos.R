library(remotes)
install_github("yonghah/esri2sf")
library(esri2sf)
library(TNRS)

url <- "https://services3.arcgis.com/c0JB6JJmyYxhWn0X/ArcGIS/rest/services/BCI_wpt_photos_AGOL/FeatureServer/5"
photos <- esri2sf(url)

labels <- photos %>% 
  st_drop_geometry() %>% 
  filter(!is.na(GBIFname)) %>% 
  select(GBIFname) %>% 
  as_tibble() %>% 
  group_by(GBIFname) %>% 
  count()

photos_species <- labels %>% 
  ungroup() %>% 
  mutate(id = 1:nrow(.)) %>% 
  select(id, GBIFname) %>% 
  as.data.frame()

photos_species_tnrs <- TNRS(photos_species)

labels_tnrs <- labels %>% 
  ungroup() %>% 
  bind_cols(as_tibble(photos_species_tnrs)) %>% 
  select(wcvp_accepted_name = Accepted_species, n_crowns = n)

combined <- read_csv('./data/processed/wright_vasquez_combined.csv') %>% 
  left_join(labels_tnrs) %>% 
  mutate(n_crowns = replace(n_crowns, is.na(n_crowns), 0))

sum(combined$n_crowns, na.rm = T)


write_csv(combined, './data/processed/species_abun_photos.csv')

