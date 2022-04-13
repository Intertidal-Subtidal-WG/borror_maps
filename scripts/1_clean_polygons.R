# script to load shapefiles from GIS output, and clean into valid polygons.
# some polygons from GIS files will have multiple species for one area,
# (i.e. "Kelp and Uchins"), so here, we split these into two identical polygons
# but add a "percent_cover" field, which we value as 1/number_species, so 
# "Kelp and urchins" would yield 2 identical polygons, one for "50% kelp" and 
# one for "50% urchins"



# libraries ----------------------------------------------------------------
library(sf)
library(tidyverse)
library(here)



# load shapefiles ---------------------------------------------------------
m82 <- read_sf(here("output maps","Maps perimeter intersect","1982_perim_intersect.shp")) %>% janitor::clean_names()
m83 <- read_sf(here("output maps","Maps perimeter intersect","1983_perim_intersect.shp")) %>% janitor::clean_names()
m84 <- read_sf(here("output maps","Maps perimeter intersect","1984_perim_intersect.shp")) %>% janitor::clean_names()
m87 <- read_sf(here("output maps","Maps perimeter intersect","1987_perim_intersect.shp")) %>% janitor::clean_names()
m90 <- read_sf(here("output maps","Maps perimeter intersect","1990_perim_intersect.shp")) %>% janitor::clean_names()
m14 <- read_sf(here("output maps","Maps perimeter intersect","2014_perim_intersect.shp")) %>% janitor::clean_names()

maine <- read_sf(here("data","Maine_State_Boundary_Polygon_Feature","Maine_State_Boundary_Polygon_Feature.shp"))
maine <- st_transform(maine, crs =4326 )
appledore <- maine %>%
  filter(OBJECTID == 5928)

rm(maine)

# check that all have same columns
colnames(m82)
colnames(m83)
colnames(m84)
colnames(m87)
colnames(m90)
colnames(m14)

# merge individual maps
shapes_full <- 
  bind_rows(m82,
            m83,
            m84,
            m87,
            m90,
            m14)

rm(m82, m83, m84, m87, m90, m14)


shapes_full %>%
  ggplot() +
  geom_sf(aes(fill=species),
          show.legend = F) +
  facet_wrap(~year) +
  ggthemes::theme_map()

shapes_full %>%
  as_tibble() %>%
  distinct(species) %>% pull

# separate polygons of mixed species --------------------------------------
shapes_cleaned <- shapes_full %>%
  
  # change all "and"s to commas so mixes are listed in comma-separated form
  mutate_at(.vars = "species", .funs = str_replace, pattern =" and", replacement = ",") %>%
  
  # find max xpecies listed in one polygon
  group_by(year) %>%
  # add arbitrary indexing number
  mutate(polygon_number = c(1:n())) %>%
  ungroup() %>%
  # count number of species in each polygon
  mutate(n_species =  str_count(species, ',')+1) %>% #arrange(desc(n_species)) # max mix is 4 species
  
  
  # separate shared species by comma
  separate(species, into =c("species1","species2","species3","species4"), sep = ",", fill="right") %>%
  
  
  # pivot 4 species columns into 1
  pivot_longer(cols = c("species1","species2","species3","species4"), names_to = "spp_number", values_to = "species") %>%
  drop_na(species)  %>%
  # drop "mix" and "mixed" and "sparse"
  mutate_at(.vars = "species", .funs = str_replace, pattern =" mix", replacement = "") %>%
  mutate_at(.vars = "species", .funs = str_replace, pattern ="Mixed ", replacement = "") %>%
  mutate_at(.vars = "species", .funs = str_replace, pattern ="sparse ", replacement = "") %>% 
  ## need to change this line if considering sparseness as quantitative
  
  
  # remove white space 
  mutate(species = str_trim(species)) %>%
  
  # manually make some changes
  mutate(species_general = case_when(species %in% c("Saccharina latissima", "Saccharina longicruris","Saccharina", "Kelp", "light kelp") ~ "Saccharina",
                                     species %in% c("Reds only","red algae","Red algae","Gigartinales","Tough coralline", "Red") ~ "Mixed reds",
                                     species %in% c("Chorda filum","Halosiphon tomentosus","Halosiphon tomentosus")  ~ "Rope Kelps",
                                     TRUE ~ species)) %>%
  
  # add column for percent cover 
  # (if a species is alone in it's polygon, assigned 100% cover,
  # if two species share a polygon, they are each assigned 50% cover)
  mutate(percent_cover = 1/n_species) %>%
  
  select(year, species, species_general, percent_cover, geometry)

rm(shapes_full)


save(shapes_cleaned,
     file = here("data","shapes_to_plot","spp_shapes_cleaned.rds"))

save(appledore,
     file = here("data","shapes_to_plot","appledore.rds"))



# if desired, make a target_species column to subset the data in the map 
# easy for integration into shiny - - see commented out line below
unique(shapes_cleaned$species_general)
target_species <- unique(shapes_cleaned$species_general)[1]

appledore %>%
  ggplot() +
  geom_sf(alpha=.4, size=.15) +
  geom_sf(data = shapes_cleaned #%>% filter(species_general %in% target_species)
          ,
          aes(alpha=I(percent_cover), geometry = geometry, fill=species_general, color = species_general),
          size=.05,
          show.legend = F) +
  ggthemes::theme_map() +
  facet_grid(species_general~year) +
  theme(strip.text = element_text(size=13),
        strip.text.y = element_text(size=11),
        panel.border = element_rect(size=.2, fill="transparent"))