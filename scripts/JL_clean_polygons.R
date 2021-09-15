# script to plot shapefiles

library(sf)
library(tidyverse)
library(here)

# can we find area of mixed vs pure

m82 <- read_sf(here("output maps","Map_Shapefiles","Borror_1982_done.shp")) %>% janitor::clean_names()
m83 <- read_sf(here("output maps","Map_Shapefiles","Borror_1983_done.shp")) %>% janitor::clean_names()
m84 <- read_sf(here("output maps","Map_Shapefiles","Borror_1984_done.2.shp")) %>% janitor::clean_names()
m87 <- read_sf(here("output maps","Map_Shapefiles","Borror_1987_done.2.shp")) %>% janitor::clean_names()
m90 <- read_sf(here("output maps","Map_Shapefiles","Borror_1990_done.shp")) %>% janitor::clean_names()
m14 <- read_sf(here("output maps","Map_Shapefiles","Borror_2014_done.shp")) %>% janitor::clean_names()

maine <- read_sf(here("data","Maine_State_Boundary_Polygon_Feature","Maine_State_Boundary_Polygon_Feature.shp"))
maine <- st_transform(maine, crs =4326 )
appledore <- maine %>%
  filter(OBJECTID == 5928)

rm(maine)

 colnames(m82)
 colnames(m83)
 colnames(m84)
 colnames(m87)
 colnames(m90)
 colnames(m14)

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



# ok now separate plygons that are mixed species


shapes_cleaned <- shapes_full %>%
  # number the polygons
  group_by(year) %>%
  mutate(polygon_number = c(1:n())) %>%
  ungroup() %>%
  mutate(n_species =  str_count(species, ',')+1) %>% #arrange(desc(n_species))

  
  mutate_at(.vars = "species", .funs = str_replace, pattern ="Kelp red algae", replacement = "Kelp, Red algae") %>% 
  mutate_at(.vars = "species", .funs = str_replace, pattern ="Tough,coralline", replacement = "Tough coralline") %>%
  
  
  # change all "and"s to commas
  mutate_at(.vars = "species", .funs = str_replace, pattern =" and", replacement = ",") %>%
  
  # separate shared species by comma
  separate(species, into =c("species1","species2","species3","species4"), sep = ",", fill="right") %>%
  
  
  # pivot 4 species columns into 1
  pivot_longer(cols = c("species1","species2","species3","species4"), names_to = "spp_number", values_to = "species") %>%
  drop_na(species)  %>%
  # drop "mix" and "mixed"
  mutate_at(.vars = "species", .funs = str_replace, pattern =" mix", replacement = "") %>%
  mutate_at(.vars = "species", .funs = str_replace, pattern ="Mixed ", replacement = "") %>%
  mutate_at(.vars = "species", .funs = str_replace, pattern ="sparse ", replacement = "") %>%
  
  
  # remove white space 
  mutate(species = str_trim(species)) %>%
  
  # manually make some changes
  mutate(species_general = case_when(species %in% c("Saccharina latissima", "Saccharina longicruris","Saccharina", "Kelp", "light kelp") ~ "Saccharina",
                                     species %in% c("Reds only","red algae","Red algae","Gigartinales","Tough coralline", "Red") ~ "Mixed reds",
                                     species %in% c("Chorda filum","Halosiphon tomentosus","Halosiphon tomentosus")  ~ "Rope Kelps",
                                     TRUE ~ species)) %>%
  
  mutate(percent_cover = 1/n_species) %>%
  
  select(year, species, species_general, percent_cover, geometry, area, perimeter)

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
    geom_sf(data = shapes_cleaned %>% filter(species_general %in% target_species)
            ,
            aes(alpha=I(percent_cover), geometry = geometry, fill=species_general, color = species_general),
            size=.05,
            show.legend = F) +
  ggthemes::theme_map() +
  facet_grid(species_general~year) +
  theme(strip.text = element_text(size=13),
        strip.text.y = element_text(size=11),
        panel.border = element_rect(size=.2, fill="transparent"))

