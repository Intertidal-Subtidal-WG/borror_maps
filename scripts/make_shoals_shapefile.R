library(sf)
library(ggplot2)
library(patchwork)
library(dplyr)
library(plotly)
library(lwgeom)

# for translation in a moment
feature_to_island <- tribble(
  ~FEATURE_ID, ~ISLAND,
  386078, "Appledore",
  211092, "Appledore",
  306689, "Appledore",
  229715, "Babbs Rock",
  88246, "Halftide Ledge",
  84144, "Smuttynose",
  419828, "Smuttynose",
  446078, "Cedar Island",
  659714, "Cedar Island",
  563479, "Smutty Jetty",
  636088, "Cedar Jetty",
  610660, "Cedar Jetty",
  229716, "Star Island",
  468721, "Star Island",
  124768, "Star Island"
  
  
)

shoals <- st_read("data/isles_of_shoals_shapefile/softcopyl1.shp") %>%
   filter(ATTRIBUTE %in% c("Natural.Mean High Water", "Man-made.Rip Rap",
                           "Breakers", "Pier.Fixed", "Pier.Floating"    )) %>%
  left_join(feature_to_island) %>%
  mutate(FEATURE_ID = as.character(FEATURE_ID),
         FEATURE_ID = ifelse(!is.na(ISLAND), ISLAND, FEATURE_ID)) %>%
  filter(!(FEATURE_ID %in% c(184087, 302122, 337582))) %>%
  group_by(FEATURE_ID) %>% 
  summarize(geometry = st_union(geometry,  is_coverage = TRUE),
            geometry = st_make_valid(geometry),
            geometry = st_cast(geometry, "POLYGON")
            )


ggplot(shoals, aes(fill = as.character(FEATURE_ID))) + 
  geom_sf()

