#libraries
library(tidyverse)
library(here)
library(sf)

#some themes
theme_set(ggthemes::theme_few())


# load data
load( here("data","shapes_to_plot","appledore.rds")) #shapefile of appledore

#polygons and cleanup
polys <- readRDS(here("data", "Appledore Polygons", "appledore_kml.rds")) 

#SE/SW overlap goes to SW
polys <- bind_rows(#polys[1:2,],
  polys[2,] %>% st_make_valid(), 
  polys[1,]%>% st_make_valid(),
  st_difference(polys[3,], polys[4,]),
  st_difference(polys[4,], polys[2,])) %>%
  mutate(geometry = st_cast(geometry, "POLYGON")) %>%
  ungroup()


#see how the polys lineup
ggplot(appledore) +
  geom_sf() +
  geom_sf(data = polys, alpha = 0.1, aes(fill = Name))


# add quadrant labels
appledore_split <- polys %>%
  group_by(Name) %>%
  nest() %>%
  summarize(cropped_lines = map(data, 
                                ~st_crop(appledore, .x))) %>%
  unnest(cropped_lines) %>% 
  st_as_sf(crs = 4326) %>%
  #factor order by exposure
  mutate(Name = factor(Name, levels = rev(c("Southeast",
                                            "Northeast",
                                            "Northwest",
                                            "Southwest")))) %>%
  rename(quadrant = Name) |>
  mutate(condition = c("Partially exposed\nflat reef", 
                       "Partially protected\nledges", 
                       "Fully exposed\nledges",
                       "Protected\nflat reef"))

saveRDS(appledore_split, "data/shapes_to_plot/appledore_split.rds")

# viz
ggplot() +
  geom_sf(data = appledore_split,
          mapping = aes(color = quadrant), 
          fill = NA, 
          lwd = 4) +
  geom_sf(data = appledore, fill = "white", color = "white") +
  ggrepel::geom_label_repel(data = appledore_split,
                mapping = aes(color = quadrant, 
                              geometry = geometry,
                              label = condition),
                stat = "sf_coordinates",
                color = "black",
                force = 2) +
  scale_colour_grey(guide = "none") +
  theme_void()

