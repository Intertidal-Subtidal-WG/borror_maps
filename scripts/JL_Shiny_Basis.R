# script to load and plot shapes - basis for shiny

# libraries
library(tidyverse)
library(here)


# load .rds objects of polygons and appledore map
load( here("data","shapes_to_plot","spp_shapes_cleaned.rds"))
load( here("data","shapes_to_plot","appledore.rds"))


# plot all species in all years
appledore %>%
  ggplot() +
  geom_sf(alpha=.4, size=.15) +
  geom_sf(data = shapes_cleaned,
          aes(alpha=I(percent_cover), geometry = geometry, fill=species_general, color = species_general),
          size=.05,
          show.legend = F) +
  ggthemes::theme_map() +
  facet_grid(species_general~year) +
  theme(strip.text = element_text(size=13),
        strip.text.y = element_text(size=11),
        panel.border = element_rect(size=.2, fill="transparent"))



# set variables to plot one at a time (similar to how shiny will work)

target_year <- unique(shapes_cleaned$year)
target_species <- unique(shapes_cleaned$species_general)[1:2]


appledore %>%
  ggplot() +
  geom_sf(alpha=.4, size=.15) +
  geom_sf(data = shapes_cleaned %>%
            filter(species_general %in% target_species &
                     year %in% target_year),
          aes(alpha=I(percent_cover), geometry = geometry, fill=species_general, color = species_general),
          size=.05,
          show.legend = F) +
  ggthemes::theme_map() +
  facet_grid(species_general~year) +
  theme(strip.text = element_text(size=13),
        strip.text.y = element_text(size=11),
        panel.border = element_rect(size=.2, fill="transparent"))












