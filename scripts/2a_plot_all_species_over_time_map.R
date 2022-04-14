#' --------------------------------------------------
#' Plots of cover of habitats over time on a map of
#' Appledore for visual exploration
#' --------------------------------------------------
# libraries
library(tidyverse)
library(here)
library(sf)


# load .rds objects of lines and appledore map
load( here("data","shapes_to_plot","appledore.rds"))

load(here("data","shapes_to_plot","island_perimeter_segments.rds"))
df <- st_as_sf(df %>% ungroup(), crs=4326) 


# plot all species in all years


# plot all species in all years
appledore %>%
  ggplot() +
  geom_sf(alpha=.4, size=.15) +
  geom_sf(data = df %>% st_buffer(10),
          #aes(fill=species_general, color = species_general),
          size=.05,fill = "black") +
  ggthemes::theme_map() +
  facet_grid(vars(species_general), vars(year), 
             labeller = label_wrap_gen(width = 10, multi_line = TRUE)) +
  theme(strip.text = element_text(size=13),
        strip.text.y = element_text(size=11),
        panel.border = element_rect(size=.2, fill="transparent"),
        strip.background = element_rect(fill = "transparent", 
                                        colour = "transparent"))

# Figure for the supplement
ggsave("figures/all_sp_all_years_faceted.jpg", dpi=900,
       heigh = 8, width = 6, units = "in",
       bg = "white")






