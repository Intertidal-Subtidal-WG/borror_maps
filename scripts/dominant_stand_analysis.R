#libraries
library(tidyverse)
library(here)
library(sf)

#some themes
theme_set(ggthemes::theme_few())

#vertical split at -70.61446
#horizontal split at 42.986933 or 42.98775

#load the data - it is called df
load(here("data","shapes_to_plot","island_perimeter_segments.rds"))
df <- df %>% ungroup()
#df <- st_as_sf(df %>% ungroup(), crs=4326) #fix object type

load( here("data","shapes_to_plot","appledore.rds")) #shapefile of appledore
kelp_sp <- c( "Laminaria digitata",    
            "Saccharina",   
            "Rope Kelps",
            "Alaria esculenta",
            "Saccorhiza dermatodea")
  
# - What is on the y axis
# - Total urchin barren
barrens <- df %>%
  st_as_sf(crs=4326) %>%
  filter(species_general == "Urchin barrens") %>%
  group_by(year) %>%
  summarize() %>%
  ungroup() %>%
  mutate(dominant_cover = "Urchin barrens")

# - Un-overlapped kelp and mixed stands with reds
kelps_and_reds <- df %>%
  filter(!(species_general %in% c("Codium fragile" ))) %>%
  group_by(year, geometry) %>%
  mutate(has_urchin = ("Urchin barrens" %in% species_general),
         has_reds = ("Mixed reds" %in% species_general),
         has_codium = ("Codium fragile" %in% species_general),
         has_kelp = (sum(kelp_sp %in% species_general)>0)
  ) %>%
  ungroup() %>%
  filter(!(has_urchin | 
             has_codium)) %>%
  group_by(year, geometry) %>%
  mutate(dominant_cover = ifelse(has_reds & has_kelp, 
                                 "Mixed Kelp and Reds", 
                                 ifelse(has_kelp, "Kelp", "Mixed Reds"))) %>%
  slice(1L) %>%
  ungroup()%>%
  st_as_sf(crs=4326) %>%
  group_by(year, dominant_cover) %>%
  summarize() %>%
  ungroup()
  
# 
# # Pure Reds
# reds <- df %>%
#   group_by(year, geometry) %>%
#   mutate(has_kelp = (sum(kelp_sp %in% species_general)>0),
#          has_urchin = ("Urchin barrens" %in% species_general)) %>%
#   ungroup() %>%
#   filter(!(has_kelp | has_urchin)) %>%
#   filter(species_general == "Mixed reds") %>%
#   st_as_sf(crs=4326) %>%
#   group_by(year) %>%
#   summarize() %>%
#   ungroup() %>%
#   mutate(dominant_cover = "Mixed reds")

#join!
df_reduced <- bind_rows(barrens, kelps_and_reds) %>%
  mutate(length = st_length(geometry))

red_barren_kelp <- ggplot(df_reduced, aes(color = dominant_cover)) +
  geom_sf(size = 2) +
  facet_wrap(.~year) +
  theme_void(base_size = 16) +
  scale_color_manual(values = c("darkgreen", "darkmagenta", "red", "grey")) +
  labs(color = "") +
  theme(legend.position="bottom") 


ggsave(red_barren_kelp,
       file = here("figures","red_barren_kelp_map.png"),
       dpi=300)


save(df_reduced,
     file = here("data","shapes_to_plot","island_perimeter_red_barren_kelp.rds"))

# check for colorblindness
# clauswilke/colorblindr from github
library(colorblindr)
cvd_grid(red_barren_kelp)



ggsave(red_barren_kelp,
       file = here("figures","red_barren_kelp_map_colorblind.png"),
       dpi=300)
