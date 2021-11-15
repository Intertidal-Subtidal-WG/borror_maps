#libraries
library(tidyverse)
library(here)
library(sf)

#some themes
theme_set(ggthemes::theme_few())



#load the data - it is called df
load(here("data","shapes_to_plot","island_perimeter_segments.rds"))
df <- df %>% ungroup() %>% st_as_sf(crs = 4326)
load( here("data","shapes_to_plot","island_perimeter_red_barren_kelp.rds")) #shapefile of appledore

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


make_ns_split_df <- function(df, exposure_split = -70.6144){
  p_bbox  <- e_bbox <- st_bbox(df) 
  e_bbox[1] <- exposure_split
  p_bbox[3] <- exposure_split
  
  df_exposed <- st_crop(df, e_bbox) %>%
    mutate(exposure = "Exposed")
  df_protected <- st_crop(df, p_bbox)%>%
    mutate(exposure = "Protected")
  
  df <- bind_rows(df_exposed, df_protected)
  df
}

make_split_df <- function(df, p = polys){

 out <-  p %>%
    group_by(Name) %>%
    nest() %>%
    summarize(cropped_lines = map(data, 
                               ~st_crop(df, .x))) %>%
   unnest(cropped_lines) %>% 
   st_as_sf(crs = 4326) %>%
   #factor order by exposure
   mutate(Name = factor(Name, levels = rev(c("Southeast",
                                         "Northeast",
                                         "Northwest",
                                         "Southwest")))) %>%
   rename(quadrant = Name)
 
 out
  
}


# #make the crops
# #vertical split at -70.61446
# exposure_split <- -70.61446
# #horizontal split at 42.986933 or 42.98775
df_exposure <- make_split_df(df)

df_reduced_exposure <- make_split_df(df_reduced) %>%
  mutate(length = st_length(geometry)) %>%
  ungroup() %>%
  group_by(year, quadrant) %>%
  mutate(std_length = length/sum(length))

write_csv(df_reduced_exposure, "data/df_reduced_exposure.csv")

porp_length_by_reduced_group <- ggplot(df_reduced_exposure,
       aes(x=as.character(year), y = as.numeric(std_length), 
           group = paste(quadrant, dominant_cover),
           fill=dominant_cover, color = dominant_cover)) +
  geom_bar(stat="identity", show.legend = F) +
  facet_grid(dominant_cover ~ quadrant) +
 # geom_line(show.legend = F, aes(lty = quadrant)) +
#  geom_point(show.legend = F) +
#  facet_wrap(~dominant_cover) +
  scale_fill_manual(values = c("darkgreen", "darkmagenta", "red", "grey")) +
  scale_color_manual(values = c("darkgreen", "darkmagenta", "red", "grey")) +
  labs(x = "Year",y="Porportion of Total Length")   +
  ylim(c(0,1))


ggsave(porp_length_by_reduced_group,
       file = here("figures","porp_length_by_reduced_group.png"),
       dpi=300)

# vegetated/unvegetated
df_reduced_exposure %>%
  group_by(quadrant, year, vegetated = dominant_cover != "Urchin barrens") %>%
  summarize(std_length = sum(std_length)) %>%
  #mutate(vegetated = ifelse(vegetated, "Vegetated", "Unvegetated")) %>%
  filter(vegetated) %>%
  ggplot(aes(x=as.character(year), y = as.numeric(std_length))) +
  geom_bar(stat="identity", show.legend = F) +
  facet_wrap(  ~ quadrant, ncol=4) +
  labs(x = "Year",y="Porportion of Length Vegetated") 

