# JL script to ST crop andrea's line

install.packages("lwgeom")
library(lwgeom)
library(tidyverse)
library(here)
library(sf)

# load line at 1.5
line15 <- read_sf(here("data","shapes_to_plot","appledore_outline_1.5.shp")) %>% janitor::clean_names()

# load .rds objects of polygons and appledore map
load( here("data","shapes_to_plot","spp_shapes_cleaned.rds"))
load( here("data","shapes_to_plot","appledore.rds"))

# set crs values 
shapes_cleaned <- st_as_sf(shapes_cleaned, crs =4326 )
line15 <- st_transform(line15, crs=4326)


# test to make sure we can find intersections
test <- st_intersection(line15, shapes_cleaned$geometry[1])
test2 <- st_intersection(line15, shapes_cleaned$geometry[2])


# plot some of those test intersections
theme_set(ggthemes::theme_map())
ggplot()+ 
  geom_sf(data = appledore)+
  geom_sf(data = shapes_cleaned, alpha=.5, size=.2) +
  geom_sf(data = line15, color ="blue")+
  geom_sf(data = test, color="red")+
  geom_sf(data = test2, color="purple")

  


# get individual intersections and lengths in for loop --------------------

# make empty tibble
df <- tibble(species_general=rep(NA,times = nrow(shapes_cleaned)),
             year = rep(NA,times = nrow(shapes_cleaned)),
             geometry = rep(NA,times = nrow(shapes_cleaned)),
             length = rep(NA,times = nrow(shapes_cleaned)))

# loop to get geometries
for (i in 1:nrow(shapes_cleaned)){
  # make species in row i same as species in row i of shapes_clean
  df$species_general[i] = shapes_cleaned$species_general[i]
  
  # make year i same
  df$year[i] = shapes_cleaned$year[i]
  
  # if an intersection between polygon and line exists, take that intersection. if not, use NA
  df$geometry[i] =
    ifelse(length(st_intersection(shapes_cleaned_2$geometry[i], line15)) == 0,
           NA,
           st_intersection(shapes_cleaned_2$geometry[i], line15))
  
  # if an intersection between polygon and line exists, take that length if not, use NA
  df$length[i] = 
    ifelse(length(st_intersection(shapes_cleaned_2$geometry[i], line15)) == 0,
           NA,
           st_length(  st_intersection(shapes_cleaned_2$geometry[i], line15)) )
  # clock
  print(i)
}

# not working: 219, 220, 265, 366, 367, 373, 374, 377, 378, 
# 382, 383, 384, 390, 391, 392, 393, 394, 395, 396, 397, 408, 417, 
df %>%
  filter(is.na(length)) %>% group_by(year) %>% tally()



# Jarrett's solution: lwgeom_make_valid
shapes_cleaned_2 <- shapes_cleaned %>% 
  rowwise() %>% 
  summarize(geometry = lwgeom_make_valid(geometry))
