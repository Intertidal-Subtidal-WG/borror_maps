# JL script to ST crop andrea's line

#install.packages("lwgeom")
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
shapes_cleaned <- shapes_cleaned %>% 
  mutate(species_general = stringr::str_to_sentence(species_general))

line15 <- st_transform(line15, crs=4326)

ggplot()+ 
  geom_sf(data = appledore)+
  geom_sf(data = line15 %>% slice(2), color = "red") +
  geom_sf(data = line15 %>% slice(1), color = "blue")
# ok looks like our sf line is spit into two for some reason

line15.2 <- st_union(line15)
ggplot()+ 
  geom_sf(data = appledore)+
  geom_sf(data = line15.2, color = "red") 

line15 <- line15.2
rm(line15.2)

# test to make sure we can find intersections
test <- st_intersection(line15, shapes_cleaned$geometry[1])
test2 <- st_intersection(line15, shapes_cleaned$geometry[2])


# plot some of those test intersections
theme_set(ggthemes::theme_map())
ggplot()+ 
  geom_sf(data = appledore)+
  geom_sf(data = shapes_cleaned, alpha=.5, size=.2,
          aes(geometry = geometry)) +
  geom_sf(data = line15, color ="blue")+
  geom_sf(data = test, color="red")+
  geom_sf(data = test2, color="purple")

  


# get individual intersections and lengths in for loop --------------------

# make empty tibble
df <- tibble(species_general=rep(NA,times = nrow(shapes_cleaned)),
             year = rep(NA,times = nrow(shapes_cleaned)),
             geometry = rep(NA,times = nrow(shapes_cleaned)),
             length = rep(NA,times = nrow(shapes_cleaned)))



# Jarrett's solution: st_make_valid to fix polygons
shapes_cleaned <- shapes_cleaned %>% 
  mutate(geometry = st_make_valid(geometry))

# loop to get geometries
for (i in 1:nrow(shapes_cleaned)){
  # make species in row i same as species in row i of shapes_clean
  df$species_general[i] = shapes_cleaned$species_general[i]
  
  # make year i same
  df$year[i] = shapes_cleaned$year[i]
  
  # if an intersection between polygon and line exists, take that intersection. if not, use NA
  df$geometry[i] =
    ifelse(length(st_intersection(shapes_cleaned$geometry[i], line15)) == 0,
           NA,
           st_intersection(shapes_cleaned$geometry[i], line15))
  
  # if an intersection between polygon and line exists, take that length if not, use NA
  df$length[i] = 
    ifelse(length(st_intersection(shapes_cleaned$geometry[i], line15)) == 0,
           NA,
           st_length(  st_intersection(shapes_cleaned$geometry[i], line15)) )
  # clock
  print(i)
}

# ok, so now we have a df of each polygon and it's intersection with the island perimeter line

df <- df %>%
  # drop rows with NA length, meaning that line and polygon don't intersect
  drop_na(length) %>%
  
  # find overlapping line segments
  group_by(year, geometry) %>% add_count() %>%
  arrange(desc(n)) %>%
  
  # fix area to divide by overlapping species
  mutate(fixed_length = length/n) %>% 
  arrange(year)


# save out the df as .rds file
#save(df,
#     file = here("data","shapes_to_plot","island_perimeter_segments.rds"))
#
#load(here("data","shapes_to_plot","island_perimeter_segments.rds"))

total_length <- st_length(line15) %>% sum()
# so total perimeter of island = 5610.871 meters

df_percents <- df %>%
  mutate(percent = fixed_length/total_length*100) %>%
  group_by(year, species_general) %>%
  summarize(sum = sum(percent)) %>%
  mutate(sum = as.numeric(sum))
     
#how much is barren?    
df_percents %>%
  filter(year<1995, species_general == "Urchin barrens")

#how much is kelp?
df_percents %>%
  filter(year<1995, 
         species_general != "Urchin barrens",
         species_general != "Mixed reds") %>%
  group_by(year) %>% summarize(percent = sum(sum))

# how much is red
#how much is kelp?
df_percents %>%
  filter(year<1995, 
         species_general == "Mixed reds") 

df_percents %>%
  filter(year>1995, 
         species_general == "Rope kelps") 





theme_set(ggthemes::theme_few())



# set colors manually -----------------------------------------------------
# here, i'm making a list of each "group" of species, which will be 
# colored with shades of the same color
colors <- 
  tribble(
    ~spp,                   ~color,
    # kelps:
    "<br><b>Kelps</b>",                 "transparent",
    "Alaria esculenta",      paste(colorspace::darken("darkgreen",.3)),
    "Rope kelps",            "darkgreen",
    "Laminaria digitata",    paste(colorspace::lighten("darkgreen",.3)),
    "Saccharina",            paste(colorspace::lighten("darkgreen",.6)),
    
    # other non-kelp browns:
    "<br><b>Other browns</b>",          "transparent",
    "Saccorhiza dermatodea", "gold",
    
    # reds:
    "<br><b>Reds</b>",                  "transparent",
    "Mixed reds",            paste(colorspace::darken("red",.3)),
    
    # greens:
    "<br><b>Greens</b>",                "transparent",
    "Codium fragile",        "chartreuse2",
    
    # other
    "<br><b>Other</b>",                 "transparent",
    "Urchin barrens",        "grey70",
    

  )

# add a column of species groups
df_percents <- df_percents %>%
  ungroup() %>%
  mutate(type = case_when(species_general %in% c("Alaria esculenta",   
                                                 "Rope kelps",         
                                                 "Laminaria digitata", 
                                                 "Saccharina") ~ "<br><b>Kelps</b>",
                          species_general %in% c("Saccorhiza dermatodea") ~"<br><b>Other browns</b>",
                          species_general %in% c("Mixed reds") ~ "<br><b>Reds</b>",
                          species_general %in% c("Codium fragile") ~ "<br><b>Greens</b>",
                          species_general %in% c("Urchin barrens") ~ "<br><b>Other</b>"))

# add blank rows for group names so that they show 
# up in the legend with transparent keys
df_percents2 <- df_percents %>%
  bind_rows( df_percents %>% distinct(type) %>% mutate(species_general = type,
                                                    year = 2014,
                                                    sum = 0))

# make plot
community_plot <- df_percents2 %>%      
  #filter(year == 1982) %>%
  ungroup() %>%
  ggplot(aes(x=as.character(year),y=sum, 
             fill = factor(species_general, levels = colors$spp))) +
  geom_col() +
  ggthemes::theme_few() +
  labs(x="Year",
       y = "Percent of Island Perimeter",
       fill = "Species Group") +
  scale_fill_manual(breaks = colors$spp,
                    values = colors$color)+
  theme(legend.text = element_markdown())

# view plot
community_plot

# export plot
ggsave(community_plot,
       file = here("figures","community_change_plot.png"),
       width = 6,
       height = 4,
       units = "in",
       dpi=300)

df_percents %>% group_by(year) %>%
  summarize(total = sum(sum))


# looks like all years only have ~ 85% cover of the line... why?

ggplot() +
  geom_sf(data = appledore) +
  geom_sf(data = line15) + 
  geom_sf(data = shapes_cleaned %>% filter(year ==1982), size = 0)





# now facet by species
faceted_perimeter_change <- df %>%
  group_by(year, species_general) %>%
  summarize(length_sum = sum(fixed_length)) %>%
  ggplot(aes(x=as.character(year), y = length_sum, fill=species_general)) +
  geom_bar(stat="identity", show.legend = F) +
  facet_wrap(~species_general, scales = "free_y", nrow = 2) +
  scale_fill_manual(values = PNWColors::pnw_palette("Bay",8)) +
  labs(x = "Year",y="Total Length (m)") 


ggsave(faceted_perimeter_change,
       file = here("figures","faceted_perimeter_change.png"),
       dpi=300,
       width = 10,
       height = 5,
       unit= "in")
