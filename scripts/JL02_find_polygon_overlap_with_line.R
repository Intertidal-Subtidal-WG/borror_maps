# JL script to use the line at 1.5m depth perimeter of island to overlap
# with polygons output from "JL01_clean_polygons". This script will measure
# the island perimeter accounted for by each species, and quantify percent
# of perimeter for each species. This script will also create a plot of
# percent of perimeter in each year, and faceted percent of perimeter of 
# each species in each year. 



# libraries ---------------------------------------------------------------
library(lwgeom)
library(tidyverse)
library(here)
library(sf)
library(ggtext)


# load data ---------------------------------------------------------------

# load 1.5 depth line
line15 <- read_sf(here("data","shapes_to_plot","appledore_outline_1.5.shp")) %>% 
  janitor::clean_names() %>%
  st_transform(line15, crs=4326) %>%
  st_union() # use this because this comes as two lines for some reason

# load .rds objects of polygons and appledore map
load( here("data","shapes_to_plot","spp_shapes_cleaned.rds"))
load( here("data","shapes_to_plot","appledore.rds"))

# set crs values 
shapes_cleaned <- st_as_sf(shapes_cleaned, crs =4326 )
shapes_cleaned <- shapes_cleaned %>% 
  mutate(species_general = stringr::str_to_sentence(species_general),
         geometry = st_make_valid(geometry)) 
  


# test to make sure we can find intersections
test <- st_intersection(line15, shapes_cleaned$geometry[1])
test2 <- st_intersection(line15, shapes_cleaned$geometry[2])


# plot some of those test intersections
theme_set(ggthemes::theme_map())
ggplot()+ 
  geom_sf(data = appledore)+
  geom_sf(data = shapes_cleaned[c(1,2),], alpha=.5, size=.2,
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
  
  # find overlapping line segments (i.e. polygons that exist for multiple species)
  group_by(year, geometry) %>% add_count() %>%
  arrange(desc(n)) %>%
  # so there are some polygons shared by up to 4 species. 
  # for these, we'll divide the length of the line segment by the number of species
  # to standardize percent calculations.
  # basically this means if there's a segment that's "urchins and kelp" it will 
  # count as 50% urchins, 50% kelp
  
  # fix area to divide by overlapping species
  mutate(fixed_length = length/n) %>% 
  arrange(year)


# save out the df as .rds file
save(df,
     file = here("data","shapes_to_plot","island_perimeter_segments.rds"))
rm(list = ls())

load(here("data","shapes_to_plot","island_perimeter_segments.rds"))
line15 <- read_sf(here("data","shapes_to_plot","appledore_outline_1.5.shp")) %>% 
  janitor::clean_names() %>%
  st_transform(line15, crs=4326) %>%
  st_union() # use this because this comes as two lines for some reason

total_length <- st_length(line15) %>% sum()
# so total perimeter of island = 5566.457 meters

df_percents <- df %>%
  mutate(percent = fixed_length/total_length*100) %>%
  group_by(year, species_general) %>%
  summarize(sum = sum(percent)) %>%
  mutate(sum = as.numeric(sum))

# see full percents sums
df_percents %>%
  group_by(year) %>%
  summarize(sum = sum(sum))
     
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




# set colors manually -----------------------------------------------------
# here, i'm making a list of each "group" of species, which will be 
# colored with shades of the same color
#014701, darkgreen, #4b944b, #82c882, #af9100, #dd0000, #76ee00, #b3b3b3
pal <- c(
  # kelps
  paste(colorspace::darken("darkgreen",.3)),
  "darkgreen",
  paste(colorspace::lighten("darkgreen",.3)),
  paste(colorspace::lighten("darkgreen",.6)),
  
  # non kelp browns
  "#af9100",
  
  # reds
  "#dd0000",
  
  # green
  "#76ee00",
  
  #other
  "grey70"
)



colors <- 
  tribble(
    ~spp,                   ~color,
    # kelps:
    "<br><b>Kelps</b>",                 "transparent",
    "<i>Alaria esculenta</i>",      pal[1],
    "Rope kelps",            pal[2],
    "<i>Laminaria digitata</i>",    pal[3],
    "<i>Saccharina latissima</i>",            pal[4],
    
    # other non-kelp browns:
    "<br><b>Other browns</b>",          "transparent",
    "<i>Saccorhiza dermatodea</i>",            pal[5],
    
    # reds:
    "<br><b>Reds</b>",                  "transparent",
    "Mixed reds",           pal[6],
    
    # greens:
    "<br><b>Greens</b>",                "transparent",
    "<i>Codium fragile</i>",        pal[7],
    
    # other
    "<br><b>Other</b>",                 "transparent",
    "Urchin barrens",        pal[8],
    

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

# change species names to italic
df_percents3 <- df_percents2 %>%
  mutate(species_general = recode(species_general,
                                  # change kelps
                                  "Alaria esculenta" = "<i>Alaria esculenta</i>",
                                  "Laminaria digitata" = "<i>Laminaria digitata</i>",
                                  "Saccharina" = "<i>Saccharina latissima</i>",
                                  # other browns
                                  "Saccorhiza dermatodea" = "<i>Saccorhiza dermatodea</i>",
                                  # greens
                                  "Codium fragile" = "<i>Codium fragile</i>"))



# make plot
community_plot <- df_percents3 %>%      
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


# check colorblind --------------------------------------------------------
library(colorblindr)
cvd_grid(community_plot)


# export plot
ggsave(community_plot,
       file = here("figures","community_change_plot.png"),
       width = 6,
       height = 4,
       units = "in",
       dpi=300)



# figure 2 - faceted by species -------------------------------------------
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
