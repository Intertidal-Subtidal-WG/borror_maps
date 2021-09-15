# JL script to get percent covers

library(tidyverse)
library(here)


data <- read.csv(here("data","Maps_data",'bmaps_data_total.csv')) %>% janitor::clean_names()


data14 %>%distinct(species)
  
data_separated <- 
  data %>%
    #filter(year == 1984) %>% 
    #distinct(species) %>%
    
    # add a number per polygon
    group_by(year) %>% mutate(polygon = c(1:n())) %>% 
    
    # change all "and"s to commas
    mutate_at(.vars = "species", .funs = str_replace, pattern =" and", replacement = ",") %>%
    
    # count number of commas
    mutate(n_species =  str_count(species, ',')+1)  %>%
    # ok, the most commas in any row is 3, which means the highest mix is 4 species
  
    # separate by commas into 4 different columns
    separate(col = species, sep = ", ", into = c("species1","species2","species3", "species4"), fill="right") %>%
    
    # pivot 4 species columns into 1
    pivot_longer(cols = c("species1","species2","species3","species4"), names_to = "spp_number", values_to = "species") %>%
    drop_na(species) %>%
    mutate_at(.vars = "species", .funs = str_replace, pattern =" mix", replacement = "") %>%

    # add column for percent cover (if it's a 2-species mix, then percent cover = 50%)
    mutate(percent_cover = 1/n_species) %>%
    
    # multiply area by percent cover
    mutate(area_by_percent_cover = area_m2 * percent_cover) %>%
    
    group_by(year, species) %>%
    summarize(total_area = sum(area_by_percent_cover)) 
    
    
    

data_separated %>%
ggplot(aes(x=year,y=total_area, fill=species)) +
    geom_col(show.legend = F) +
    facet_wrap(~species) +
    scale_x_continuous(labels = unique(data$year), breaks = unique(data$year)) +
    theme_bw()
  

  