library(rnoaa)
library(dplyr)
library(purrr)
library(lubridate)
library(ggplot2)
library(readr)

#buoy 44005
get_buoy <- function(a_year){
  b <- buoy("stdmet", 44005, a_year)
  
  b_summary <- b$data %>%
    mutate(time = ymd_hms(time),
           year = year(time),
           month = month(time)) %>%
    filter(year == a_year) %>%
    mutate(ifelse(wave_height>40, NA, wave_height)) %>%
    group_by(year, month) %>%
    summarize(wave_height = mean(wave_height, na.rm=TRUE),
              time = time[1]) %>%
    ungroup()
  
  b_summary
}



#buoy 44005
get_buoy_raw <- function(a_year){
  b <- buoy("stdmet", 44005, a_year)
  
  b<- b$data %>%
    mutate(time = ymd_hms(time),
           year = year(time),
           month = month(time)) %>%
    filter(year == a_year) %>%
    select(time, year, month, wave_height) %>%
    mutate(ifelse(wave_height>40, NA, wave_height))

  b
}

b <- map_df(1980:1995, get_buoy)
b_raw <- map_df(1980:1995, get_buoy_raw)

#write data
write_csv(b, "data/44005_wave_height_monthly.csv")
write_csv(b_raw, "data/44005_wave_height_raw.csv")

b <- read_csv("data/44005_wave_height_monthly.csv")

#make annual derived data
b_annual <- b %>%
  group_by(year) %>%
  summarize(max_wave_height = max(wave_height),
            wave_height = mean(wave_height))



ggplot(b_raw,
       aes(x = as.Date(time), y = wave_height)) +
  geom_line() +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 90)) 

ggplot(b,
       aes(x = as.Date(time), y = wave_height)) +
  geom_line() +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(vars(month)) +
  geom_vline(xintercept = as.Date("1990-1-01 00:00:00 UTC"), color = "red", lty = 2) +
  geom_vline(xintercept = as.Date("1987-1-01 00:00:00 UTC"), color = "red", lty = 2)


ggplot(b %>% filter(month %in% c(1,2,3,12)),
       aes(x = as.Date(time), y = wave_height, color = as.factor(year))) +
  geom_point() +
  scale_x_date(date_breaks = "1 year",
               date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 90))



ggplot(b_annual,
       aes(x = year, y = wave_height)) +
  geom_line() + 
  scale_x_continuous(breaks=1980:1995)+
  theme(axis.text.x = element_text(angle = 90))


ggplot(b_annual,
       aes(x = year, y = max_wave_height)) +
  geom_line() + 
  scale_x_continuous(breaks=1980:1995)+
  theme(axis.text.x = element_text(angle = 90))

