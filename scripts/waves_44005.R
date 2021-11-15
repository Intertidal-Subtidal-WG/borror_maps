library(rnoaa)
library(dplyr)
library(purrr)
library(lubridate)
library(ggplot2)

#buoy 44005
get_buoy <- function(a_year){
  b <- buoy("stdmet", 44005, a_year)
  
  b_summary <- b$data %>%
    mutate(time = ymd_hms(time),
           year = year(time),
           month = month(time)) %>%
    filter(year == a_year) %>%
    group_by(year, month) %>%
    summarize(wave_height = mean(wave_height, na.rm=TRUE),
              time = time[1]) %>%
    ungroup()
  
  b_summary
}


b <- map_df(1980:1995, get_buoy)
b_annual <- b %>%
  group_by(year) %>%
  summarize(max_wave_height = max(wave_height),
            wave_height = mean(wave_height, na.rm=TRUE))

write_csv(b, "data/44005_wave_height_monthly.csv")

ggplot(b,
       aes(x = as.Date(time), y = wave_height)) +
  geom_line() +
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

