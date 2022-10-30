library(tidyverse)
library(trackeR)
library(FITfileR)
library(lubridate)
library(leaflet)


# run

run_summary_day_new <- read_csv("data/activity_9888642654.csv",
                                 col_types = cols(
                                   Laps = col_character(),
                                   Time = col_time(format = ""),
                                   `Cumulative Time` = col_time(format = ""),
                                   Distance = col_double(),
                                   `Avg Pace` = col_time(format = ""),
                                   `Avg HR` = col_double(),
                                   `Max HR` = col_double(),
                                   `Total Ascent` = col_double(),
                                   `Total Descent` = col_double(),
                                   `Avg Run Cadence` = col_double(),
                                   Calories = col_double(),
                                   `Avg Temperature` = col_double(),
                                   `Best Pace` = col_time(format = ""),
                                   `Max Run Cadence` = col_double(),
                                   `Moving Time` = col_time(format = ""),
                                   `Avg Moving Pace` = col_time(format = "")
                                 ))

run_fitday <- readFitFile(fileName = "data/9888642654_ACTIVITY.fit")

run_fitday_records <- records(run_fitday) %>%
  bind_rows() %>%
  arrange(timestamp) %>%
  mutate(across(starts_with("position"), ~ if_else(.x > 0, NA_real_, .x)))


run_session <- getMessagesByType(run_fitday, "session")

write_rds(run_fitday_records, file = "data/run_fitday_records.rds")
write_rds(run_summary_day_new, file = "data/run_summary_day.rds")
write_rds(run_session, file = "data/run_session.rds")
# 
# 
# library(leaflet)
# 
# 

# Create a continuous palette function
pal <- colorNumeric(
  palette = "BuPu", #"YlOrRd",
  domain = run_fitday_records$speed)

base <- run_fitday_records %>%
  mutate(color = pal(speed),
         next_lng = lead(position_long),
         next_lat = lead(position_lat)) %>% 
  filter(!is.na(next_lat))
map <-  base %>% 
  leaflet() %>%
  setView(lat = -7.27189470967143, lng = -37.38794673121044, zoom = 17) %>%
  addProviderTiles(providers$Esri.WorldImagery) 
    
    
for (i in 1:nrow(base)) {
  map <- map %>% addPolylines(lng = as.numeric(base[i, c('position_long', 'next_lng')]), 
                              lat = as.numeric(base[i, c('position_lat', 'next_lat')]), 
                               color = as.character(base[i, c('color')])
  )
}

map
