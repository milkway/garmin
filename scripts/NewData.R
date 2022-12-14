library(tidyverse)
library(trackeR)
library(FITfileR)
library(lubridate)

treadmill_summary_day <- read_rds("data/treadmill_summary_day.rds") %>% distinct()
treadmill_fitday_records <- read_rds("data/treadmill_fitday_records.rds") %>% distinct()
treadmill_session <- read_rds("data/treadmill_session.rds") %>% distinct()

# Data from: https://connect.garmin.com/modern/
# Read fit files: remotes::install_github("grimbough/FITfileR")
# 9773017029 9850647264_ACTIVITY
treadmill_summary_day_new <- read_csv("data/activity_9874600749.csv",
                 col_types = cols(Laps = col_character(), 
                                  Time = col_character(), `Cumulative Time` = col_character(), 
                                  `Avg Pace` = col_character(), `Total Ascent` = col_skip(), 
                                  `Total Descent` = col_skip(), `Avg Power` = col_skip(), 
                                  `Avg W/kg` = col_skip(), `Max Power` = col_skip(), 
                                  `Max W/kg` = col_skip(), `Avg Ground Contact Time` = col_skip(), 
                                  `Avg GCT Balance` = col_skip(), `Avg Vertical Oscillation` = col_skip(), 
                                  `Avg Vertical Ratio` = col_skip(), 
                                  `Best Pace` = col_character(), `Max Run Cadence` = col_double(), 
                                  `Moving Time` = col_character(), 
                                  `Avg Moving Pace` = col_character()))



treadmill_fitday <- readFitFile(fileName = "data/9874600749_ACTIVITY.fit")


treadmill_fitday_records_new <- records(treadmill_fitday) %>% 
  bind_rows() %>% 
  arrange(timestamp)
treadmill_fitday_records <- bind_rows(treadmill_fitday_records, treadmill_fitday_records_new) %>% distinct()
write_rds(treadmill_fitday_records, file = "data/treadmill_fitday_records.rds")

treadmill_session_new <- getMessagesByType(treadmill_fitday, "session")
treadmill_session <- bind_rows(treadmill_session, treadmill_session_new) %>% distinct()
write_rds(treadmill_session, file = "data/treadmill_session.rds")


treadmill_summary_day_new$timestamp = treadmill_session_new$timestamp
treadmill_summary_day <- bind_rows(treadmill_summary_day, treadmill_summary_day_new)
write_rds(treadmill_summary_day, file = "data/treadmill_summary_day.rds")

write_rds(treadmill_fitday, file = paste0("data/treadmill_fitday", str_remove_all(as.Date(treadmill_session_new$timestamp), "-"),".rds"))


rm(list = grep("_new", ls(), value = TRUE))


change_exercises_dates <- tibble(Date = as.Date(c("2022-08-16", "2022-08-23", "2022-08-26", "2022-09-01", "2022-09-20", "2022-10-03"))) %>% 
  mutate(Order = row_number())
labels_exercises <- tibble(label = c("Exerc??cio 1", "Exerc??cio 2", "", "Exerc??cio 3", "Exerc??cio 4", "Exerc??cio 5", "Livre", "Trek"))
write_rds(change_exercises_dates, "data/change_exercises_dates.rds")
write_rds(labels_exercises, "labels_exercises.rds")

# ColAttr <- function(x, attrC, ifIsNull) {
#   # Returns column attribute named in attrC, if present, else isNullC.
#   atr <- attr(x, attrC, exact = TRUE)
#   atr <- if (is.null(atr)) {ifIsNull} else {atr}
#   atr
# }

## Trek
# 
# trek_summary_day_new <- read_csv("data/activity_9773017029.csv",
#                                  col_types = cols(
#                                    Laps = col_character(),
#                                    Time = col_time(format = ""),
#                                    `Cumulative Time` = col_time(format = ""),
#                                    Distance = col_double(),
#                                    `Avg Pace` = col_time(format = ""),
#                                    `Avg HR` = col_double(),
#                                    `Max HR` = col_double(),
#                                    `Total Ascent` = col_double(),
#                                    `Total Descent` = col_double(),
#                                    `Avg Run Cadence` = col_double(),
#                                    Calories = col_double(),
#                                    `Avg Temperature` = col_double(),
#                                    `Best Pace` = col_time(format = ""),
#                                    `Max Run Cadence` = col_double(),
#                                    `Moving Time` = col_time(format = ""),
#                                    `Avg Moving Pace` = col_time(format = "")
#                                  ))
# 
# trek_fitday <- readFitFile(fileName = "data/9773017029_ACTIVITY.fit")
# 
# trek_fitday_records <- records(trek_fitday) %>%
#   bind_rows() %>%
#   arrange(timestamp) %>%
#   mutate(across(starts_with("position"), ~ if_else(.x > 0, NA_real_, .x)))
# 
# 
# trek_session <- getMessagesByType(trek_fitday, "session")
# 
# write_rds(trek_fitday_records, file = "data/trek_fitday_records.rds")
# write_rds(trek_summary_day_new, file = "data/trek_summary_day.rds")
# write_rds(trek_session, file = "data/trek_session.rds")
# 
# 
# library(leaflet)
# 
# 
# trek_fitday_records %>%
#   leaflet() %>%
#   #setView(lat =-7.270892769510834, lng = -37.388718303202566, zoom = 12) %>%
#   #addTiles() %>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   #addProviderTiles(providers$Esri.WorldImagery) %>%
#   addPolylines(lng = ~position_long, lat = ~position_lat, color = "red")
# 

# tibble_units <- treadmill_session_new %>% 
#   map(.f =  ColAttr, attrC="units", ifIsNull = "") %>% 
#   as_tibble() %>% 
#     pivot_longer(cols = everything(), names_to = "uariable", values_to = "unit")

#write_rds(tibble_units, file = "data/tibble_units.rds")

# treadmill_fitday_records_2 %>% ggplot() +
#   geom_point(aes(x = timestamp, y = distance))
# 
# ggplot() +
#   geom_point(aes(x = time, y = speed), na.rm = TRUE, color = 'red', data = treadmill_fitday_records_1  %>% mutate(time = as.numeric(timestamp - min(timestamp)))) + 
#   geom_point(aes(x = time, y = speed), na.rm = TRUE, color = 'blue', data = treadmill_fitday_records_2 %>% mutate(time = as.numeric(timestamp - min(timestamp))))
# 
# 
# treadmill_fitday_records_1 %>% ggplot(aes(x = timestamp, y = heart_rate)) +
#   geom_point(na.rm = TRUE)
# treadmill_fitday_records_2 %>% ggplot(aes(x = timestamp, y = heart_rate)) +
#   geom_point(na.rm = TRUE)
# 
# 
# treadmill_fitday_records_1 %>% ggplot(aes(x = timestamp, y = temperature)) +
#   geom_point(na.rm = TRUE)
# treadmill_fitday_records_2 %>% ggplot(aes(x = timestamp, y = temperature)) +
#   geom_point(na.rm = TRUE)
# 
# treadmill_fitday_recordes %>% ggplot(aes(x = timestamp, y = cadence)) +
#   geom_point(na.rm = TRUE)
# 
# treadmill_fitday_records_1 %>% mutate(time = timestamp - min(timestamp),
#                                      target_speed = if_else(time < 5*60, 5/3.6, 
#                                                             if_else(time > max(time) - 5*60, 5/3.6, 6/3.6))) %>% 
#   ggplot() + geom_point(aes(x = as.numeric(time), y = speed), na.rm = TRUE) +
#   geom_line(aes(x = as.numeric(time), y = target_speed), color = "red", na.rm = TRUE) 
# 
# treadmill_fitday_records_2 %>% mutate(time = timestamp - min(timestamp),
#                                       target_speed = if_else(time < 5*60, 5/3.6, 
#                                                              if_else(time > max(time) - 5*60, 5/3.6, 6/3.6))) %>% 
#   ggplot() + geom_point(aes(x = as.numeric(time), y = speed), na.rm = TRUE) +
#   geom_line(aes(x = as.numeric(time), y = target_speed), color = "red", na.rm = TRUE) 
# 
# 
# 
# treadmill_fitday_records_1 %>% mutate(time = timestamp - min(timestamp),
#                                      phase = if_else(time < 5*60, 'Phase 1', 
#                                                             if_else(time > max(time) - 5*60, 'Phase 3', 'Phase 2'))) %>% 
#   ggplot() + geom_point(aes(x = as.numeric(time), y = speed, color = phase), na.rm = TRUE) 
# 
# treadmill_fitday_records_2 %>% mutate(time = timestamp - min(timestamp),
#                                       phase = if_else(time < 5*60, 'Phase 1', 
#                                                       if_else(time > max(time) - 5*60, 'Phase 3', 'Phase 2'))) %>% 
#   ggplot() + geom_point(aes(x = as.numeric(time), y = speed, color = phase), na.rm = TRUE) 
# 
# 
# treadmill_fitday_recordes %>% mutate(time = timestamp - min(timestamp),
#                                      phase = if_else(time < 5*60, 'Phase 1', 
#                                                      if_else(time > max(time) - 5*60, 'Phase 3', 'Phase 2'))) %>% 
#   ggplot() + geom_point(aes(x = as.numeric(time), y = heart_rate, color = phase), na.rm = TRUE) 
