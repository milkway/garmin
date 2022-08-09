library(tidyverse)
library(trackeR)
library(FITfileR)

# Data from: https://connect.garmin.com/modern/
# Read fit files: remotes::install_github("grimbough/FITfileR")
library(readr)
treadmill_summary_day <- read_csv("data/activity_9364716502.csv", 
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


library()

treadmill_day <- readTCX("data/activity_9364716502.tcx") %>% 
  as_tibble() %>%  select(-latitude, -longitude, -altitude, -cadence_cycling, -power) %>% view()

treadmill_fitday <- readFitFile(fileName = "data/9364716502_ACTIVITY.fit")


treadmill_fitday_recordes <- records(treadmill_fitday) %>% 
  bind_rows() %>% 
  arrange(timestamp) 

treadmill_session <- getMessagesByType(treadmill_fitday, "session")

ColAttr <- function(x, attrC, ifIsNull) {
  # Returns column attribute named in attrC, if present, else isNullC.
  atr <- attr(x, attrC, exact = TRUE)
  atr <- if (is.null(atr)) {ifIsNull} else {atr}
  atr
}

list_units <- treadmill_session %>% 
  map(.f =  ColAttr, attrC="units", ifIsNull = "") %>% 
  simplify()

tibble_units <- treadmill_session %>% 
  map(.f =  ColAttr, attrC="units", ifIsNull = "") %>% 
  as_tibble() %>% 
    pivot_longer(cols = everything(), names_to = "uariable", values_to = "unit")


treadmill_fitday_recordes %>% ggplot() +
  geom_point(aes(x = timestamp, y = distance))

treadmill_fitday_recordes %>% ggplot(aes(x = timestamp, y = speed)) +
  geom_point(na.rm = TRUE) + geom_smooth(na.rm = TRUE)

treadmill_fitday_recordes %>% ggplot(aes(x = timestamp, y = heart_rate)) +
  geom_point(na.rm = TRUE)


treadmill_fitday_recordes %>% ggplot(aes(x = timestamp, y = temperature)) +
  geom_point(na.rm = TRUE)

treadmill_fitday_recordes %>% ggplot(aes(x = timestamp, y = cadence)) +
  geom_point(na.rm = TRUE)

treadmill_fitday_recordes %>% mutate(time = timestamp - min(timestamp),
                                     target_speed = if_else(time < 5*60, 5/3.6, 
                                                            if_else(time > max(time) - 5*60, 5/3.6, 6/3.6))) %>% 
  ggplot() + geom_point(aes(x = as.numeric(time), y = speed), na.rm = TRUE) +
  geom_line(aes(x = as.numeric(time), y = target_speed), color = "red", na.rm = TRUE) 



treadmill_fitday_recordes %>% mutate(time = timestamp - min(timestamp),
                                     phase = if_else(time < 5*60, 'Phase 1', 
                                                            if_else(time > max(time) - 5*60, 'Phase 3', 'Phase 2'))) %>% 
  ggplot() + geom_point(aes(x = as.numeric(time), y = speed, color = phase), na.rm = TRUE) 


treadmill_fitday_recordes %>% mutate(time = timestamp - min(timestamp),
                                     phase = if_else(time < 5*60, 'Phase 1', 
                                                     if_else(time > max(time) - 5*60, 'Phase 3', 'Phase 2'))) %>% 
  ggplot() + geom_point(aes(x = as.numeric(time), y = heart_rate, color = phase), na.rm = TRUE) 
