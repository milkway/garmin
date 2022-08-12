library(tidyverse)
library(trackeR)
library(FITfileR)

# Data from: https://connect.garmin.com/modern/
# Read fit files: remotes::install_github("grimbough/FITfileR")
library(readr)
library(FITfileR)
treadmill_summary_day_1 <- read_csv("data/activity_9364716502.csv", 
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
treadmill_fitday_1 <- readFitFile(fileName = "data/9364716502_ACTIVITY.fit")

treadmill_summary_day_2 <- read_csv("data/activity_9371505441.csv", 
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

treadmill_fitday_2 <- readFitFile(fileName = "data/9371505441_ACTIVITY.fit")

treadmill_fitday_records_1 <- records(treadmill_fitday_1) %>% 
  bind_rows() %>% 
  arrange(timestamp)

treadmill_fitday_records_2 <- records(treadmill_fitday_2) %>% 
  bind_rows() %>% 
  arrange(timestamp)

treadmill_session_1 <- getMessagesByType(treadmill_fitday_1, "session")
treadmill_session_2 <- getMessagesByType(treadmill_fitday_2, "session")


ColAttr <- function(x, attrC, ifIsNull) {
  # Returns column attribute named in attrC, if present, else isNullC.
  atr <- attr(x, attrC, exact = TRUE)
  atr <- if (is.null(atr)) {ifIsNull} else {atr}
  atr
}

# list_units <- treadmill_session_1 %>% 
#   map(.f =  ColAttr, attrC="units", ifIsNull = "") %>% 
#   simplify()

tibble_units <- treadmill_session_1 %>% 
  map(.f =  ColAttr, attrC="units", ifIsNull = "") %>% 
  as_tibble() %>% 
    pivot_longer(cols = everything(), names_to = "uariable", values_to = "unit")

treadmill_fitday_records_2 %>% ggplot() +
  geom_point(aes(x = timestamp, y = distance))

ggplot() +
  geom_point(aes(x = time, y = speed), na.rm = TRUE, color = 'red', data = treadmill_fitday_records_1  %>% mutate(time = as.numeric(timestamp - min(timestamp)))) + 
  geom_point(aes(x = time, y = speed), na.rm = TRUE, color = 'blue', data = treadmill_fitday_records_2 %>% mutate(time = as.numeric(timestamp - min(timestamp))))


treadmill_fitday_records_1 %>% ggplot(aes(x = timestamp, y = heart_rate)) +
  geom_point(na.rm = TRUE)
treadmill_fitday_records_2 %>% ggplot(aes(x = timestamp, y = heart_rate)) +
  geom_point(na.rm = TRUE)


treadmill_fitday_records_1 %>% ggplot(aes(x = timestamp, y = temperature)) +
  geom_point(na.rm = TRUE)
treadmill_fitday_records_2 %>% ggplot(aes(x = timestamp, y = temperature)) +
  geom_point(na.rm = TRUE)

treadmill_fitday_recordes %>% ggplot(aes(x = timestamp, y = cadence)) +
  geom_point(na.rm = TRUE)

treadmill_fitday_records_1 %>% mutate(time = timestamp - min(timestamp),
                                     target_speed = if_else(time < 5*60, 5/3.6, 
                                                            if_else(time > max(time) - 5*60, 5/3.6, 6/3.6))) %>% 
  ggplot() + geom_point(aes(x = as.numeric(time), y = speed), na.rm = TRUE) +
  geom_line(aes(x = as.numeric(time), y = target_speed), color = "red", na.rm = TRUE) 

treadmill_fitday_records_2 %>% mutate(time = timestamp - min(timestamp),
                                      target_speed = if_else(time < 5*60, 5/3.6, 
                                                             if_else(time > max(time) - 5*60, 5/3.6, 6/3.6))) %>% 
  ggplot() + geom_point(aes(x = as.numeric(time), y = speed), na.rm = TRUE) +
  geom_line(aes(x = as.numeric(time), y = target_speed), color = "red", na.rm = TRUE) 



treadmill_fitday_records_1 %>% mutate(time = timestamp - min(timestamp),
                                     phase = if_else(time < 5*60, 'Phase 1', 
                                                            if_else(time > max(time) - 5*60, 'Phase 3', 'Phase 2'))) %>% 
  ggplot() + geom_point(aes(x = as.numeric(time), y = speed, color = phase), na.rm = TRUE) 

treadmill_fitday_records_2 %>% mutate(time = timestamp - min(timestamp),
                                      phase = if_else(time < 5*60, 'Phase 1', 
                                                      if_else(time > max(time) - 5*60, 'Phase 3', 'Phase 2'))) %>% 
  ggplot() + geom_point(aes(x = as.numeric(time), y = speed, color = phase), na.rm = TRUE) 


treadmill_fitday_recordes %>% mutate(time = timestamp - min(timestamp),
                                     phase = if_else(time < 5*60, 'Phase 1', 
                                                     if_else(time > max(time) - 5*60, 'Phase 3', 'Phase 2'))) %>% 
  ggplot() + geom_point(aes(x = as.numeric(time), y = heart_rate, color = phase), na.rm = TRUE) 
