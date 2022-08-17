library(tidyverse)
library(trackeR)
library(FITfileR)
library(lubridate)

treadmill_session
treadmill_summary_day %>% 
  filter(Laps != 'Summary') %>% 
  mutate(Laps = as.integer(Laps)) %>% 
  ggplot(aes(x = Laps, y = `Avg HR`, color = factor(as.Date(timestamp)))) +
  geom_point() + geom_line() + 
  labs(color = 'Date') +
  theme_bw()

# Dia 16: Program 1 3x


treadmill_fitday_records %>% 
  mutate(Date = as.Date(timestamp)) %>% 
  group_by(Date) %>% 
  mutate(Time = as.numeric(timestamp - min(timestamp, na.rm = TRUE))) %>% 
  ggplot(aes(x = Time, y = heart_rate, color = factor(Date))) +
  geom_point(alpha = .25) + 
  geom_smooth(method = 'loess', formula = 'y ~ x', span = .5, se = FALSE) +
  theme_bw() + labs(y = 'Heart Rate', color = 'Date')


treadmill_fitday_records %>% 
  mutate(Date = as.Date(timestamp)) %>% 
  filter(Date >= "2022-08-16") %>% 
  group_by(Date) %>% 
  mutate(Time = as.numeric(timestamp - min(timestamp, na.rm = TRUE))) %>% 
  ggplot(aes(x = Time, y = heart_rate, color = factor(Date))) +
  geom_point(alpha = .25) + 
  geom_smooth(method = 'loess', formula = 'y ~ x', span = .25, se = FALSE) +
  theme_bw() + labs(y = 'Heart Rate', color = 'Date')



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
