library(tidyverse)
library(trackeR)
library(FITfileR)
library(lubridate)

treadmill_session
treadmill_summary_day %>% 
  filter(Laps != 'Summary') %>% 
  filter(as.Date(timestamp) <= "2022-08-15") %>% 
  mutate(Laps = as.integer(Laps)) %>% 
  ggplot(aes(x = Laps, y = `Avg HR`, color = factor(as.Date(timestamp)))) +
  geom_point() + geom_line() + 
  labs(color = 'Date') +
  theme_bw()

treadmill_summary_day %>% 
  filter(Laps != 'Summary') %>% 
  filter(as.Date(timestamp) > "2022-08-15") %>% 
  mutate(Laps = as.integer(Laps)) %>% 
  ggplot(aes(x = Laps, y = `Avg HR`, color = factor(as.Date(timestamp)))) +
  geom_point() + geom_line() + 
  labs(color = 'Date') +
  theme_bw()

# Dia 16: Program 1 3x


treadmill_fitday_records %>% 
  mutate(Date = as.Date(timestamp)) %>% 
  filter(Date < "2022-08-16") %>% 
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

library(tidyverse)

treadmill_session <- read_rds("data/treadmill_session.rds")

str(treadmill_session)




treadmill_session %>% 
  mutate(Date = as.Date(timestamp)) %>% 
  ggplot(aes(x = Date, y = total_calories)) +
  geom_point() +
  geom_line() + 
  geom_ribbon(aes(ymin = Lim_Inf, 
                  ymax = Lim_Sup, 
                  fill = Label,
                  x = seq(as.Date("2022-08-08"), as.Date("2022-08-24"), "days")), 
              data = reference_table,
              inherit.aes = FALSE) +
  theme_bw()



range_date <- treadmill_session %>% 
  mutate(Date = as.Date(timestamp)) %>% 
  summarise(Min = min(Date), Max = max(Date))


reference_table <- tibble(
  Label = c("Overreaching", "Highly Impacting", "Impacting", "Maintaining", "Some Benefit", "No Benefit"),
  Lim_Inf = c(5, 4, 3, 2, 1, 0),
  Lim_Sup = c(5.2, 5, 4, 3, 2, 1),
  xmin = range_date$Min,
  xmax = range_date$Max,
  Color =  c(rgb(255/255, 0, 53/255), 
             rgb(255/255, 158/255, 13/255), 
             rgb(114/255, 234/255, 36/255), 
             rgb(17/255, 169/255, 237/255),
             rgb(100/255, 100/255, 100/255),
             rgb(136/255, 136/255, 136/255))
)

treadmill_session %>% 
  mutate(Date = as.Date(timestamp)) %>% 
  ggplot(aes(x = Date, y = total_training_effect)) +
  geom_rect(
    aes(xmin = xmin, 
        xmax = xmax, 
        fill = Color,
        ymin = Lim_Inf, 
        ymax = Lim_Sup),
    alpha = 0.5, 
    color = 'white',
    data = reference_table, inherit.aes = FALSE
  ) + 
  theme_bw() + 
  #theme(legend.position = 'bottom') +
  scale_fill_identity() +
  scale_color_identity() +
  geom_point() +
  geom_line() +
  geom_text(
    aes(
      x = xmin,
      y = Lim_Inf,
      label = Label,
      #colour =  Color, 
      ),
    #fill = 'white',
    colour =  "black", 
    fontface = "bold",
    vjust = 0,
    hjust = 0,
    size = 3,
    nudge_y = .025,
    nudge_x = .1,
    alpha = .9,
    data = reference_table, 
    inherit.aes = FALSE) +
  labs(x = "Date", y = "Training Effect") + 
  scale_x_date(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    panel.border = element_blank() 
    #panel.grid.major = element_blank(), 
    #panel.grid.minor = element_blank(),
    #axis.line = element_blank() 
    #axis.line = element_line(arrow = arrow())
    )
  #theme(plot.margin = margin(0,0,0,0))
  #geom_point(aes(x = Date, y = total_anaerobic_training_effect)) +
  #geom_line(aes(x = Date, y = total_anaerobic_training_effect))

  
  #scale_fill_manual(values = reference_table$Color, )



