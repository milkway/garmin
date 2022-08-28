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
      x = xmax,
      y = Lim_Inf,
      label = Label,
      #colour =  Color, 
      ),
    #fill = 'white',
    colour =  "black", 
    fontface = "bold",
    vjust = 0,
    hjust = 1,
    size = 3,
    nudge_y = .025,
    nudge_x = -.05,
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
    ) +
  geom_vline(aes(xintercept = Date), color = 'red', size = 1, data = change_exercises_dates, alpha = .5)
  #theme(plot.margin = margin(0,0,0,0))
  #geom_point(aes(x = Date, y = total_anaerobic_training_effect)) +
  #geom_line(aes(x = Date, y = total_anaerobic_training_effect))

  
  #scale_fill_manual(values = reference_table$Color, )


info_session <- treadmill_fitday_records %>% 
  select(timestamp, heart_rate) %>%
  mutate(Date = as.Date(timestamp)) %>%  
  group_by(Date) %>% 
  mutate(#Duration = timestamp - lag(timestamp, order_by = timestamp),
          Duration = lead(timestamp, order_by = timestamp) - timestamp,
         `Zone 5` = if_else(heart_rate > 160, Duration, 0), 
         `Zone 4` = if_else(heart_rate <= 160 & heart_rate >= 143, Duration, 0), 
         `Zone 3` = if_else(heart_rate <= 142 & heart_rate >= 125, Duration, 0),
         `Zone 2` = if_else(heart_rate <= 124 & heart_rate >= 107, Duration, 0),
         `Zone 1` = if_else(heart_rate <= 106 & heart_rate >= 90, Duration, 0),
         `Zone 0` = if_else(heart_rate < 90, Duration, 0)) %>% 
  pivot_longer(-timestamp:-Duration, names_to = "Zone", values_to = "Detect") %>% 
  group_by(Date, Zone) %>% 
  summarise(Total = sum(Detect, na.rm = TRUE), 
            N = n(),
            min_time = min(timestamp, na.rm = TRUE), 
            max_time = max(timestamp, na.rm = TRUE),
            Duration = max_time - min_time, .groups = 'drop') %>% 
  mutate(Fraction = as.numeric(Total, units = "mins")/as.numeric(Duration, units = "mins"),
         Percent = num(Fraction*100,  label = "%")) %>% 
  select(Date, Zone, Total, Percent)


info_session %>% 
  mutate(
    Days = as.numeric(Date - min(Date, na.rm = TRUE)),
    Fade = Days/max(Days, na.rm = TRUE)) %>% 
  ggplot() +
  geom_point(aes(x = Zone, y = Percent, color = Fade), size = 3) +
  #scale_color_gradient(high = "#004529", low = "#ffffe5") +
  scale_color_gradient(low = "yellow", high = "red", na.value = NA) +
  theme_bw()
    

info_session  %>% 
  ggplot(aes(x = Date, y = Percent, color = Zone, Fill = Zone)) +
  geom_line() +
  geom_point()



info_session  %>% 
  ggplot(aes(x = Date, y = Total, color = Zone, Fill = Zone)) +
  geom_line() +
  geom_point() + 
  theme_bw()


info_session  %>% 
  ggplot(aes(x = Date, y = Percent)) +
  geom_bar(stat = 'identity', position = 'dodge', fill="forest green")  +
  facet_wrap(. ~ Zone) +
  theme_bw() + 
  theme(legend.position = "none",
        strip.text = element_text(face = "bold"),
        strip.background = element_rect(colour="black",
                                        fill="white"))
#|\
library(readr)
Weight <- read_csv("data/Weight.csv", col_types = cols(`Body Fat` = col_skip(), 
                                                       `Skeletal Muscle Mass` = col_skip(), 
                                                       `Bone Mass` = col_skip(), `Body Water` = col_skip(), 
                                                       ...9 = col_skip()))
Weight_Base <- bind_cols(
  Weight %>% filter(is.na(BMI)) %>% select(Time),
  Weight %>% filter(!is.na(BMI)) %>% select(-Time)
) %>%
  mutate(Date = as.Date(Time, format = "%b %d, %Y"),
         Weight_num = as.numeric(str_remove(Weight, '\\skg')),
         Change_num = as.numeric(str_remove(Change, '\\skg|--'))
         )
write_rds(Weight_Base, "data/Weight.rds")
library(tidyverse)

Weight_Base

library(forecast)
library(tsibble)
library(timetk)
?auto.arima

Weight_Base %>%
  filter(Date >= "2022-08-22") %>%
  select(Date, Weight_num) %>%
  as_tsibble(index = Date) %>%
  fill_gaps() %>%
  ggplot() +
  geom_line(aes(x = Date, y = Weight_num)) +
  geom_point(aes(x = Date, y = Weight_num))
  


Weight_Base %>%
  filter(Date >= "2022-08-22") %>%
  select(Date, Weight_num) %>%
  plot_time_series(Date, Weight_num, 
                   .interactive = FALSE,
                   .plotly_slider = TRUE)
  
  as_tsibble(index = Date) %>%
  auto.arima() %>%
  forecast(h=20) %>%
  autoplot()

fcst <- Weight_Base %>%
  filter(Date >= "2022-08-22") %>%
  select(Date, Weight_num) %>%
  as_tsibble(index = Date) %>%
  fill_gaps() %>%
  auto.arima() 

fcst %>%
  forecast() %>%
  summary()
