library(tidyverse)
library(lubridate)
library(broom)

raw <- tibble(files = list.files("data/", pattern = "Weight_\\d{8}.csv")) %>% 
  rowwise() %>% 
  mutate(raw = list(data = suppressMessages(suppressWarnings(read_csv(paste0("data/", files),show_col_types = FALSE))))) %>% 
  unnest(raw)


weights <- raw %>% 
  select(files, Time, Weight, BMI) %>% 
  mutate(Weight_New = if_else(is.na(Weight), lead(Weight), Weight),
         BMI_New = if_else(is.na(BMI), lead(BMI), BMI),
         Time_New = if_else(is.na(Weight), paste(Time, lead(Time)), NA_character_)
  ) %>% 
  filter(!is.na(Time_New)) %>% 
  select(files, Time = Time_New, Weight = Weight_New, BMI = BMI_New) %>% 
  mutate(date = ymd(paste(str_extract(Time, "\\d{4}"),
                          str_extract(Time, "([A-Z]{1}[a-z]{2})"), 
                          str_extract(Time, "\\s\\d{1,2}")))) %>% 
  group_by(date) %>% 
  slice_head(n = 1) %>% 
  ungroup() %>% 
  mutate(model = ymd(str_extract(files, "\\d{8}")),
         weightNum = as.numeric(str_remove(Weight, " kg"))) %>% 
  select(-Time)

base <- weights %>% 
  summarise(model = unique(model), 
            db = list(.)) %>%  
  mutate(fit = map2(db, model, ~ lm(weightNum ~ date, data = .x %>% filter(date < .y))),
         forecast = map(fit, ~ tibble(date = seq(from = last(.x$model$date) + 1, length.out = 356, by = '1 day')) %>% 
                         mutate(predict = predict(.x, newdata = .),
                                fitmodel = 'Linear Model') %>% 
                          filter(predict >= 100)),
         arima = map2(db, model, ~ .x %>% 
                        filter(date < .y) %>% 
                        select(date, weightNum) %>% 
                        as_tsibble(index = "date") %>% 
                        fill_gaps() %>% 
                        model(arima = ARIMA(weightNum))),
         forecast_arima = map(arima, ~ .x %>% 
                                forecast(h = "356 days") %>%
                                as_tibble() %>% 
                                select(date, predict = .mean) %>% 
                                mutate(fitmodel = 'ARIMA') %>% 
                                filter(predict >= 100))#,
         # tidied = map(fit, tidy),
         # glanced = map(fit, glance),
         # augmented = map(fit, augment)
         ) 

base  
  
ggplot() +
  geom_line(aes(x = date, y = predict, color =  factor(model), linetype = fitmodel),
            data = base %>% 
              unnest(forecast), size = 1) +
  geom_line(aes(x = date, y = predict, color =  factor(model), linetype = fitmodel),
            data = base %>% 
              unnest(forecast_arima), size = 1) +
  scale_color_brewer(palette = "YlGn") +
  geom_point(aes(x = date, y = weightNum),
             color = "red",
            data = weights, inherit.aes = FALSE)  +
  #geom_vline(xintercept = unique(weights$model), color = "black") +
  geom_hline(size = 1, aes(yintercept = 100), color = "purple") +
  annotate("text", 
           x = ymd("2022-8-20"), 
           y = 100,  
           label = "Target", 
           fontface = 'bold',
           vjust = -.5) +
  labs(color = 'Legend', y = 'Weight (Kg)') +
  theme_light() + 
  theme(
    legend.background = element_blank(),
    legend.direction = 'horizontal',
    legend.position = c(.5, .9),
    legend.text.align = 0,
    legend.title.align = 1,
    #plot.caption= element_text(size=9, 
    #                           color=gray(.25),
    #                           face="italic"),
    #legend.position = 'top', 
    aspect.ratio =  2/(1+sqrt(5))
  )  +
  #scale_color_brewer(palette = 'Set1') +
  scale_x_date(#limits = ymd(c("2020-03-25", "2021-01-05")),
    expand = c(.01,0,0,0),
    date_breaks = "1 month",
    minor_breaks = NULL,
    date_labels = "%b")

?pmap
model_1 <- weights %>% 
model_2 <- weights %>% lm(BMI ~ date, data = .)

summary(model_1)

model_1$coefficients

forecasts <- 

predict(model_1, newdata = tibble(date = seq(ymd("2022-10-28"), ymd("2023-05-28"), by = '1 day')))

weights %>% 
  ggplot() +
  geom_point(aes(x = date, y = weightNum, color = 'Actual'), alpha = .75) +
  geom_line(aes(x = date, y = predict, color = "Model"), size = 1, 
            data = forecasts) + 
  # geom_line(aes(x = Date, y = ARIMA, color = 'SARIMA'), size = 1, 
  #           data = previsão2) + 
  geom_hline(size = 1, aes(yintercept = 100, color = 'Target')) +
  annotate("text", 
           x = ymd("2022-8-20"), 
           y = 100,  
           label = "Target", 
           fontface = 'bold',
           vjust = -.5) +
  labs(color = 'Legend', y = 'Weight (Kg)') +
  theme_light() + 
  theme(
    legend.background = element_blank(),
    legend.direction = 'horizontal',
    legend.position = c(.5, .9),
    legend.text.align = 0,
    legend.title.align = 1,
    #plot.caption= element_text(size=9, 
    #                           color=gray(.25),
    #                           face="italic"),
    #legend.position = 'top', 
    aspect.ratio =  2/(1+sqrt(5))
  )  +
  scale_color_brewer(palette = 'Set1') +
  scale_x_date(#limits = ymd(c("2020-03-25", "2021-01-05")),
    expand = c(.01,0,0,0),
    date_breaks = "1 month",
    minor_breaks = NULL,
    date_labels = "%b")


library(fable)
library(tsibble)
library(tsibbledata)

model_arima <- weights %>% 
  select(date, weightNum) %>% 
  as_tsibble(index = "date") %>% 
  fill_gaps() %>% 
  model(arima = ARIMA(weightNum))


arima_bmi <- pesos %>% 
  select(Date, BMI) %>% 
  as_tsibble(index = "Date") %>% 
  fill_gaps() %>% 
  model(
    #ets = ETS(box_cox(Weight, 0.3)),
    arima = ARIMA(BMI)
    #snaive = SNAIVE(Weight)
  )


previsão2 <- previsão %>% 
  mutate(Model = 'Linear Regression') %>% 
  bind_rows(arima %>% 
              forecast(h = "256 days") %>%
              as_tibble() %>% 
              select(Date, Predict = .mean) %>% 
              mutate(Model = 'ARIMA') %>% 
              filter(Predict >= 100))


previsão_bmi <- tibble(
  Date = seq(ymd("2022-10-25"), ymd("2024-04-12"), by = '1 day')
) %>% 
  mutate(Predict = predict(model_2, newdata = .),
         Model = 'Linear Regression') %>% 
  bind_rows(arima_bmi %>% 
              forecast(h = "351 days") %>%
              as_tibble() %>% 
              select(Date, Predict = .mean) %>% 
              mutate(Model = 'ARIMA'))




pesos %>% 
  ggplot() +
  geom_point(aes(x = Date, y = BMI, color = 'Actual'), alpha = .75) +
  #geom_line(aes(x = Date, y = Predict, color = 'Ajuste'), size = 1) +
  geom_line(aes(x = Date, y = Predict, color = Model), size = 1, 
            data = previsão_bmi) + 
  # geom_line(aes(x = Date, y = ARIMA, color = 'SARIMA'), size = 1, 
  #           data = previsão2) + 
  geom_hline(size = 1, aes(yintercept = 25, color = 'Target')) +
  annotate("text", 
           x = ymd("2022-8-20"), 
           y = 25,  
           label = "Target", 
           fontface = 'bold',
           vjust = -.5) +
  labs(color = 'Legend', y = 'Weight (Kg)') +
  theme_light() + 
  theme(
    legend.background = element_blank(),
    legend.direction = 'horizontal',
    legend.position = c(.5, .9),
    legend.text.align = 0,
    legend.title.align = 1,
    #plot.caption= element_text(size=9, 
    #                           color=gray(.25),
    #                           face="italic"),
    #legend.position = 'top', 
    aspect.ratio =  2/(1+sqrt(5))
  )  +
  scale_color_brewer(palette = 'Set1') +
  scale_x_date(#limits = ymd(c("2020-03-25", "2021-01-05")),
    expand = c(0.05,0,0,0),
    date_breaks = "1 month",
    minor_breaks = NULL,
    date_labels = "%b")
