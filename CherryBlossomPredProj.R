#Installs required packages
install.packages('tidyverse')
install.packages('rnoaa')

#Accesses libraries
library(tidyverse)
library(rnoaa)

#Loads and combines the historical data (code from demo)
cherry <- read.csv("data/washingtondc.csv") %>% 
  bind_rows(read.csv("data/liestal.csv")) %>% 
  bind_rows(read.csv("data/kyoto.csv"))

#---------------Average Daylight Covariate---------------#

#Reads in the daylight csv
daylight <- read.csv("data/daylight.csv") %>%
  mutate(month = factor(month, levels = month.name))

#Quick graph showing the average monthly daylight by region over the year        
daylight %>%
  ggplot() +
  geom_point(aes(x = month, y = mon_sun)) +
  facet_grid(factor(daylight$place)) +
  labs(title = "Daylight by City (Region)", 
         x = "Month", 
         y = "Average Monthly Sunlight")

#Groups the average monthly daylight by the most likely seasons
daylight_dc <-
  daylight %>%
  subset(place == "Washington" & (season == "Winter" | season == "Spring"))

daylight_liestal <-
  daylight %>%
  subset(place == "Liestal" & (season == "Winter" | season == "Spring"))

daylight_kyoto <-
  daylight %>%
  subset(place == "Kyoto" & (season == "Winter" | season == "Spring"))

daylight_vancouver <-
  daylight %>%
  subset(place == "Vancouver" & (season == "Winter" | season == "Spring"))

#Aggregates the average monthly sunlight for the first six months of the year then divides by number of days 
avg_daylight_dc <- (sum(daylight_dc$mon_sun) / 181)

avg_daylight_liestal <- (sum(daylight_liestal$mon_sun) / 181)

avg_daylight_kyoto <- (sum(daylight_kyoto$mon_sun) / 181)

avg_daylight_vancouver <- (sum(daylight_vancouver$mon_sun) / 181)

#Adds the average daylight column to the cherry data-frame
cherry <- cherry %>%
  add_column(avg_daylight = 
               if_else(cherry$location == "washingtondc", 
                       avg_daylight_dc, 
                       0) +
               if_else(cherry$location == "liestal",
                       avg_daylight_liestal,
                       0) +
               if_else(cherry$location == "kyoto",
                       avg_daylight_kyoto,
                       0)
  )

#Creates the regression model for daylight 
ls_daylight <- lm(bloom_doy - avg_daylight ~ year, data = cherry, subset = year >= 1880)

#Provides a summary of the model created
summary(ls_daylight)

#Creates the predictions for the sites
predictions <- expand_grid(location = unique(cherry$location),
                           year = 1880:2031) %>% 
  bind_cols(predicted_doy = predict(ls_daylight, newdata = .))

#Graphs my predictions and the historical data together (code from demo)
cherry %>% 
  right_join(predictions, by = c('year', 'location')) %>%
  filter(year >= 1880) %>% 
  ggplot(aes(x = year, y = predicted_doy)) +
  geom_line(aes(color = year > 2021), size = 1) +
  geom_point(aes(y = bloom_doy)) +
  scale_color_manual(values = c('FALSE' = 'gray50', 'TRUE' = 'blue'),
                     guide = 'none') +
  facet_grid(cols = vars(str_to_title(location))) +
  labs(x = "Year", y = "Peak bloom (days since Jan 1st)")

#Extrapolation to Vancouver (partial code from demo)
ls_fit_for_van <- lm(bloom_doy - avg_daylight_vancouver ~ year, data = cherry, subset = year >= 1880)

predictions_vancouver <- tibble(location = 'vancouver',
                                year = 2022:2031) %>% 
  bind_cols(predicted_doy = round(predict(ls_fit_for_van, newdata = .)))

#Binds the two data-frames
predictions <- bind_rows(predictions, predictions_vancouver)

