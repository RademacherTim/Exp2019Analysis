#========================================================================================
# this script tests for differences between treatments and sampling heights in 
# phloem temperature for the 2019 phloem chilling experiment on red maple  at Harvard 
# Forest. 
#----------------------------------------------------------------------------------------

# load dependencies
#----------------------------------------------------------------------------------------
library ('lme4')
library ('tidyverse')
library ('lubridate')
source ('plotingFunctions.R')

# read processed respiration data
#----------------------------------------------------------------------------------------
source ('./readTemperatureData.R')

# average temperature data over one hour intervals
#----------------------------------------------------------------------------------------
hourlyData <- tempData %>% 
  group_by (datetime = cut (datetime, breaks = '1 hour')) %>% 
  summarise (t.acer.01.1p5m = mean (t.acer.01.1p5m , na.rm = TRUE),
             t.acer.02.1p5m = mean (t.acer.02.1p5m , na.rm = TRUE),
             t.acer.03.1p5m = mean (t.acer.03.1p5m , na.rm = TRUE),
             t.acer.04.1p5m = mean (t.acer.04.1p5m , na.rm = TRUE),
             t.acer.05.1p5m = mean (t.acer.05.1p5m , na.rm = TRUE),
             t.acer.06.1p5m = mean (t.acer.06.1p5m , na.rm = TRUE),
             t.acer.07.1p5m = mean (t.acer.07.1p5m , na.rm = TRUE),
             t.acer.08.1p5m = mean (t.acer.08.1p5m , na.rm = TRUE),
             t.oak.1p5m     = mean (t.oak.1p5m,      na.rm = TRUE),
             t.acer.02.2p0m = mean (t.acer.02.2p0m , na.rm = TRUE),
             t.acer.02.1p0m = mean (t.acer.02.1p0m , na.rm = TRUE),
             t.acer.04.2p0m = mean (t.acer.04.2p0m , na.rm = TRUE),
             t.acer.04.1p0m = mean (t.acer.04.1p0m , na.rm = TRUE),
             t.acer.06.2p0m = mean (t.acer.06.2p0m , na.rm = TRUE),
             t.acer.06.1p0m = mean (t.acer.06.1p0m , na.rm = TRUE),
             t.acer.07.2p0m = mean (t.acer.07.2p0m , na.rm = TRUE),
             t.acer.07.1p0m = mean (t.acer.07.1p0m , na.rm = TRUE),
             t.acer.01.2p0m = mean (t.acer.01.2p0m , na.rm = TRUE),
             t.acer.01.1p0m = mean (t.acer.01.1p0m , na.rm = TRUE),
             t.acer.03.2p0m = mean (t.acer.03.2p0m , na.rm = TRUE),
             t.acer.03.1p0m = mean (t.acer.03.1p0m , na.rm = TRUE),
             t.acer.05.2p0m = mean (t.acer.05.2p0m , na.rm = TRUE),
             t.acer.05.1p0m = mean (t.acer.05.1p0m , na.rm = TRUE),
             t.acer.08.2p0m = mean (t.acer.08.2p0m , na.rm = TRUE),
             t.acer.08.1p0m = mean (t.acer.08.1p0m , na.rm = TRUE),
             t.air.1p5m     = mean (t.air.1p5m, na.rm = TRUE))

# convert datetime back from factor to datetime
#----------------------------------------------------------------------------------------
hourlyData [['datetime']] <- as_datetime (hourlyData [['datetime']])

# average daily values 
#----------------------------------------------------------------------------------------
dailyAverage <- hourlyData %>% 
  group_by (datetime = cut (datetime, breaks = '1 day')) %>% 
  summarise (t.01.1p5m  = mean (t.acer.01.1p5m , na.rm = TRUE),
             t.02.1p5m  = mean (t.acer.02.1p5m , na.rm = TRUE),
             t.03.1p5m  = mean (t.acer.03.1p5m , na.rm = TRUE),
             t.04.1p5m  = mean (t.acer.04.1p5m , na.rm = TRUE),
             t.05.1p5m  = mean (t.acer.05.1p5m , na.rm = TRUE),
             t.06.1p5m  = mean (t.acer.06.1p5m , na.rm = TRUE),
             t.07.1p5m  = mean (t.acer.07.1p5m , na.rm = TRUE),
             t.08.1p5m  = mean (t.acer.08.1p5m , na.rm = TRUE),
             t.oak.1p5m = mean (t.oak.1p5m, na.rm = TRUE),
             t.air.1p5m = mean (t.air.1p5m, na.rm = TRUE),
             t.01.2p0m  = mean (t.acer.01.2p0m, na.rm = TRUE),
             t.01.1p0m  = mean (t.acer.01.1p0m, na.rm = TRUE),
             t.02.2p0m  = mean (t.acer.02.2p0m, na.rm = TRUE),
             t.02.1p0m  = mean (t.acer.02.1p0m, na.rm = TRUE),
             t.03.2p0m  = mean (t.acer.03.2p0m, na.rm = TRUE),
             t.03.1p0m  = mean (t.acer.03.1p0m, na.rm = TRUE),
             t.04.2p0m  = mean (t.acer.04.2p0m, na.rm = TRUE),
             t.04.1p0m  = mean (t.acer.04.1p0m, na.rm = TRUE),
             t.05.2p0m  = mean (t.acer.05.2p0m, na.rm = TRUE),
             t.05.1p0m  = mean (t.acer.05.1p0m, na.rm = TRUE),
             t.06.2p0m  = mean (t.acer.06.2p0m, na.rm = TRUE),
             t.06.1p0m  = mean (t.acer.06.1p0m, na.rm = TRUE),
             t.07.2p0m  = mean (t.acer.07.2p0m, na.rm = TRUE),
             t.07.1p0m  = mean (t.acer.07.1p0m, na.rm = TRUE),
             t.08.2p0m  = mean (t.acer.08.2p0m, na.rm = TRUE),
             t.08.1p0m  = mean (t.acer.08.1p0m, na.rm = TRUE))

# convert datetime back to datetime
#----------------------------------------------------------------------------------------
dailyAverage <- dailyAverage %>% mutate (datetime = as_datetime (datetime))

# get start and end dates for each treatment
#----------------------------------------------------------------------------------------
startDate <- as_datetime ('2019-05-29 12:00:00') 
endDate   <- as_datetime ('2019-07-10 12:00:00')

# wrangle data into long format
#----------------------------------------------------------------------------------------
dailyAverage <- dailyAverage %>% 
  pivot_longer (cols = !datetime, names_to =  c ('tree','height'), 
                names_prefix = 't.', 
                names_pattern = '(.*)\\.(.*)', 
                values_to = 'temp') 

# add column based on period (e.g., before, during, and after treatment)
#----------------------------------------------------------------------------------------
period <- ifelse (dailyAverage [['datetime']] < startDate, 'before', 
                  ifelse (dailyAverage [['datetime']] > endDate, 'after','during'))
periodAlt <- ifelse (dailyAverage [['datetime']] < startDate | 
                       dailyAverage [['datetime']] > endDate,'non-chilling','chilling')
dailyAverage <- dailyAverage %>% mutate (period, periodAlt)

# add a treatment group
#----------------------------------------------------------------------------------------
treatment <-  ifelse (dailyAverage [['tree']] %in% c ('02','04','06','07'), 'chilled', 
                      ifelse (dailyAverage [['tree']] %in% c ('01','03','05','08'), 'control',
                              'air'))
dailyAverage <- mutate (dailyAverage, treatment)

# drop rows with no temperature reading
#----------------------------------------------------------------------------------------
dailyAverage <- dailyAverage %>% filter (!is.nan (temp), !is.na (temp))

# wrangle fixed effects into reasonably-leveled factors
#----------------------------------------------------------------------------------------
dailyAverage <- dailyAverage %>% 
  mutate (period    = factor (period,    levels = c ('before','after','during')),
          periodAlt = factor (periodAlt, levels = c ('chilling','non-chilling')),
          height    = factor (height,    levels = c ('2p0m','1p0m','1p5m')),
          treatment = factor (treatment, levels = c ('chilled','air','control')),
          datetime  = factor (datetime),
          tree      = factor (tree))

# compare temperature during the non-chilling period
#----------------------------------------------------------------------------------------
M1 <- lmer (formula = temp ~ (tree|height) + datetime + period:height:treatment,
            data = dailyAverage,
            REML = TRUE)

# calculate time at desired temperatures (below 5.0 degree Celsius) for the phloem at 1.0 
# and 2.0 m in chilled trees throughout chilling
#----------------------------------------------------------------------------------------
temp <- tempData %>% filter (datetime > startDate, datetime < endDate) %>% 
  select (datetime, t.acer.02.2p0m, t.acer.02.1p0m, t.acer.04.2p0m, t.acer.04.1p0m, 
          t.acer.06.2p0m, t.acer.06.1p0m, t.acer.07.2p0m, t.acer.07.1p0m) %>% 
  pivot_longer (cols = !datetime, 
                names_to =  c ('tree','height'), 
                names_prefix = 't.', 
                names_pattern = '(.*)\\.(.*)', 
                values_to = 'temp') %>%
  filter (!is.na (temp), !is.nan (temp)) %>%
  group_by (temp < 5.0) 
sum (temp [['temp < 5']] [temp [['height']] == '1p0m'], na.rm = TRUE) / 
  length (temp [['temp < 5']] [temp [['height']] == '1p0m']) * 100.0
sum (temp [['temp < 5']] [temp [['height']] == '2p0m'], na.rm = TRUE) / 
  length (temp [['temp < 5']] [temp [['height']] == '2p0m']) * 100.0

# calculate mean temperature during chilling
#----------------------------------------------------------------------------------------
temp <- tempData %>% filter (datetime > startDate, datetime < endDate) %>% 
  select (datetime, t.01.2p0m, t.01.1p0m, t.02.2p0m, t.02.1p0m, t.03.2p0m, t.03.1p0m, 
          t.04.2p0m, t.04.1p0m, t.05.2p0m, t.05.1p0m) %>% 
  pivot_longer (cols = !datetime, 
                names_to =  c ('tree','height'), 
                names_prefix = 't.', 
                names_pattern = '(.*)\\.(.*)', 
                values_to = 'temp') %>%
  filter (!is.na (temp), !is.nan (temp)) 
mean (temp [['temp']] [temp [['height']] == '1p0m'], na.rm = TRUE)
sd   (temp [['temp']] [temp [['height']] == '1p0m'], na.rm = TRUE)
se   (temp [['temp']] [temp [['height']] == '1p0m'])
mean (temp [['temp']] [temp [['height']] == '2p0m'], na.rm = TRUE)
sd   (temp [['temp']] [temp [['height']] == '2p0m'], na.rm = TRUE)
se   (temp [['temp']] [temp [['height']] == '2p0m'])

#========================================================================================