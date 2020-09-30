#========================================================================================
# This script tests for differences between treatments and sampling heights in 
# stem CO2 efflux for the 2018 phloem chilling and compression experiment at Harvard 
# Forest. 
#----------------------------------------------------------------------------------------

# load dependencies
#----------------------------------------------------------------------------------------
library ('lme4')
library ('tidyverse')
library ('lubridate')

# read processed respiration data
#----------------------------------------------------------------------------------------
source ('./readProcessedRespData.R')

# drop unnecessary variable
#----------------------------------------------------------------------------------------
respDataExp2019 <- respDataExp2019 %>% 
  select (treatment, tree, chamber, datetime, flux.raw)

# make sure all variables are factors
#----------------------------------------------------------------------------------------
respDataExp2019 [['date']]      <- factor (as_date (respDataExp2019 [['datetime']]))
respDataExp2019 [['treatment']] <- factor (respDataExp2019 [['treatment']], levels = c (5, 1))
respDataExp2019 [['tree']]      <- factor (respDataExp2019 [['tree']])
respDataExp2019 [['height']]    <- factor (respDataExp2019 [['chamber']])

# get start and end dates for each treatment
#----------------------------------------------------------------------------------------
startDate <- as_datetime ('2019-05-29 12:00:00')
endDate   <- as_datetime ('2019-07-10 12:00:00')

# add column based on period (e.g., before, during, and after treatment)
#----------------------------------------------------------------------------------------
period <- ifelse (respDataExp2019 [['datetime']] < startDate, 'before', 
                  ifelse (respDataExp2019 [['datetime']] > endDate, 'after','during'))
periodAlt <- ifelse (respDataExp2019 [['datetime']] < startDate | 
                       respDataExp2019 [['datetime']] > endDate,'non-chilling','chilling')
respDataExp2019 <- respDataExp2019 %>% 
  mutate (period    = factor (period,    levels = c ('during','after','before')), 
          periodAlt = factor (periodAlt, levels = c ('chilling', 'non-chilling')))

# fit mixed effects model with tree as random effect
#----------------------------------------------------------------------------------------
M1 <- lmer (formula = flux.raw ~ (tree | height) + date + period:treatment:height, 
            data = respDataExp2019,
            REML = TRUE)
summary (M1)

# fit mixed effects model with tree as random effect
#----------------------------------------------------------------------------------------
M2 <- lmer (formula = flux.raw ~ (tree | height) + period + period:treatment:height, 
            data = respDataExp2019,
            REML = TRUE)
summary (M2)


respDataExp2019 %>% filter (treatment == 1) %>% select (flux.raw) %>% 
  summarise (mean = mean (flux.raw, na.rm = TRUE),
             sd = sd (flux.raw, na.rm = TRUE))
#========================================================================================