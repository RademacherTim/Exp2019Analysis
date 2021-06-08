#========================================================================================
# This script tests for differences between treatments and sampling heights in 
# stem CO2 efflux for the 2018 phloem chilling and compression experiment at Harvard 
# Forest. 
#----------------------------------------------------------------------------------------

# load dependencies
#----------------------------------------------------------------------------------------
if (!existsFunction ('lmer'))    library ('lme4')
if (!existsFunction ('%>%'))     library ('tidyverse')
if (!existsFunction ('as_date')) library ('lubridate')
if (!exists ('cAIC')) library ('cAIC4')

# read processed respiration data
#----------------------------------------------------------------------------------------
if (!exists ('respData2019')) source ('./readProcessedRespData.R')

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


# fit model with a tree random effect and date as one fixed effect
#----------------------------------------------------------------------------------------
M0 <- lmer (formula = flux.raw ~ (1 | tree) + date + period:treatment:height, 
            data = respDataExp2019,
            REML = TRUE)
summary (M0)
cAIC (M0)

# fit model with a nested tree and sample height random effect and date as one fixed effect
#----------------------------------------------------------------------------------------
M1 <- lmer (formula = flux.raw ~ (1 | tree / height) + date + period:treatment:height, 
            data = respDataExp2019,
            REML = TRUE)
summary (M1)
cAIC (M1)
# This model provides the best fit and is reported in the publication.

# fit model with a tree random effect and period as one fixed effect
#----------------------------------------------------------------------------------------
M2 <- lmer (formula = flux.raw ~ (1 | tree ) + period + period:treatment:height, 
            data = respDataExp2019,
            REML = TRUE)
summary (M2)
cAIC (M2)

# fit model with a nested tree and sample height random effect and period as one fixed effect
#----------------------------------------------------------------------------------------
M3 <- lmer (formula = flux.raw ~ (1 | tree / height) + period + period:treatment:height, 
            data = respDataExp2019,
            REML = TRUE)
summary (M3)
cAIC (M3)

# 
respDataExp2019 %>% filter (treatment == 1) %>% select (flux.raw) %>% 
  summarise (mean = mean (flux.raw, na.rm = TRUE),
             sd = sd (flux.raw, na.rm = TRUE))
#========================================================================================