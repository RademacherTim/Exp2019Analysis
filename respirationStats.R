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

# make sure all variables are factors
#----------------------------------------------------------------------------------------
respDataExp2019 [['date']]      <- factor (as_date (respDataExp2019 [['datetime']]))
respDataExp2019 [['treatment']] <- factor (respDataExp2019 [['treatment']], levels = c (5, 1))
respDataExp2019 [['tree']]      <- factor (respDataExp2019 [['tree']])
respDataExp2019 [['height']]    <- factor (respDataExp2019 [['chamber']])

# fit mixed effects model with tree as random effect
#----------------------------------------------------------------------------------------
M1 <- lmer (formula = flux.raw ~ (1 | tree) + date + height + date:treatment:height, 
            data = respDataExp2019,
            REML = TRUE)
summary (M1)

# divide period into before, during and after
#----------------------------------------------------------------------------------------
respDataExp2019 [['period']] <- 2
respDataExp2019 [['period']] [respDataExp2019 [['datetime']] <= as_datetime ('2019-07-03')] <- 1
respDataExp2019 [['period']] [respDataExp2019 [['datetime']] <= as_datetime ('2019-05-29')] <- 0
respDataExp2019 [['period']] <- factor (respDataExp2019 [['period']], levels = 0:2)

# fit mixed effects model with tree as random effect
#----------------------------------------------------------------------------------------
M2 <- lmer (formula = flux.raw ~ (1 | tree) + period + height + period:treatment:height, 
            data = respDataExp2019,
            REML = TRUE)
summary (M2)

#========================================================================================