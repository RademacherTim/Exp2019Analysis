#========================================================================================
# This script tests for differences between treatments and sampling heights in 
# stem CO2 efflux for the 2019 phloem chilling experiment on red maples at Harvard 
# Forest. 
#----------------------------------------------------------------------------------------

# load dependencies
#----------------------------------------------------------------------------------------
library ('lme4')

# read water potential data
#----------------------------------------------------------------------------------------
source ('./readWaterPotential.R')
source ('./plotingFunctions.R')

# get start and end dates for each treatment
#----------------------------------------------------------------------------------------
startDate <- as_date ('2019-05-29')
endDate   <- as_date ('2019-07-10')

# add column based on period (e.g., before, during, and after treatment)
#----------------------------------------------------------------------------------------
period <- ifelse (phi [['date']] < startDate, 'before', 
                  ifelse (phi [['date']] > endDate, 'after','during'))
periodAlt <- ifelse (phi [['date']] < startDate | 
                     phi [['date']] > endDate,'non-chilling','chilling')
phi <- phi %>% 
  mutate (period    = factor (period,    levels = c ('during','after','before')), 
          periodAlt = factor (periodAlt, levels = c ('chilling', 'non-chilling')))

# wrangle data 
#----------------------------------------------------------------------------------------
phi <- phi %>% mutate (tree      = factor (tree),
                       treatment = factor (treatment, levels =  c (5, 1)),
                       date      = factor (date))

# fit a mixed effects model to account for individual tree effects to nranch water 
# potential measurements
#----------------------------------------------------------------------------------------
M1 <- lmer (formula = phi.branch ~ (1|tree) + date + period:treatment, data = phi,
            REML = TRUE)
summary (M1)

# fit a mixed effects model to account for individual tree effects to nranch water 
# potential measurement
#----------------------------------------------------------------------------------------
M2 <- lmer (formula = phi.leaf ~ (1|tree) + date + period:treatment, 
            data = phi,
            REML = TRUE)
summary (M2)


#========================================================================================