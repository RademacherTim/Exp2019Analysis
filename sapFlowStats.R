#========================================================================================
# script to calcualte differences in sap flow velocity between chilled and non-chilled 
# red maples from the 2019 chilling experiment at Harvard Forest.
#----------------------------------------------------------------------------------------

# load dependencies
#----------------------------------------------------------------------------------------
library ('lme4')
source ('../Exp2019Analysis/plotingFunctions.R')

# source sap flow data
#----------------------------------------------------------------------------------------
source ('./readSapFlowData.R')

# reconvert datetime to a datetime
#----------------------------------------------------------------------------------------
sapFlowData [['datetime']] <- sapFlowData [['datetime']] %>%
  floor_date (unit = '15 mins')

# average temperature data over 15-minute intervals
#----------------------------------------------------------------------------------------
sapFlowData <- sapFlowData %>% 
  select (datetime, v.sap.1, v.sap.2, v.sap.3, tree, treatment) %>%
  group_by (tree, treatment, datetime = cut (datetime, breaks = '1 hour')) %>%
  summarise (v.sap.1 = mean (v.sap.1, na.rm = TRUE),
             v.sap.2 = mean (v.sap.2, na.rm = TRUE),
             v.sap.3 = mean (v.sap.3, na.rm = TRUE)) %>% 
  mutate (datetime = as_datetime (datetime)) %>% ungroup ()

# get start and end date of the chilling
#----------------------------------------------------------------------------------------
startDate <- criticalDates (group = '5', asDate = TRUE, startOnly = TRUE) 
endDate   <- criticalDates (group = '5', asDate = TRUE, endOnly   = TRUE)

# add column based on period (e.g., before, during, and after treatment)
#----------------------------------------------------------------------------------------
period <- ifelse (sapFlowData [['datetime']] < startDate, 'before', 
                  ifelse (sapFlowData [['datetime']] > endDate, 'after','during'))
periodAlt <- ifelse (sapFlowData [['datetime']] < startDate | 
                       sapFlowData [['datetime']] > endDate,'non-chilling','chilling')
sapFlowData <- sapFlowData %>% mutate (period, periodAlt)

# calculate mean sap flow velocity and standard deviation by group during the experiment
#----------------------------------------------------------------------------------------
sapFlowData %>% group_by (treatment, period) %>% 
  filter (datetime < as_datetime ('2019-11-01')) %>%
  summarise (mean.v.sap.1 = mean (v.sap.1, na.rm = TRUE),
             se.v.sap.1   = se   (v.sap.1),
             mean.v.sap.2 = mean (v.sap.2, na.rm = TRUE),
             se.v.sap.2   = se   (v.sap.2),
             mean.v.sap.3 = mean (v.sap.3, na.rm = TRUE),
             se.v.sap.3   = se   (v.sap.3))

# wrangle data into long format
#----------------------------------------------------------------------------------------
temp <- pivot_longer (data = sapFlowData, cols = c (v.sap.1, v.sap.2, v.sap.3), 
                      names_to = 'depth', names_prefix = 'v.sap.', 
                      values_to = 'v.sap')

# convert variable to factors
#----------------------------------------------------------------------------------------
temp <- temp %>%
  mutate (datetime  = factor (datetime),
          tree      = factor (tree),
          treatment = factor (treatment, levels = c (5, 1)),
          period    = factor (period,    levels = c ('during','after','before')),
          periodAlt = factor (periodAlt, levels = c ('chilling','non-chilling')),
          depth     = factor (depth,     levels = 3:1))

# fit mixed effects model to wood sugar concentrations with tree and height as random 
# effects to account for idiosyncratic differences due to factors such as variations 
# in exact azimuth or systematic difference between trees
#----------------------------------------------------------------------------------------
M1 <- lmer (formula = v.sap ~ (1 | tree) + depth + datetime + period:treatment, 
            data = temp,
            REML = TRUE)
summary (M1)
# TR Check whether there is another Inf in one of the v.sap, because the fixed effect model crashed.
#========================================================================================