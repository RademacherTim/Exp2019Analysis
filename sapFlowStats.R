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
sapFlowData %>% group_by (treatment) %>% filter ()

#========================================================================================