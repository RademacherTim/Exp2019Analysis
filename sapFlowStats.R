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

# reduce datetime to nearest 15 minute interval
#----------------------------------------------------------------------------------------
sapFlowData [['datetime']] <- sapFlowData [['datetime']] %>%
  floor_date (unit = '15 mins')

# average sap flow data over 15 minute intervals
#----------------------------------------------------------------------------------------
sapFlowData <- sapFlowData %>%
  select (datetime, v.sap.1, v.sap.2, v.sap.3, tree, treatment) %>%
  group_by (tree, treatment, datetime = cut (datetime, breaks = '15 min')) %>%
  summarise (v.sap.1 = mean (v.sap.1, na.rm = TRUE),
             v.sap.2 = mean (v.sap.2, na.rm = TRUE),
             v.sap.3 = mean (v.sap.3, na.rm = TRUE)) %>%
  mutate (datetime = as_datetime (datetime)) %>% ungroup ()

# get start and end date of the chilling
#----------------------------------------------------------------------------------------
startDate1   <- criticalDates (group = '5', asDate = TRUE, startOnly = TRUE) 
endDate1     <- criticalDates (group = '5', asDate = TRUE, endOnly   = TRUE)
startDate2   <- as_datetime ('2020-05-29')
endDate2     <- as_datetime ('2020-07-10')
seasonEnd    <- as_datetime ('2019-11-01')
seasonStart2 <- as_datetime ('2020-05-01')

# filter out off-season
#----------------------------------------------------------------------------------------
sapFlowData <- 
  sapFlowData %>% filter (datetime > as_datetime ('2019-05-01')) %>%
  filter (datetime < as_datetime ('2019-11-01') | datetime > as_datetime ('2020-05-01'))

# add column based on period (e.g., before, during, and after treatment)
#----------------------------------------------------------------------------------------
sapFlowData [['period']] [sapFlowData [['datetime']] < startDate1] <- 'early1'
sapFlowData [['period']] [sapFlowData [['datetime']] >= startDate1 & 
                          sapFlowData [['datetime']] <= endDate1] <- 'mid1'
sapFlowData [['period']] [sapFlowData [['datetime']] >= endDate1 & 
                          sapFlowData [['datetime']] <= as_datetime ('2020-01-01')] <- 'late1'
sapFlowData [['period']] [sapFlowData [['datetime']] >= as_datetime ('2020-01-01') & 
                          sapFlowData [['datetime']] < startDate2] <- 'early2'
sapFlowData [['period']] [sapFlowData [['datetime']] >= startDate2 & 
                            sapFlowData [['datetime']] <= endDate2] <- 'mid2'
sapFlowData [['period']] [sapFlowData [['datetime']] >= endDate2] <- 'late2'

# calculate mean sap flow velocity and standard deviation by group during the experiment
#----------------------------------------------------------------------------------------
# sapFlowData %>% group_by (treatment, period) %>% 
#   summarise (mean.v.sap.1 = mean (v.sap.1, na.rm = TRUE),
#              se.v.sap.1   = se   (v.sap.1),
#              mean.v.sap.2 = mean (v.sap.2, na.rm = TRUE),
#              se.v.sap.2   = se   (v.sap.2),
#              mean.v.sap.3 = mean (v.sap.3, na.rm = TRUE),
#              se.v.sap.3   = se   (v.sap.3))

# use mid-day rates only (e.g., 11-14h)
#----------------------------------------------------------------------------------------
middaySapFlowData <- sapFlowData %>% 
  filter (hour (datetime) >= 11 & hour (datetime) <= 14) %>%
  mutate (date = as_date (datetime))

# get midday median value of sapflow and replace negative values by NA
#----------------------------------------------------------------------------------------
middaySapFlowData <- middaySapFlowData %>% 
  group_by (tree, treatment, date, period) %>%
  summarise (v.sap.1 = median (v.sap.1, na.rm = TRUE),
             v.sap.2 = median (v.sap.2, na.rm = TRUE),
             v.sap.3 = median (v.sap.3, na.rm = TRUE)) %>% 
  ungroup ()

# wrangle data into long format
#----------------------------------------------------------------------------------------
temp <- pivot_longer (data = middaySapFlowData, 
                      cols = c (v.sap.1, v.sap.2, v.sap.3), 
                      names_to = 'depth', names_prefix = 'v.sap.', 
                      values_to = 'v.sap')

# filter for only depth with best signal for each tree
#----------------------------------------------------------------------------------------
temp <- temp %>% 
  filter ((tree == 1 & depth == 3) |
          (tree == 2 & depth == 3) | 
          (tree == 3 & depth == 3) | 
          (tree == 4 & depth == 3) | 
          (tree == 5 & depth == 2) | 
          (tree == 6 & depth == 2) | 
          (tree == 7 & depth == 3) | 
          (tree == 8 & depth == 2)) %>%
  select (-depth)

# exclude tree 7 in 2020, because the sensor broke
#----------------------------------------------------------------------------------------
temp <- temp %>% filter (!(tree == 7 & date > as_date ('2020-01-01')))

# filter out 2020 late period 
#----------------------------------------------------------------------------------------
temp <- temp %>% filter (period != 'late2')

# convert variable to factors
#----------------------------------------------------------------------------------------
temp <- temp %>%
  mutate (date      = factor (date),
          tree      = factor (tree),
          treatment = factor (treatment, levels = c (1, 5)),
          period    = factor (period,    levels = c ('mid1','early2','late2','mid2',
                                                     'early1','late1')))

# fit mixed effects model to wood sugar concentrations with tree and height as random 
# effects to account for idiosyncratic differences due to factors such as variations 
# in exact azimuth or systematic difference between trees
#----------------------------------------------------------------------------------------
M1 <- lmer (formula = v.sap ~ (1 | tree) + period + period:treatment, 
            data = temp,
            REML = TRUE)
summary (M1)

for (treeID in 1:8) {
    plot (x = select (filter (temp, tree == treeID), date) [[1]],
          y = select (filter (temp, tree == treeID), v.sap) [[1]], typ ='l',
          las = 1,
          main = paste0 ('Tree ',treeID))
}
#========================================================================================