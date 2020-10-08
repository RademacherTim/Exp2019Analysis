#========================================================================================
# script to plot sap flow velocity between chilled and non-chilled red maples from the 
# 2019 chilling experiment at Harvard Forest.
#----------------------------------------------------------------------------------------

# load dependencies
#----------------------------------------------------------------------------------------
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
  mutate (datetime = as_datetime (datetime))

# plot sap flow velocity (mm s-1) of chilled and control trees at 1.5m at three different 
# depths 
#----------------------------------------------------------------------------------------
for (treeID in 1:8) {
  png (filename = paste0 ('./fig/sapFlowExp19p',treeID,'.png'), width = 967, height = 457)
  tmp <- filter (sapFlowData, tree == treeID) 
  treatment <- unique (select (tmp, treatment)) [[2]]
  dates <- c (as_datetime ('2019-05-20'), as_datetime ('2019-11-03'))
  chillingDates <- c (as_datetime ('2019-05-29'), as_datetime ('2020-07-10'))
  par (mfrow = c (3, 1))
  par (mar = c (2, 7, 1, 1))
  plot (x = tmp [['datetime']], y = tmp [['v.sap.1']], typ = 'l', las = 1, xlim = dates, 
        ylim = c (-0.2, 0.5), ylab = expression ('sap flow velocity (mm'~s^{-1}~')'),
        xlab = '', col = tColours [['colour']] [treatment])
  mtext (side = 2, line = 5, text = 'inner')
  criticalDates (group = ifelse (treatment == 1, 'control', 'chilled'))
  plot (x = tmp [['datetime']], y = tmp [['v.sap.2']], typ = 'l', las = 1, xlim = dates, 
        ylim = c (-0.2, 0.5), ylab = expression ('sap flow velocity (mm'~s^{-1}~')'),
        xlab = '', col = tColours [['colour']] [treatment])
  mtext (side = 2, line = 5, text = 'middle')
  criticalDates (group = ifelse (treatment == 1, 'control', 'chilled'))
  par (mar = c (2, 7, 1, 1))
  plot (x = tmp [['datetime']], y = tmp [['v.sap.3']], typ = 'l', las = 1, xlim = dates, 
        ylim = c (-0.2, 0.5), ylab = expression ('sap flow velocity (mm'~s^{-1}~')'),
        xlab = 'date', col = tColours [['colour']] [treatment])
  mtext (side = 2, line = 5, text = 'outer')
  criticalDates (group = ifelse (treatment == 1, 'control', 'chilled'))
  text (x = tail (dates, n = 1), y = 0.45, labels = paste ('Tree',treeID), cex = 2)
  dev.off ()
}
#========================================================================================
