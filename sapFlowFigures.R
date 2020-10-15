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

# average temperature data over 15-minute
#----------------------------------------------------------------------------------------
sapFlowData <- sapFlowData %>% 
  select (datetime, v.sap.1, v.sap.2, v.sap.3, tree, treatment) %>%
  group_by (tree, treatment, datetime = cut (datetime, breaks = '1 hour')) %>%
  summarise (v.sap.1 = mean (v.sap.1, na.rm = TRUE),
             v.sap.2 = mean (v.sap.2, na.rm = TRUE),
             v.sap.3 = mean (v.sap.3, na.rm = TRUE)) %>% 
  mutate (datetime = as_datetime (datetime))
  # %>%
  # mutate (v.sap.1 = replace (v.sap.1, which (v.sap.1 < -0.05), NA),
  #         v.sap.2 = replace (v.sap.2, which (v.sap.2 < -0.05), NA),
  #         v.sap.3 = replace (v.sap.3, which (v.sap.3 < -0.05), NA))

# plot sap flow velocity (mm s-1) of chilled and control trees at 1.5m at three different 
# depths 
#----------------------------------------------------------------------------------------
for (treeID in 1:8) {
  png (filename = paste0 ('./fig/sapFlowExp19p',treeID,'.png'), width = 967, height = 457)
  tmp <- filter (sapFlowData, tree == treeID) 
  treatment <- unique (select (ungroup (tmp), treatment)) [[1]]
  dates <- c (as_datetime ('2019-05-20'), as_datetime ('2020-08-01'))
  par (mfrow = c (3, 1))
  par (mar = c (2, 7, 1, 1))
  plot (x = tmp [['datetime']], y = tmp [['v.sap.1']], typ = 'l', las = 1, xlim = dates, 
        ylim = c (-0.2, 0.5), ylab = expression ('sap flow velocity (mm'~s^{-1}~')'),
        xlab = '', col = tColours [['colour']] [treatment])
  mtext (side = 2, line = 5, text = 'inner')
  abline (v = c (as_datetime ('2019-05-29'), as_datetime ('2019-07-10'),
                 as_datetime ('2019-11-01'), as_datetime ('2020-05-01'), 
                 as_datetime ('2020-05-29'), as_datetime ('2020-07-10')))
  criticalDates (group = ifelse (treatment == 1, 'control', 'chilled'))
  plot (x = tmp [['datetime']], y = tmp [['v.sap.2']], typ = 'l', las = 1, xlim = dates, 
        ylim = c (-0.2, 0.5), ylab = expression ('sap flow velocity (mm'~s^{-1}~')'),
        xlab = '', col = tColours [['colour']] [treatment])
  mtext (side = 2, line = 5, text = 'middle')
  abline (v = c (as_datetime ('2019-05-29'), as_datetime ('2019-07-10'),
                 as_datetime ('2019-11-01'), as_datetime ('2020-05-01'), 
                 as_datetime ('2020-05-29'), as_datetime ('2020-07-10')))
  criticalDates (group = ifelse (treatment == 1, 'control', 'chilled'))
  par (mar = c (2, 7, 1, 1))
  plot (x = tmp [['datetime']], y = tmp [['v.sap.3']], typ = 'l', las = 1, xlim = dates, 
        ylim = c (-0.2, 0.5), ylab = expression ('sap flow velocity (mm'~s^{-1}~')'),
        xlab = 'date', col = tColours [['colour']] [treatment])
  mtext (side = 2, line = 5, text = 'outer')
  abline (v = c (as_datetime ('2019-05-29'), as_datetime ('2019-07-10'),
                 as_datetime ('2019-11-01'), as_datetime ('2020-05-01'), 
                 as_datetime ('2020-05-29'), as_datetime ('2020-07-10')))
  criticalDates (group = ifelse (treatment == 1, 'control', 'chilled'))
  text (x = tail (dates, n = 1), y = 0.45, labels = paste ('Tree',treeID), cex = 2)
  dev.off ()
}

# plot sap flow velocity (mm s-1) of chilled and control trees at 1.5m at three different 
# depths 
#----------------------------------------------------------------------------------------
for (treeID in 1:8) {
  png (filename = paste0 ('./fig/sapFlowExp19p',treeID,'_oneWeek.png'), width = 967, height = 457)
  tmp <- filter (sapFlowData, tree == treeID) 
  treatment <- unique (select (ungroup (tmp), treatment)) [[1]]
  dates <- c (as_datetime ('2019-06-25'), as_datetime ('2019-07-03'))
  chillingDates <- c (as_datetime ('2019-05-29'), as_datetime ('2020-07-10'))
  par (mfrow = c (3, 1))
  par (mar = c (2, 7, 1, 1))
  plot (x = tmp [['datetime']], y = tmp [['v.sap.1']], typ = 'l', las = 1, xlim = dates, 
        ylim = c (-0.1, 0.4), ylab = expression ('sap flow velocity (mm'~s^{-1}~')'),
        xlab = '', col = tColours [['colour']] [treatment])
  mtext (side = 2, line = 5, text = 'inner')
  axis (side = 1, at = seq (as_datetime ('2019-06-26 00:00:00'),
                            as_datetime ('2019-07-03 00:00:00'), len = 8), 
        labels = c ('26','27','28','29','30','1','2','3'))
  criticalDates (group = ifelse (treatment == 1, 'control', 'chilled'))
  plot (x = tmp [['datetime']], y = tmp [['v.sap.2']], typ = 'l', las = 1, xlim = dates, 
        ylim = c (-0.1, 0.4), ylab = expression ('sap flow velocity (mm'~s^{-1}~')'),
        xlab = '', col = tColours [['colour']] [treatment])
  mtext (side = 2, line = 5, text = 'middle')
  criticalDates (group = ifelse (treatment == 1, 'control', 'chilled'))
  par (mar = c (2, 7, 1, 1))
  plot (x = tmp [['datetime']], y = tmp [['v.sap.3']], typ = 'l', las = 1, xlim = dates, 
        ylim = c (-0.1, 0.4), ylab = expression ('sap flow velocity (mm'~s^{-1}~')'),
        xlab = 'date', col = tColours [['colour']] [treatment])
  mtext (side = 2, line = 5, text = 'outer')
  criticalDates (group = ifelse (treatment == 1, 'control', 'chilled'))
  text (x = tail (dates, n = 1), y = 0.45, labels = paste ('Tree',treeID), cex = 2)
  dev.off ()
}
#========================================================================================
