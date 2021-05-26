#========================================================================================
# Script to plot the temperature data from the project chill shed and the phloem of all 
# trees at 1.0 and 2.0 m of the 2019 chilling experiment on red maple at Harvard Forest.
#----------------------------------------------------------------------------------------

# load libraries
#----------------------------------------------------------------------------------------
if (!existsFunction ('as_datetime')) library ('lubridate')

# source temperature data
#----------------------------------------------------------------------------------------
if (!exists ('tempData')) source ('./readTemperatureData.R')
if (!exists ('tColours')) source ('./plotingFunctions.R')

# summarise temperature by treatment
#----------------------------------------------------------------------------------------
summaryTemp <- tempData %>% 
  group_by (datetime, treatment, height) %>%
  summarise (meanTemp = mean (temp, na.rm = TRUE),
             sdTemp   = sd   (temp, na.rm = TRUE),
             .groups = 'drop') %>% 
  filter (datetime < as_datetime ('2019-07-18')) %>%
  arrange () 
# There are a few value in earlier July when only one sensor at 2.0m worked and the 
# standard deviation is resultingly NA, below we set those to 0 to be able to plot the 
# polygon for standard deviation nonetheless
#----------------------------------------------------------------------------------------
summaryTemp [['sdTemp']] [is.na (summaryTemp [['sdTemp']])] <- 0.0
  
# plot temperature of chilled and control trees at 1.0, 1.5, and 2.0 m 
#----------------------------------------------------------------------------------------
png (filename = './fig/Exp2019ChillingTemperature.png', width = 700, height = 400)
layout (matrix (1:3), heights = c (1, 1, 1.3))
for (h in c (2.0, 1.5, 1.0)) {

  # determine margins
  if (h != 1.0) {
    par (mar = c (1, 8, 1, 1))
  } else {
    par (mar = c (5, 8, 1, 1))
  }
  
  # plot the chilled trees average
  con <- summaryTemp [['treatment']] == 'chilled' & summaryTemp [['height']] == h
  plot (x = summaryTemp [['datetime']] [con], 
        y = summaryTemp [['meanTemp']] [con], 
        typ = 'l', las = 1,
        xlim = c (as_datetime ('2019-05-07'), as_datetime ('2019-07-18')),
        ylim = c (-2, 33), axes = FALSE,
        xaxt = 'n', xlab = '', ylab = paste (h, ' m', sep = ''),
        col = tColours [['colour']] [5], lwd = 2)
  polygon (x = c (summaryTemp [['datetime']] [con], rev (summaryTemp [['datetime']] [con])),
           y = c (summaryTemp [['meanTemp']] [con] - summaryTemp [['sdTemp']] [con],
                  rev (summaryTemp [['meanTemp']] [con] + summaryTemp [['sdTemp']] [con])),
           lty = 0, col = addOpacity (tColours [['colour']] [5], 0.3))
  
  # add desired chilling zone
  rect (xleft = as_datetime ('2019-05-29 12:00:00'), xright = as_datetime ('2019-07-10 12:00:00'),
        ybottom = 0, ytop = 5, col = addOpacity ('#666666', 0.3), lty = 0)
  
  # add critical dates
  res <- criticalDates ('chilled')
  
  # add air temperature
  if (h == 1.5) {
    con <- summaryTemp [['treatment']] == 'air' & summaryTemp [['height']] == 1.5
    lines (x = summaryTemp [['datetime']] [con], 
           y = summaryTemp [['meanTemp']] [con],
           col = tColours [['colour']] [7])
  }
  
  # add control trees
  con <- summaryTemp [['treatment']] == 'control' & summaryTemp [['height']] == h
  polygon (x = c (summaryTemp [['datetime']] [con], rev (summaryTemp [['datetime']] [con])),
           y = c (summaryTemp [['meanTemp']] [con] - summaryTemp [['sdTemp']] [con],
                  rev (summaryTemp [['meanTemp']] [con] + summaryTemp [['sdTemp']] [con])),
           lty = 0, col = addOpacity (tColours [['colour']] [1], 0.3))
  lines (x = summaryTemp [['datetime']] [con],
         y = summaryTemp [['meanTemp']] [con],
         col = tColours [['colour']] [1], lwd = 2)
  
  # add axes
  if (h != 1.0 ) {
    axis (side = 1, at = c (as_datetime ('2019-05-07'),
                            as_datetime ('2019-05-29'),
                            as_datetime ('2019-06-19'), 
                            as_datetime ('2019-07-10'), 
                            as_datetime ('2019-07-18')), 
          labels = rep ('', 5))
  } else {
    axis (side = 1, at = c (as_datetime ('2019-05-07'),
                            as_datetime ('2019-05-29'),
                            as_datetime ('2019-06-19'), 
                            as_datetime ('2019-07-10'), 
                            as_datetime ('2019-07-18')), 
          labels = c ('7 May', '29 May','19 Jun', '10 Jul','18 Jul'))
  }
  axis (side = 2, las = 1, at = seq (0, 30, by = 10))
}
dev.off ()
#========================================================================================