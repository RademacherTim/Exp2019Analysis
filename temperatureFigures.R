#========================================================================================
# Script to plot the temperature data from the project chill shed and the phloem of all 
# trees at 1.0 and 2.0 m of the 2019 chilling experiment on red maple at Harvard Forest.
#----------------------------------------------------------------------------------------

# source temperature data
#----------------------------------------------------------------------------------------
source ('./readTemperatureData.R')

# plot temperature of chilled and control trees at 1 and 2m 
#----------------------------------------------------------------------------------------
layout (matrix (1:3), heights = c (1, 1, 1.2))
par (mar = c (1, 8, 1, 1))
plot (x = tempData [['datetime']], 
      y = tempData [['t.acer.02.2p0m']], 
      typ = 'l', las = 1,
      xlim = c (as_datetime ('2019-05-07'), as_datetime ('2019-07-18')),
      ylim = c (-10, 35),
      xaxt = 'n', xlab = '', ylab = '2.0 m',
      col = tColours [['colour']] [5])

# add desired chilling zone
rect (xleft = as_datetime ('2019-05-29 12:00:00'), xright = as_datetime ('2019-07-10 12:00:00'),
      ybottom = 0, ytop = 5, col = addOpacity ('#666666', 0.3), lty = 0)

# add critical dates
res <- criticalDates ('chilled')

# add more chilled trees
lines (x = tempData [['datetime']],
       y = tempData [['t.acer.04.2p0m']],
       col = tColours [['colour']] [5])
lines (x = tempData [['datetime']],
       y = tempData [['t.acer.06.2p0m']],
       col = tColours [['colour']] [5])
lines (x = tempData [['datetime']], 
       y = tempData [['t.acer.07.2p0m']],
       col = tColours [['colour']] [5])

# add control trees
lines (x = tempData [['datetime']], 
       y = tempData [['t.acer.01.2p0m']],
       col = tColours [['colour']] [1])
lines (x = tempData [['datetime']], 
       y = tempData [['t.acer.03.2p0m']],
       col = tColours [['colour']] [1]) 
lines (x = tempData [['datetime']], 
       y = tempData [['t.acer.05.2p0m']],
       col = tColours [['colour']] [1]) 
lines (x = tempData [['datetime']],
       y = tempData [['t.acer.08.2p0m']],
       col = tColours [['colour']] [1])

# add air temperature
lines (x = tempData [['datetime']], 
       y = tempData [['t.air.1p5m']],
       col = tColours [['colour']] [2])

# plot temperatures at 1.5m
plot (x = tempData [['datetime']], 
      y = tempData [['t.acer.02.1p5m']], 
      typ = 'l', las = 1,
      xlim = c (as_datetime ('2019-05-07'), as_datetime ('2019-07-18')),
      ylim = c (-10, 35),
      xaxt = 'n', xlab = '', ylab = '1.5 m',
      col = tColours [['colour']] [5])
mtext (text = expression (paste ('temperature (',degree,'C)', sep = '')), 
       side = 2, line = 5, cex = 1.8)

# add critical dates
res <- criticalDates ('chilled')

# add more chilled trees
lines (x = tempData [['datetime']], 
       y = tempData [['t.acer.04.1p5m']],
       col = tColours [['colour']] [5])
lines (x = tempData [['datetime']], 
       y = tempData [['t.acer.06.1p5m']],
       col = tColours [['colour']] [5])
lines (x = tempData [['datetime']], 
       y = tempData [['t.acer.07.1p5m']],
       col = tColours [['colour']] [5])

# add control trees
lines (x = tempData [['datetime']], 
       y = tempData [['t.acer.01.1p5m']],
       col = tColours [['colour']] [1])
lines (x = tempData [['datetime']], 
       y = tempData [['t.acer.03.1p5m']],
       col = tColours [['colour']] [1])
lines (x = tempData [['datetime']], 
       y = tempData [['t.acer.05.1p5m']],
       col = tColours [['colour']] [1])
lines (x = tempData [['datetime']], 
       y = tempData [['t.acer.08.1p5m']],
       col = tColours [['colour']] [1])

# add air temperature
lines (x = tempData [['datetime']], 
       y = tempData [['t.air.1p5m']],
       col = tColours [['colour']] [2])

# add temperatures at 1 m
par (mar = c (3, 8, 1, 1))
plot (x = tempData [['datetime']], 
      y = tempData [['t.acer.02.1p0m']], 
      typ = 'l', las = 1,
      xlim = c (as_datetime ('2019-05-07'), as_datetime ('2019-07-16')),
      ylim = c (-10, 35),
      xaxt = 'n', xlab = '', ylab = '1.0 m',
      col = tColours [['colour']] [5])
axis (side = 1, at = seq (as_datetime ('2019-05-01'), as_datetime ('2019-08-01'), 
                          length.out = 4), label = c ('May','June', 'July','August'))
# add desired chilling zone
rect (xleft = as_datetime ('2019-05-29 12:00:00'), xright = as_datetime ('2019-07-10 12:00:00'),
      ybottom = 0, ytop = 5, col = addOpacity ('#666666', 0.3), lty = 0)

# add critical dates
res <- criticalDates ('chilled')

# add more chilled trees
lines (x = tempData [['datetime']], 
       y = tempData [['t.acer.04.1p0m']],
       col = tColours [['colour']] [5])
lines (x = tempData [['datetime']], 
       y = tempData [['t.acer.06.1p0m']],
       col = tColours [['colour']] [5])
lines (x = tempData [['datetime']], 
       y = tempData [['t.acer.07.1p0m']],
       col = tColours [['colour']] [5])

# add control trees
lines (x = tempData [['datetime']], 
       y = tempData [['t.acer.01.1p0m']],
       col = tColours [['colour']] [1])
lines (x = tempData [['datetime']], 
       y = tempData [['t.acer.03.1p0m']],
       col = tColours [['colour']] [1])
lines (x = tempData [['datetime']], 
       y = tempData [['t.acer.05.1p0m']],
       col = tColours [['colour']] [1])
lines (x = tempData [['datetime']], 
       y = tempData [['t.acer.08.1p0m']],
       col = tColours [['colour']] [1])

# add air temperature
lines (x = tempData [['datetime']], 
       y = tempData [['t.air.1p5m']],
       col = tColours [['colour']] [2])

# add legend 
legend (x = as_datetime ('2019-05-07'), y = 35, box.lty = 0, 
        col = tColours [['colour']] [c (2, 1, 5)], 
        legend = c ('air temperature (1.5m)','control trees','chilled trees'),
        lwd = 1, cex = 0.6, bg = 'transparent')

