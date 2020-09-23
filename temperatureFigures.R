#========================================================================================
# Script to plot the temperature data from the project chill shed and the phloem of all 
# trees at 1.0 and 2.0 m.
#----------------------------------------------------------------------------------------

# source temperature data
#----------------------------------------------------------------------------------------
source ('./readTemperatureData.R')

# plot temperature of chilled and control trees at 1 and 2m 
#----------------------------------------------------------------------------------------
layout (matrix (1:2), heights = c (1, 1.2))
par (mar = c (1, 8, 1, 1))
plot (x = tempData [['TIMESTAMP']], 
      y = data [['Acer19p02_2p0m']], 
      typ = 'l', las = 1,
      xlim = c (as_datetime ('2019-05-07'), as_datetime ('2019-07-16')),
      ylim = c (-10, 35),
      xaxt = 'n', xlab = '', ylab = '2 m',
      col = tColours [['colour']] [5])
mtext (text = expression (paste ('phloem temperature (',degree,'C)', sep = '')), 
       side = 2, line = 5, cex = 1.8, at = -15)

# add desired chilling zone
rect (xleft = as_datetime ('2019-05-01'), xright = as_datetime ('2019-07-20'),
      ybottom = 0, ytop = 5, col = addOpacity ('#666666', 0.3), lty = 0)

# add critical dates
res <- criticalDates ('chilled')

# add more chilled trees
lines (x = tempData [['TIMESTAMP']], 
       y = data [['Acer19p04_2p0m']],
       col = tColours [['colour']] [5])
lines (x = tempData [['TIMESTAMP']], 
       y = data [['Acer19p06_2p0m']],
       col = tColours [['colour']] [5])
lines (x = tempData [['TIMESTAMP']], 
       y = data [['Acer19p07_2p0m']],
       col = tColours [['colour']] [5])

# add control trees
lines (x = tempData [['TIMESTAMP']], 
       y = data [['Acer19p01_2p0m']],
       col = tColours [['colour']] [1])
lines (x = tempData [['TIMESTAMP']], 
       y = data [['Acer19p03_2p0m']],
       col = tColours [['colour']] [1])
lines (x = tempData [['TIMESTAMP']], 
       y = data [['Acer19p05_2p0m']],
       col = tColours [['colour']] [1])
# lines (x = tempData [['TIMESTAMP']], 
#        y = data [['Acer19p08_2p0m']],
#        col = tColours [['colour']] [1])

# add air temperature
lines (x = tempData [['TIMESTAMP']], 
       y = data [['Misc1']],
       col = tColours [['colour']] [2])

# add temperatures at 1 m
par (mar = c (3, 8, 1, 1))
plot (x = tempData [['TIMESTAMP']], 
      y = data [['Acer19p02_1p0m']], 
      typ = 'l', las = 1,
      xlim = c (as_datetime ('2019-05-07'), as_datetime ('2019-07-16')),
      ylim = c (-10, 35),
      xaxt = 'n', xlab = '', ylab = '1 m',
      col = tColours [['colour']] [5])
axis (side = 1, at = seq (as_datetime ('2019-05-01'), as_datetime ('2019-08-01'), 
                          length.out = 4), label = c ('May','June', 'July','August'))
# add desired chilling zone
rect (xleft = as_datetime ('2019-05-01'), xright = as_datetime ('2019-07-20'),
      ybottom = 0, ytop = 5, col = addOpacity ('#666666', 0.3), lty = 0)

# add critical dates
res <- criticalDates ('chilled')

# add more chilled trees
lines (x = tempData [['TIMESTAMP']], 
       y = data [['Acer19p04_1p0m']],
       col = tColours [['colour']] [5])
lines (x = tempData [['TIMESTAMP']], 
       y = data [['Acer19p06_1p0m']],
       col = tColours [['colour']] [5])
lines (x = tempData [['TIMESTAMP']], 
       y = data [['Acer19p07_1p0m']],
       col = tColours [['colour']] [5])

# add control trees
lines (x = tempData [['TIMESTAMP']], 
       y = data [['Acer19p01_1p0m']],
       col = tColours [['colour']] [1])
lines (x = tempData [['TIMESTAMP']], 
       y = data [['Acer19p03_1p0m']],
       col = tColours [['colour']] [1])
lines (x = tempData [['TIMESTAMP']], 
       y = data [['Acer19p05_1p0m']],
       col = tColours [['colour']] [1])
lines (x = tempData [['TIMESTAMP']], 
       y = data [['Acer19p08_1p0m']],
       col = tColours [['colour']] [1])

# add air temperature
lines (x = tempData [['TIMESTAMP']], 
       y = data [['Misc1']],
       col = tColours [['colour']] [2])

# add legend 
legend (x = as_datetime ('2019-05-07'), y = 35, box.lty = 0, 
        col = tColours [['colour']] [c (2, 1, 5)], 
        legend = c ('air temperature','control trees','chilled trees'),
        lwd = 1, cex = 0.6, bg = 'transparent')


# plot (x = data [['TIMESTAMP']], y = data [['Acer19p02_2p0m']], typ = 'l')
# plot (x = data [['TIMESTAMP'  ]], y = data [['Acer19p04_1p0m']], typ = 'l')
# plot (x = data [['TIMESTAMP']], y = data [['Acer19p04_2p0m']], typ = 'l')
# plot (x = data [['TIMESTAMP']], y = data [['Acer19p06_1p0m']], typ = 'l') # Sensor died in July 2019
# plot (x = data [['TIMESTAMP']], y = data [['Acer19p06_2p0m']], typ = 'l') # Sensor died in September 2019
# plot (x = data [['TIMESTAMP']], y = data [['Acer19p07_1p0m']], typ = 'l')
# plot (x = data [['TIMESTAMP']], y = data [['Acer19p07_2p0m']], typ = 'l')
# 
# plot (x = data [['TIMESTAMP']], y = data [['Acer19p01_1p0m']], typ = 'l')
# plot (x = data [['TIMESTAMP']], y = data [['Acer19p01_2p0m']], typ = 'l')
# plot (x = data [['TIMESTAMP']], y = data [['Acer19p03_1p0m']], typ = 'l')
# plot (x = data [['TIMESTAMP']], y = data [['Acer19p03_2p0m']], typ = 'l')
# plot (x = data [['TIMESTAMP']], y = data [['Acer19p05_1p0m']], typ = 'l') 
# plot (x = data [['TIMESTAMP']], y = data [['Acer19p05_2p0m']], typ = 'l') 
# plot (x = data [['TIMESTAMP']], y = data [['Acer19p08_1p0m']], typ = 'l')
# plot (x = data [['TIMESTAMP']], y = data [['Acer19p08_2p0m']], typ = 'l') # Sensors gave weird data after May and is probably useless.

# Shed air temperature
plot (x = data [['TIMESTAMP']], y = data [['Misc1']], typ = 'l') 
plot (x = data [['TIMESTAMP']], y = data [['Misc2']], typ = 'l') 
