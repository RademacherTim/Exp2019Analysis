#========================================================================================
# Script to plot respiration data for the 2019 plhoem chilling experiment
#----------------------------------------------------------------------------------------

# load dependencies
#----------------------------------------------------------------------------------------
library ('lubridate')

# source ploting functions for treatment colours 
#----------------------------------------------------------------------------------------
source ('plotingFunctions.R')

# Read processed the respiration data
#----------------------------------------------------------------------------------------
source ('./readProcessedRespData.R')

# Source function to convert units from RespChamberFlux package
#----------------------------------------------------------------------------------------
source ('../stemCO2Efflux/RespChamberProc/R/convertUnits.R')

# Convert flux from micro mol per second per square meter to g per square meter per day 
#----------------------------------------------------------------------------------------
respDataExp2019 [['flux.raw.g']] <- convert_mumolPers_to_gPerday (respDataExp2019 [['flux.raw']])

# Plot respiration in control and chilled trees over time
#----------------------------------------------------------------------------------------
par (mar = c (5,5,1,1))
layout (matrix (1:8, nrow = 2))
for (t in c (1:6,8,7)) {
  con <- respDataExp2019 [['tree']] == t & respDataExp2019 [['chamber']] == 1
  plot (x = respDataExp2019 [['datetime']] [con],
        y = respDataExp2019 [['flux.raw']] [con], typ = 'l', las = 1,
        xlab = 'date', col = tColours [['colour']] [respDataExp2019 [['treatment']] [con]], 
        ylim = c (0, 4),
        ylab = expression (paste ('respiration rate (',mu, mol,' ', m^-2,' ', s^-1,')', sep = ' ')))
  for (c in 2:3) {
    con <- respDataExp2019 [['tree']] == t & respDataExp2019 [['chamber']] == c
    lines (x = respDataExp2019 [['datetime']] [con],
           y = respDataExp2019 [['flux.raw']] [con], 
           col = tColours [['colour']] [respDataExp2019 [['treatment']] [con]],
           lty = c)
  }
  
  # Add critical dates
  #--------------------------------------------------------------------------------------
  res <- criticalDates ('chilled')
  
  # Add tree panel descriptor
  #--------------------------------------------------------------------------------------
  text (x = as_datetime ('2019-05-04'), y = 3.8, labels = t, cex = 2)
}
# add a legend to the last panel
#----------------------------------------------------------------------------------------
legend (x = as_datetime ('2019-08-01'), y = 4, box.lty = 0, lty = 1:3, col = tColours [['colour']] [5], 
        legend = c ('0.5 m','1.5 m','2.5 m'))

# plot respiration versus air temperature for control only
#----------------------------------------------------------------------------------------
par (mfrow = c (1, 1))
plot (x = respDataExp2019 [['airt.c']] [respDataExp2019 [['treatment']] == 1],
      y = respDataExp2019 [['flux.raw']] [respDataExp2019 [['treatment']] == 1],
      xlab = expression (paste ('air temperature (', degree,'C)', sep = '')),
      ylab = expression (paste ('stem ',CO[2],' efflux (',mu, mol,' ', m^-2,' ', s^-1,')', sep = ' ')),
      xlim = c (0, 30), ylim = c (0, 10),
      pch = 19, las = 1, 
      col = addOpacity (tColours [['colour']] [respDataExp2019 [['treatment']] [respDataExp2019 [['treatment']] == 1]], 0.6))

# add observational trees to the plot to get a larger temperature variation and sample size 
#----------------------------------------------------------------------------------------
points (x = respDataObs [['airt.c']] [respDataObs [['treatment']] == 1],
        y = respDataObs [['flux.raw']] [respDataObs [['treatment']] == 1],
        pch = 19, 
        col = addOpacity (tColours [['colour']] [2], 0.6))

# legend 
#----------------------------------------------------------------------------------------
legend (x = 0, y = 10, box.lty = 0, col = addOpacity (tColours [['colour']] [1:2], 0.6),
        pch = 19, legend = c ('observational','experimental'))

# plot respiration versus soil moisture for control group only
#----------------------------------------------------------------------------------------
par (mfrow = c (1, 1))
plot (x = respDataExp2019 [['vwc.daily']] [respDataExp2019 [['treatment']] == 1],
      y = respDataExp2019 [['flux.raw']] [respDataExp2019 [['treatment']] == 1],
      xlab = 'daily mean soil moisture (%)',
      ylab = expression (paste ('stem ',CO[2],' efflux (',mu, mol,' ', m^-2,' ', s^-1,')', sep = ' ')),
      xlim = c (0.15, 0.45), ylim = c (0, 10),
      pch = 19, las = 1, 
      col = addOpacity (tColours [['colour']] [1], 0.6))

# add observational trees to the plot to get a larger temperature variation and sample size 
#----------------------------------------------------------------------------------------
points (x = respDataObs [['vwc.daily']] [respDataObs [['treatment']] == 1],
        y = respDataObs [['flux.raw']] [respDataObs [['treatment']] == 1],
        pch = 19, 
        col = addOpacity (tColours [['colour']] [2], 0.6))

# legend 
#----------------------------------------------------------------------------------------
legend (x = 0.15, y = 10, box.lty = 0, col = addOpacity (tColours [['colour']] [1:2], 0.6),
        pch = 19, legend = c ('observational','experimental'))
#========================================================================================