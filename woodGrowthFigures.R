#========================================================================================
# This script plots volume growth over time for the eight maple trees from the 2019 
# chilling experiment at Harvard Forest. The experiment has four control trees (1901, 
# 1903, 1905, 1908) and four chilled trees (1902, 1904, 1906, 1907) that were chilled at 
# 1.0 and 2.0m.  
#----------------------------------------------------------------------------------------

# load dependencies
#----------------------------------------------------------------------------------------
if (!existsFunction ('scam')) library ('scam')
if (!existsFunction ('%>%'))  library ('tidyverse')

# read in the ring width data that Patrick prepared 
#----------------------------------------------------------------------------------------
if (!exists ('xyloData')) source ('readXylogenesisData.R')

# add day of year for the sample date 
#----------------------------------------------------------------------------------------
xyloData <- xyloData %>% mutate (sample.doy = yday (sample.date))

# make a figure of growth over time for each control tree across sampling height
#----------------------------------------------------------------------------------------
layout (matrix (1:16, nrow = 4))
for (t in c (1, 3, 5, 8)) {
 for (h in 4:1) {
   
   # set margins
   par (mar = c (3, 3, 1, 1))
   
   # select only relevant data
   tmpData <- xyloData %>% 
     filter (tree.id == (t + 1900) & sample.height == h & Year == 2019) 
   
   # correct sample.doy for the 2020 sample
   tmpData [['sample.doy']] [tmpData [['sample.date']] == as_date ('2020-08-04')] <- 365
   
   # fit monotonic general additive model to growth data
   fit.gam <- scam::scam (ring.width ~ s (sample.doy, k = 8, bs = 'mpi'), 
                          data   = tmpData, 
                          family = quasipoisson)
   

   # determine the max y-axis value
   if (t %in% c (1, 5, 8)) {
     maxY <- 3000
   } else {
     maxY <- 8000
   }   

   # volume growth over time
   plot (x = tmpData [['sample.date']],
         y = tmpData [['ring.width']], typ = 'p', col = tColours [['colour']] [1],
         xlab = '', ylab = expression (paste ('ring width (',mu,m,')', sep = '')), las = 1, 
         xlim = c (as_date ('2019-04-01'), as_date ('2019-10-15')),
         ylim = c (0, maxY), axes = FALSE)
   if (h != 1) { 
     axis (side = 1, at = c (as_date ('2019-04-01'), as_date ('2019-05-01'), as_date ('2019-06-01'), 
                             as_date ('2019-07-01'), as_date ('2019-08-01'), as_date ('2019-09-01'),
                             as_date ('2019-10-01')), labels = rep ('', 7))
   } else {
     axis (side = 1, c (as_date ('2019-04-01'), as_date ('2019-05-01'), as_date ('2019-06-01'), 
                        as_date ('2019-07-01'), as_date ('2019-08-01'), as_date ('2019-09-01'),
                        as_date ('2019-10-01')), labels = c ('Apr','May','Jun','Jul','Aug','Sep','Oct'))
   }
   axis (side = 2, las = 1)
   
   # add chilling dates
   abline (v = c (as_date ('2019-05-29'), as_date ('2019-07-10')), col = '#66666666', lty = 2)

   # plot GAM
   lines (x = as_date (1:365, origin = '2018-12-31'), 
          y = exp (predict (fit.gam, newdata = data.frame (sample.doy = 1:365))),
          col = tColours [['colour']] [1])
 }
}

# make a figure of growth over time for each chilled tree across sampling height
#----------------------------------------------------------------------------------------
layout (matrix (1:16, nrow = 4))
for (t in c (2, 4, 6, 7)) {
  for (h in 4:1) {
    
    # set margins
    par (mar = c (3, 3, 1, 1))
    
    # select only relevant data
    tmpData <- xyloData %>% 
      filter (tree.id == (t + 1900) & sample.height == h & Year == 2019) 
    
    # correct sample.doy for the 2020 sample
    tmpData [['sample.doy']] [tmpData [['sample.date']] == as_date ('2020-08-04')] <- 365
    
    
    # fit monotonic general additive model to growth data
    fit.gam <- scam::scam (ring.width ~ s (sample.doy, k = 8, bs = 'mpi'), 
                           data   = tmpData, 
                           family = quasipoisson)
    
    if (t %in% c (2, 6, 7)) {
      maxY <- 3000
    } else {
      maxY <- 7000
    }   
    
    # plot growth over time   
    plot (x = tmpData [['sample.date']],
          y = tmpData [['ring.width']], typ = 'p', col = tColours [['colour']] [5],
          xlab = '', ylab = expression (paste ('ring width (',mu,m,')', sep = '')), las = 1, 
          xlim = c (as_date ('2019-04-01'), as_date ('2019-10-15')),
          ylim = c (0, maxY), axes = FALSE)
    if (h != 1) { 
      axis (side = 1, at = c (as_date ('2019-04-01'), as_date ('2019-05-01'), as_date ('2019-06-01'), 
                              as_date ('2019-07-01'), as_date ('2019-08-01'), as_date ('2019-09-01'),
                              as_date ('2019-10-01')), labels = rep ('', 7))
    } else {
      axis (side = 1, c (as_date ('2019-04-01'), as_date ('2019-05-01'), as_date ('2019-06-01'), 
                         as_date ('2019-07-01'), as_date ('2019-08-01'), as_date ('2019-09-01'),
                         as_date ('2019-10-01')), labels = c ('Apr','May','Jun','Jul','Aug','Sep','Oct'))
    }
    axis (side = 2, las = 1)
    
    # add chilling dates
    abline (v = c (as_date ('2019-05-29'), as_date ('2019-07-10')), col = '#66666666', lty = 2)

    # plot GAM
    lines (x = as_date (1:365, origin = '2018-12-31'), 
           y = exp (predict (fit.gam, newdata = data.frame (sample.doy = 1:365))),
           col = tColours [['colour']] [5])
  }
}

#========================================================================================