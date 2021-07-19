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
if (!existsFunction ('lmer')) library ('lme4')
if (!existsFunction ('cAIC')) library ('cAIC4')

# read in the ring width data that Patrick prepared 
#----------------------------------------------------------------------------------------
#if (!exists ('xyloData')) source ('readXylogenesisData.R')
if (!exists ('imageData')) source ('analyseCellWallArea.R')

# read in the visual observations
#----------------------------------------------------------------------------------------
if (!exists ('visualData')) {
  visualData <- read_csv (file = './data/visual_wood_anatomy_observations_HF_Exp2019.csv',
                          col_types = cols ()) 
}

# Add growing season dates to the xyloData
#----------------------------------------------------------------------------------------
visualData <- visualData %>% 
  mutate (treatment = ifelse (treatment == 1, 'control', 'chilled')) %>%
  select (-iadf, -comments)
xyloData <- merge (xyloData, visualData, by = c ('tree.id','treatment','sample.height')) 

# figure of growth over time for each control tree across sampling height
#----------------------------------------------------------------------------------------
layout (matrix (1:16, nrow = 4))
for (t in c (1, 3, 5, 8)) {
 for (h in c (4.0, 2.5, 1.5, 0.5)) {
   
   # set margins
   par (mar = c (3, 3, 1, 1))
   
   # select only relevant data
   tmpData <- xyloData %>% 
     filter (tree.id == (t + 1900) & sample.height == h & year == 2019) 
   
   # correct sample.doy for the 2020 sample
   tmpData [['sample.date']] [tmpData [['sample.date']] == as_date ('2020-08-04')] <- as_date ('2019-12-31')
   
   # fit monotonic general additive model to growth data
   fit.gam <- scam::scam (ring.width ~ s (sample.doy, k = 9, bs = 'mpi'), 
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
         xlim = c (as_date ('2019-04-01'), as_date ('2020-01-01')),
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

# figure of growth over time for each chilled tree across sampling height
#----------------------------------------------------------------------------------------
layout (matrix (1:16, nrow = 4))
for (t in c (2, 4, 6, 7)) {
  for (h in c (4.0, 2.5, 1.5, 0.5)) {
    
    # set margins
    par (mar = c (3, 3, 1, 1))
    
    # select only relevant data
    tmpData <- xyloData %>% 
      filter (tree.id == (t + 1900) & sample.height == h & year == 2019) 
    
    # correct sample.doy for the 2020 sample
    tmpData [['sample.date']] [tmpData [['sample.date']] == as_date ('2020-08-04')] <- as_date ('2019-12-31')
    
    
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
          xlim = c (as_date ('2019-04-01'), as_date ('2019-12-31')),
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

# figure of average growth over time for chilled and control trees across sampling height
#----------------------------------------------------------------------------------------
png (filename = './fig/Exp2019ChillingAbsoluteVolumeGrowthDynamics.png', width = 800, height = 700)
layout (matrix (1:4, nrow = 4), heights = c (1, 1, 1,  1.2))
for (h in c (4.0, 2.5, 1.5, 0.5)) {
  
  # determine panel margins
  #--------------------------------------------------------------------------------------
  if (h != 0.5) {
    par (mar = c (1, 5, 1, 4))
  } else {
    par (mar = c (4, 5, 1, 4))
  }
  
  # create plot outline 
  #--------------------------------------------------------------------------------------
  plot (NULL,
        xlim = c (121, 284), ylim = c (0, 3300), 
        axes = FALSE, 
        xlab = '', ylab = '') 
  
  # add critical dates
  #--------------------------------------------------------------------------------------
  res <- abline (v = lubridate::yday (c ('2018-05-29','2018-07-10')), 
                 col = '#999999', lty = 2, lwd = 1)

  # add axes
  #--------------------------------------------------------------------------------------
  if (h != 0.5) {
    axis (side = 1, at = c (91, 121, 152, 182, 213, 244, 274), labels = rep ('', 7))
  } else {
    axis (side = 1, at = c (91, 121, 152, 182, 213, 244, 274), 
          labels = c ('Apr','May','Jun','Jul','Aug','Sep','Oct'), cex.axis = 1.4)
    
    # Add sampling dates as orange tick marks on x-axis
    points (x = yday (unique (xyloData [['sample.date']])),
            y = rep (-145, length (unique (xyloData [['sample.date']]))), 
            pch = 3, lwd = 3, col = '#e37222', cex = 1.5)
    
  }  
  axis (side = 2, at = seq (0, 3000, 1000), labels = 0:3, las = 1, 
        cex.axis = 1.4)
  mtext (side = 2, line = 3.5, 
         text = expression (paste ('Cumulative radial growth (mm)',sep = '')), 
         at = 2000, cex = 0.7)
  
  # add monotonic GAM for treatment 
  for (t in c ('control', 'chilled')) {
    
    # get treatment specific data
    treatmentData <- xyloData %>% 
      filter (treatment == t & sample.height == h) %>% 
      arrange (by = sample.date)
    
    # only retain the 2019 ring widths
    treatmentData <- treatmentData %>% 
      filter (year == 2019)
    
    # Fit general additive model to growth data
    fit.gam <- scam::scam (ring.width ~ s (sample.doy, k = 8, bs = 'mpi'), 
                           data   = treatmentData, 
                           family = quasipoisson)
    
    # predict confidence interval for the model
    m <- predict (fit.gam, newdata = data.frame (sample.doy = 1:365), type = 'link', se.fit = TRUE) 
    
    # add mean and standard error for start of the growing season
    arrows (x0 = mean (treatmentData [['start.of.season.doy']]) - 
                 se   (treatmentData [['start.of.season.doy']]),
            x1 = mean (treatmentData [['start.of.season.doy']]) + 
                 se   (treatmentData [['start.of.season.doy']]), 
            y0 = 3000 + ifelse (t == 'control', -100, 100), 
            col = tColours [['colour']] [ifelse (t == 'control',1,5)], 
            length = 0, angle = 90, code = 3, lwd = 2)
    points (x = mean (treatmentData [['start.of.season.doy']]), 
            y = 3000 + ifelse (t == 'control', -100, 100), 
            pch = ifelse (t == 'control', 19, 23), 
            col = tColours [['colour']] [ifelse (t == 'control',1,5)], 
            cex = 1.8, bg = 'white', lwd = 2)
    
    # add mean and standard error for end of the growing season
    arrows (x0 = mean (treatmentData [['end.of.season.doy']], na.rm = TRUE) -
                 se   (treatmentData [['end.of.season.doy']]),
            x1 = mean (treatmentData [['end.of.season.doy']], na.rm = TRUE) +
                 se   (treatmentData [['end.of.season.doy']]),
            y0 = 3000 + ifelse (t == 'control', -100, 100),
            col = tColours [['colour']] [ifelse (t == 'control',1,5)],
            length = 0, angle = 90, code = 3, lwd = 2)
    points (x = mean (treatmentData [['end.of.season.doy']], na.rm = TRUE),
            y = 3000 + ifelse (t == 'control', -100, 100),
            pch = ifelse (t == 'control', 19, 23),
            col = tColours [['colour']] [ifelse (t == 'control',1,5)], 
            cex = 1.8, bg = 'white', lwd = 2)
    
    # determine last day of year for growth 
    endDOY <- ceiling (mean (treatmentData [['end.of.season.doy']], na.rm = TRUE) +
                       2*sd   (treatmentData [['end.of.season.doy']], na.rm = TRUE))

    # add confidence interval
    polygon (x = c (150:274, 274:150), 
             y = exp (c (m$fit [150:274] + m$se.fit [150:274], 
                         rev (m$fit [150:274] - m$se.fit [150:274]))), 
             lty = 0,
             col = addOpacity (tColours [['colour']] [ifelse (t == 'control',1,5)], 0.3))
    
    # add treatment mean behaviour
    lines (x = 1:150, y = exp (m$fit) [1:150], 
           col = '#99999999', 
           lwd = 2,
           lty = ifelse (h == 0.5, 3, 
                         ifelse (h == 1.5, 2, ifelse (h == 2.5, 1, 4))))
    # lines (x = endDOY:274, y = exp (m$fit) [endDOY:274], 
    #        col = '#99999999', 
    #        lwd = 2,
    #        lty = ifelse (h == 0.5, 3, 
    #                     ifelse (h == 1.5, 2, ifelse (h == 2.5, 1, 4))))
    lines (x = 150:274, y = exp (m$fit) [150:274], 
           col = tColours [['colour']] [ifelse (t == 'control',1,5)], 
           lwd = 2,
           lty = ifelse (h == 0.5, 3, 
                         ifelse (h == 1.5, 2, ifelse (h == 2.5, 1, 4))))
    
  } # end treatment loop

  # add mean increase in total mass
  #--------------------------------------------------------------------------------------
  par (new = TRUE)
  plot (NULL, xlim = c (121, 284), ylim = c (0, 1.3), axes = FALSE,
        xlab = '', ylab = '')
  axis (side = 4, at = seq (0, 1.2, by = 0.4), las = 1, cex.axis = 1.4)
  for (t in c ('control', 'chilled')) {
    con <- imageData %>% filter (treatment == t & sampleHeight == h & year == 2019)
    arrows (y0 = mean (con [['mass']], na.rm = TRUE) -
                 se   (con [['mass']]),
            y1 = mean (con [['mass']], na.rm = TRUE) +
                 se   (con [['mass']]),
            x0 = 283 + ifelse (t == 'control', -2, 2),
            col = tColours [['colour']] [ifelse (t == 'control',1,5)],
            length = 0, angle = 90, code = 3, lwd = 3)
    points (y = mean (con [['mass']], na.rm = TRUE),
            x = 283 + ifelse (t == 'control', -2, 2), 
            pch = ifelse (t == 'control', 19, 23),
            col = tColours [['colour']] [ifelse (t == 'control',1,5)],
            cex = 2.5, bg = 'white', lwd = 3)
  } # end second treatment loop
} # end sample height loop
dev.off ()

# Look at rhoCW, RW and mass
boxplot (mass ~ sample.height + treatment, imageData %>% filter (year == 2019))

# estimate the difference in growth onset between chilled and control trees
#----------------------------------------------------------------------------------------
tmpData <- xyloData %>% filter (year == 2019, sample.date == as_date ('2019-09-25')) %>%
  mutate (tree.id = factor (tree.id),
          treatment = factor (treatment),
          sample.height = factor (sample.height))
mod <- lmer (start.of.season.doy ~ (1| tree.id), 
             data = tmpData,
             REML = TRUE)
summary (mod)
cAIC (mod)
mod1 <- lmer (start.of.season.doy ~ (1| tree.id) + treatment, 
              data = tmpData,
              REML = TRUE)
summary (mod1)
cAIC (mod1)
mod1 <- lmer (start.of.season.doy ~ (1| tree.id) + sample.height, 
              data = tmpData,
              REML = TRUE)
summary (mod1)
cAIC (mod1)
mod2 <- lmer (start.of.season.doy ~ (1| tree.id) + sample.height + treatment, 
              data = tmpData,
              REML = TRUE)
summary (mod2)
cAIC (mod2)
mod3 <- lmer (start.of.season.doy ~ (1| tree.id) + sample.height * treatment, 
              data = tmpData,
              REML = TRUE)
summary (mod3)
cAIC (mod3)
mod4 <- lmer (start.of.season.doy ~ (1| tree.id) + sample.height:treatment, 
              data = tmpData,
              REML = TRUE)
summary (mod4)
cAIC (mod4)
# Report model 3 which has the lowest conditional AIC.

# Get mean growth across height for 2018 to see whether there were pre-existing differences
#----------------------------------------------------------------------------------------
xyloData %>% filter (year %in% 2014:2018) %>% 
  group_by (year, treatment, sample.height) %>%
  summarise (MRW = mean (ring.width, na.rm = TRUE),
             seRW = se (ring.width), .groups = 'drop') %>% 
  print (n = 48)
#========================================================================================
