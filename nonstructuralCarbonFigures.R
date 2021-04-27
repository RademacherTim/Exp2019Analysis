#========================================================================================
# script to plot nonstructural carbon data from the 2019 chilling experiment on red maple
# at Harvard Forest. 
#----------------------------------------------------------------------------------------

# source colour schemes and ploting functions
#----------------------------------------------------------------------------------------
source ('./plotingFunctions.R')

# source processed data
#----------------------------------------------------------------------------------------
source ('./readNonstructuralCarbonData.R') 

# plot the 2019 stem sugar concentration data by tree
#----------------------------------------------------------------------------------------
layout (matrix (1:8, nrow = 2, byrow = TRUE))
for (t in c (1,3,5,8,2,4,6,7)) {
  # plot below treatment
  #--------------------------------------------------------------------------------------
  con <- stemData2019 [['treeID']] == t & stemData2019 [['sampleHeight']] == 1
  plot (x = stemData2019 [['DateOfSampleCollection']] [con],
        y = stemData2019 [['ConcentrationSugarPerDW']] [con], typ = 'l',
        xlab = 'date', ylab = 'sugar concentration (%DW)', las = 1,
        ylim = c (0, 3.75), lty = 3, lwd = 2,
        col = tColours [['colour']] [unique (stemData2019 [['treatment']] [con])])
  # add in between the treatments
  #--------------------------------------------------------------------------------------
  con <- stemData2019 [['treeID']] == t & stemData2019 [['sampleHeight']] == 2
  lines (x = stemData2019 [['DateOfSampleCollection']] [con],
         y = stemData2019 [['ConcentrationSugarPerDW']] [con], lty = 2, lwd = 2,
         col = tColours [['colour']] [unique (stemData2019 [['treatment']] [con])])
  # add in above the treatments
  #--------------------------------------------------------------------------------------
  con <- stemData2019 [['treeID']] == t & stemData2019 [['sampleHeight']] == 3
  lines (x = stemData2019 [['DateOfSampleCollection']] [con],
         y = stemData2019 [['ConcentrationSugarPerDW']] [con], lty = 1, lwd = 2,
         col = tColours [['colour']] [unique (stemData2019 [['treatment']] [con])])
  
  # add a tree panel identifier
  #--------------------------------------------------------------------------------------
  text (x = as_datetime ('2019-05-01'), y = 3.5, labels = t, cex = 2)
  
  # add critical dates
  #--------------------------------------------------------------------------------------
  res <- criticalDates (group = unique (stemData2019 [['treatment']] [con]), asDate = FALSE)
  
  # add legend
  #--------------------------------------------------------------------------------------
  if (t == 8) {
    legend (x = as.POSIXct ('2019-06-30'), cex = 0.6,
            y = 3.75, box.lty = 0, lty = 1:3, lwd = 1, legend = c ('above','middle','below'))}
}

# plot the 2019 stem starch concentration data by tree
#----------------------------------------------------------------------------------------
layout (matrix (1:8, nrow = 2, byrow = TRUE))
for (t in c (1,3,5,8,2,4,6,7)) {
  # plot below treatment
  #--------------------------------------------------------------------------------------
  con <- stemData2019 [['treeID']] == t & stemData2019 [['sampleHeight']] == 1
  plot (x = stemData2019 [['DateOfSampleCollection']] [con],
        y = stemData2019 [['ConcentrationStarchPerDW']] [con], typ = 'l',
        xlab = 'date', ylab = 'starch concentration (%DW)', las = 1,
        ylim = c (0, 4), lty = 3, lwd = 2,
        col = tColours [['colour']] [unique (stemData2019 [['treatment']] [con])])
  
  # add in between the treatments
  #--------------------------------------------------------------------------------------
  con <- stemData2019 [['treeID']] == t & stemData2019 [['sampleHeight']] == 2
  lines (x = stemData2019 [['DateOfSampleCollection']] [con],
         y = stemData2019 [['ConcentrationStarchPerDW']] [con], lty = 2, lwd = 2,
         col = tColours [['colour']] [unique (stemData2019 [['treatment']] [con])])
  
  # add in above the treatments
  #--------------------------------------------------------------------------------------
  con <- stemData2019 [['treeID']] == t & stemData2019 [['sampleHeight']] == 3
  lines (x = stemData2019 [['DateOfSampleCollection']] [con],
         y = stemData2019 [['ConcentrationStarchPerDW']] [con], lty = 1, lwd = 2,
         col = tColours [['colour']] [unique (stemData2019 [['treatment']] [con])])
  
  # add a tree panel identifier
  #--------------------------------------------------------------------------------------
  text (x = as_datetime ('2019-05-01'), y = 3.7, labels = t, cex = 2)
  
  # add critical dates
  #--------------------------------------------------------------------------------------
  res <- criticalDates (group = unique (stemData2019 [['treatment']] [con]), asDate = FALSE)
  
  # Add legend
  #--------------------------------------------------------------------------------------
  if (t == 8) {
    legend (x = as.POSIXct ('2019-07-30'), cex = 0.6,
            y = 4, box.lty = 0, lty = 1:3, lwd = 1, legend = c ('above','middle','below'))}
}

# summarise the data by treatment and sampling height
#----------------------------------------------------------------------------------------
summaryDataStem <- stemData2019 %>% 
  group_by (DateOfSampleCollection, treatment, sampleHeight) %>% 
  summarise (meanSugar = mean (ConcentrationSugarPerDW), 
             sdSugar   = sd (ConcentrationSugarPerDW), 
             seSugar   = se (ConcentrationSugarPerDW),
             meanStarch = mean (ConcentrationStarchPerDW), 
             sdStarch   = sd (ConcentrationStarchPerDW), 
             seStarch   = se (ConcentrationStarchPerDW))

# plot the 2019 stem sugar concentration data by treatment
#----------------------------------------------------------------------------------------
png (filename = './fig/stemSugarConcentrationByTreatmentExp2019.png', width = 700, height = 300)
layout (matrix (1:2, nrow = 1, byrow = TRUE), widths = c (1.2, 1))
for (t in c (1, 5)) {
  if (t == 1) {
    par (mar = c (3, 5, 1, 1))
  } else {
    par (mar = c (3, 1, 1, 1))
  }
  con <- summaryDataStem [['treatment']] == 1 &
    summaryDataStem [['sampleHeight']] == 1 &
    !is.na (summaryDataStem [['meanSugar']])
  plot (x = summaryDataStem [['DateOfSampleCollection']] [con],
        y = summaryDataStem [['meanSugar']] [con], 
        typ = 'l', xlab = '', 
        ylab = ifelse (t == 1, 'wood sugar concentration (% dry weight)',''), las = 1,
        ylim = c (0, 3.5), col = 'white', axes = FALSE)
  axis (side = 1, at = c (as_datetime ('2019-05-01'),
                          as_datetime ('2019-06-01'), as_datetime ('2019-07-01'),
                          as_datetime ('2019-08-01'), as_datetime ('2019-09-01'),
                          as_datetime ('2019-10-01'), as_datetime ('2019-11-01')),
        labels = c ('May','Jun','Jul','Aug','Sep','Oct','Nov'))
  if (t == 1) {
    axis (side = 2, at  = seq (0, 3.5, by = 0.5), las = 1)
  } else {
    axis (side = 2, at  = seq (0, 3.5, by = 0.5), las = 1, labels = rep ('', 8))
  }
  
  for (h in 1:3) {
    con <- summaryDataStem [['treatment']] == t &
      summaryDataStem [['sampleHeight']] == h &
      !is.na (summaryDataStem [['meanSugar']])
    polygon (x = c (summaryDataStem [['DateOfSampleCollection']] [con], 
                    rev (summaryDataStem [['DateOfSampleCollection']] [con])),
             y = c (summaryDataStem [['meanSugar']] [con] - summaryDataStem [['seSugar']] [con], 
                    rev (summaryDataStem [['meanSugar']] [con] + summaryDataStem [['seSugar']] [con])),
             col = addOpacity (tColours [['colour']] [t], 0.3), lty = 0)
    lines (x = summaryDataStem [['DateOfSampleCollection']] [con],
           y = summaryDataStem [['meanSugar']] [con], lty = ifelse (h == 1, 3, ifelse (h == 2, 2, 1)), 
           lwd = 2, col = tColours [['colour']] [t])
  }
  
  # add panel descriptor
  #--------------------------------------------------------------------------------------
  #text (x = as.POSIXct ('2019-04-05'),
  #      y = 3.3, cex = 2, pos = 4, 
  #      labels = ifelse (t == 1, 'control', ifelse (t == 4, 'compressed', 'chilled')))
  
  # add critical dates
  #--------------------------------------------------------------------------------------
  res <- criticalDates (group = 5, asDate = FALSE)
  
  # add legend
  #--------------------------------------------------------------------------------------
  #if (t == 5) {
  #  legend (x = as.POSIXct ('2019-07-25'), bg = 'transparent',
  #          y = 3.5, box.lty = 0, lty = 1:3, lwd = 2, legend = c ('above','middle','below'))}
  
}
dev.off ()

# plot the 2019 stem starch concentration data by treatment
#----------------------------------------------------------------------------------------
png (filename = './fig/stemStarchConcentrationByTreatmentExp2019.png', width = 700, height = 300)
layout (matrix (1:2, nrow = 1, byrow = TRUE))
for (t in c (1, 5)) {
  if (t == 1) {
    par (mar = c (3, 5, 1, 1))
  } else {
    par (mar = c (3, 1, 1, 1))
  }
  con <- summaryDataStem [['treatment']] == 1 &
    summaryDataStem [['sampleHeight']] == 1 &
    !is.na (summaryDataStem [['meanStarch']])
  plot (x = summaryDataStem [['DateOfSampleCollection']] [con],
        y = summaryDataStem [['meanStarch']] [con], 
        typ = 'l', xlab = '', 
        ylab = ifelse (t == 1, 'wood starch concentration (% dry weight)',''), las = 1,
        ylim = c (0, 3), col = 'white', axes = FALSE)
  axis (side = 1, at = c (as_datetime ('2019-05-01'),
                          as_datetime ('2019-06-01'), as_datetime ('2019-07-01'),
                          as_datetime ('2019-08-01'), as_datetime ('2019-09-01'),
                          as_datetime ('2019-10-01'), as_datetime ('2019-11-01')),
        labels = c ('May','Jun','Jul','Aug','Sep','Oct','Nov'))
  if (t == 1) {
    axis (side = 2, at  = seq (0, 3.0, by = 0.5), las = 1)
  } else {
    axis (side = 2, at  = seq (0, 3.0, by = 0.5), las = 1, labels = rep ('', 7))
  }
  
  for (h in 1:3) {
    con <- summaryDataStem [['treatment']] == t &
      summaryDataStem [['sampleHeight']] == h &
      !is.na (summaryDataStem [['meanStarch']])
    polygon (x = c (summaryDataStem [['DateOfSampleCollection']] [con], 
                    rev (summaryDataStem [['DateOfSampleCollection']] [con])),
             y = c (summaryDataStem [['meanStarch']] [con] - summaryDataStem [['seStarch']] [con], 
                    rev (summaryDataStem [['meanStarch']] [con] + summaryDataStem [['seStarch']] [con])),
             col = addOpacity (tColours [['colour']] [t], 0.3), lty = 0)
    lines (x = summaryDataStem [['DateOfSampleCollection']] [con],
           y = summaryDataStem [['meanStarch']] [con], lty = ifelse (h == 1, 3, ifelse (h == 2, 2, 1)), 
           lwd = 2, col = tColours [['colour']] [t])
  }
  
  # add panel descriptor
  #--------------------------------------------------------------------------------------
  #text (x = as.POSIXct ('2019-04-05'),
  #     y = 2.8, cex = 2, pos = 4, 
  #      labels = ifelse (t == 1, 'control', ifelse (t == 4, 'compressed', 'chilled')))
  
  # add critical dates
  #--------------------------------------------------------------------------------------
  criticalDates (group = 5, asDate = FALSE)
  
  # add legend
  #--------------------------------------------------------------------------------------
  #if (t == 5) {
  #  legend (x = as.POSIXct ('2019-07-15'), bg = 'transparent',
  #          y = 3.1, box.lty = 0, lty = 1:3, lwd = 2, legend = c ('above','middle','below'))}
}   
dev.off ()

# plot the 2019 phloem sugar concentration data by tree
#----------------------------------------------------------------------------------------
layout (matrix (1:8, nrow = 2, byrow = TRUE))
for (t in c (1,3,5,8,2,4,6,7)) {
  # plot below treatment
  #--------------------------------------------------------------------------------------
  con <- phloemData2019 [['treeID']] == t & phloemData2019 [['sampleHeight']] == 1
  plot (x = phloemData2019 [['DateOfSampleCollection']] [con],
        y = phloemData2019 [['ConcentrationSugarPerDW']] [con], typ = 'l',
        xlab = 'date', ylab = 'sugar concentration (%DW)', las = 1,
        xlim = c (as_datetime ('2019-04-15'), as_datetime ('2019-10-01')),
        ylim = c (0, 6.5), lty = 3, lwd = 2,
        col = tColours [['colour']] [unique (phloemData2019 [['treatment']] [con])])
  # add in between the treatments
  #--------------------------------------------------------------------------------------
  con <- phloemData2019 [['treeID']] == t & phloemData2019 [['sampleHeight']] == 2
  lines (x = phloemData2019 [['DateOfSampleCollection']] [con],
         y = phloemData2019 [['ConcentrationSugarPerDW']] [con], lty = 2, lwd = 2,
         col = tColours [['colour']] [unique (phloemData2019 [['treatment']] [con])])
  # add in above the treatments
  #--------------------------------------------------------------------------------------
  con <- phloemData2019 [['treeID']] == t & phloemData2019 [['sampleHeight']] == 3
  lines (x = phloemData2019 [['DateOfSampleCollection']] [con],
         y = phloemData2019 [['ConcentrationSugarPerDW']] [con], lty = 1, lwd = 2,
         col = tColours [['colour']] [unique (phloemData2019 [['treatment']] [con])])
  
  # add a tree panel identifier
  #--------------------------------------------------------------------------------------
  text (x = as_datetime ('2019-05-01'), y = 6.2, labels = t, cex = 2)
  
  # add critical dates
  #--------------------------------------------------------------------------------------
  res <- criticalDates (group = unique (phloemData2019 [['treatment']] [con]), asDate = FALSE)
  
  # add legend
  #--------------------------------------------------------------------------------------
  if (t == 8) {
    legend (x = as.POSIXct ('2019-06-30'), cex = 0.6,
            y = 6.5, box.lty = 0, lty = 1:3, lwd = 1, legend = c ('above','middle','below'))}
}

# plot the 2019 phloem starch concentration data by tree
#----------------------------------------------------------------------------------------
layout (matrix (1:8, nrow = 2, byrow = TRUE))
for (t in c (1,3,5,8,2,4,6,7)) {
  # plot below treatment
  #--------------------------------------------------------------------------------------
  con <- phloemData2019 [['treeID']] == t & phloemData2019 [['sampleHeight']] == 1
  plot (x = phloemData2019 [['DateOfSampleCollection']] [con],
        y = phloemData2019 [['ConcentrationStarchPerDW']] [con], typ = 'l',
        xlab = 'date', ylab = 'starch concentration (%DW)', las = 1,
        xlim = c (as_datetime ('2019-04-15'), as_datetime ('2019-10-01')),
        ylim = c (0, 4), lty = 3, lwd = 2,
        col = tColours [['colour']] [unique (phloemData2019 [['treatment']] [con])])
  
  # add in between the treatments
  #--------------------------------------------------------------------------------------
  con <- phloemData2019 [['treeID']] == t & phloemData2019 [['sampleHeight']] == 2
  lines (x = phloemData2019 [['DateOfSampleCollection']] [con],
         y = phloemData2019 [['ConcentrationStarchPerDW']] [con], lty = 2, lwd = 2,
         col = tColours [['colour']] [unique (phloemData2019 [['treatment']] [con])])
  
  # add in above the treatments
  #--------------------------------------------------------------------------------------
  con <- phloemData2019 [['treeID']] == t & phloemData2019 [['sampleHeight']] == 3
  lines (x = phloemData2019 [['DateOfSampleCollection']] [con],
         y = phloemData2019 [['ConcentrationStarchPerDW']] [con], lty = 1, lwd = 2,
         col = tColours [['colour']] [unique (phloemData2019 [['treatment']] [con])])
  
  # add a tree panel identifier
  #--------------------------------------------------------------------------------------
  text (x = as_datetime ('2019-05-01'), y = 3.7, labels = t, cex = 2)
  
  # add critical dates
  #--------------------------------------------------------------------------------------
  res <- criticalDates (group = unique (phloemData2019 [['treatment']] [con]), asDate = FALSE)
  
  # Add legend
  #--------------------------------------------------------------------------------------
  if (t == 8) {
    legend (x = as.POSIXct ('2019-07-30'), cex = 0.6,
            y = 4, box.lty = 0, lty = 1:3, lwd = 1, legend = c ('above','middle','below'))}
}

# summarise the data by treatment and sampling height
#----------------------------------------------------------------------------------------
summaryDataPhloem <- phloemData2019 %>% 
  group_by (DateOfSampleCollection, treatment, sampleHeight) %>% 
  summarise (meanSugar = mean (ConcentrationSugarPerDW), 
             sdSugar   = sd (ConcentrationSugarPerDW), 
             seSugar   = se (ConcentrationSugarPerDW),
             meanStarch = mean (ConcentrationStarchPerDW), 
             sdStarch   = sd (ConcentrationStarchPerDW), 
             seStarch   = se (ConcentrationStarchPerDW),
             .groups = 'keep')

# plot the 2019 phloem sugar concentration data by treatment
#----------------------------------------------------------------------------------------
png (filename = './fig/phloemSugarConcentrationByTreatmentExp2019.png', width = 700, height = 300)
layout (matrix (1:2, nrow = 1, byrow = TRUE))
for (t in c (1, 5)) {  
  if (t == 1) {
    par (mar = c (3, 5, 1, 1))
  } else {
    par (mar = c (3, 1, 1, 1))
  }
  con <- summaryDataPhloem [['treatment']] == 1 &
    summaryDataPhloem [['sampleHeight']] == 1 &
    !is.na (summaryDataPhloem [['meanSugar']])
  plot (x = summaryDataPhloem [['DateOfSampleCollection']] [con],
        y = summaryDataPhloem [['meanSugar']] [con], 
        typ = 'l', xlab = '', 
        ylab = ifelse (t == 1,'phloem sugar concentration (% dry weight)',''), las = 1,
        xlim = c (as_datetime ('2019-04-15'), as_datetime ('2019-10-01')),
        ylim = c (0, 5.5), col = 'white', axes = FALSE)
  axis (side = 1, at = c (as_datetime ('2019-05-01'),
                          as_datetime ('2019-06-01'), as_datetime ('2019-07-01'),
                          as_datetime ('2019-08-01'), as_datetime ('2019-09-01'),
                          as_datetime ('2019-10-01'), as_datetime ('2019-11-01')),
        labels = c ('May','Jun','Jul','Aug','Sep','Oct','Nov'))
  if (t == 1) {
    axis (side = 2, at = 0:5, las = 1)
  } else {
    axis (side = 2, at = 0:5, las = 1, labels = rep ('', 6))
  }
  
  for (h in 1:3) {
    con <- summaryDataPhloem [['treatment']] == t &
      summaryDataPhloem [['sampleHeight']] == h &
      !is.na (summaryDataPhloem [['meanSugar']])
    polygon (x = c (summaryDataPhloem [['DateOfSampleCollection']] [con], 
                    rev (summaryDataPhloem [['DateOfSampleCollection']] [con])),
             y = c (summaryDataPhloem [['meanSugar']] [con] - summaryDataPhloem [['seSugar']] [con], 
                    rev (summaryDataPhloem [['meanSugar']] [con] + summaryDataPhloem [['seSugar']] [con])),
             col = addOpacity (tColours [['colour']] [t], 0.3), lty = 0)
    lines (x = summaryDataPhloem [['DateOfSampleCollection']] [con],
           y = summaryDataPhloem [['meanSugar']] [con], lty = ifelse (h == 1, 3, ifelse (h == 2, 2, 1)), 
           lwd = 2, col = tColours [['colour']] [t])
  }
  
  # add panel descriptor
  #--------------------------------------------------------------------------------------
  #text (x = as.POSIXct ('2019-04-05'),
  #      y = 5.3, cex = 2, pos = 4, 
  #      labels = ifelse (t == 1, 'control', ifelse (t == 4, 'compressed', 'chilled')))
  
  # add critical dates
  #--------------------------------------------------------------------------------------
  res <- criticalDates (group = 5, asDate = FALSE)
  
  # add legend
  #--------------------------------------------------------------------------------------
  #if (t == 5) {
  #  legend (x = as.POSIXct ('2019-07-25'), bg = 'transparent',
  #          y = 5.6, box.lty = 0, lty = 1:3, lwd = 2, legend = c ('above','middle','below'))}
  
}
dev.off ()

# plot the 2019 phloem starch concentration data by treatment
#----------------------------------------------------------------------------------------
png (filename = './fig/phloemStarchConcentrationByTreatmentExp2019.png', width = 700, height = 300)
layout (matrix (1:2, nrow = 1, byrow = TRUE))
for (t in c (1, 5)) {
  if (t == 1) {
    par (mar = c (3, 5, 1, 1))
  } else {
    par (mar = c (3, 1, 1, 1))
  }
  con <- summaryDataPhloem [['treatment']] == 1 &
    summaryDataPhloem [['sampleHeight']] == 1 &
    !is.na (summaryDataPhloem [['meanStarch']])
  plot (x = summaryDataPhloem [['DateOfSampleCollection']] [con],
        y = summaryDataPhloem [['meanStarch']] [con], 
        typ = 'l', xlab = '', 
        ylab = ifelse (t == 1,'phloem starch concentration (% dry weight)',''), las = 1,
        xlim = c (as_datetime ('2019-04-15'), as_datetime ('2019-10-01')),
        ylim = c (0, 3), col = 'white', axes = FALSE)
  axis (side = 1, at = c (as_datetime ('2019-05-01'),
                          as_datetime ('2019-06-01'), as_datetime ('2019-07-01'),
                          as_datetime ('2019-08-01'), as_datetime ('2019-09-01'),
                          as_datetime ('2019-10-01'), as_datetime ('2019-11-01')),
        labels = c ('May','Jun','Jul','Aug','Sep','Oct','Nov'))
  if (t == 1) {
    axis (side = 2, at  = seq (0, 3.0, by = 0.5), las = 1)
  } else {
    axis (side = 2, at  = seq (0, 3.0, by = 0.5), las = 1, labels = rep ('', 7))
  }
  
  for (h in 1:3) {
    con <- summaryDataPhloem [['treatment']] == t &
      summaryDataPhloem [['sampleHeight']] == h &
      !is.na (summaryDataPhloem [['meanStarch']])
    polygon (x = c (summaryDataPhloem [['DateOfSampleCollection']] [con], 
                    rev (summaryDataPhloem [['DateOfSampleCollection']] [con])),
             y = c (summaryDataPhloem [['meanStarch']] [con] - summaryDataPhloem [['seStarch']] [con], 
                    rev (summaryDataPhloem [['meanStarch']] [con] + summaryDataPhloem [['seStarch']] [con])),
             col = addOpacity (tColours [['colour']] [t], 0.3), lty = 0)
    lines (x = summaryDataPhloem [['DateOfSampleCollection']] [con],
           y = summaryDataPhloem [['meanStarch']] [con], lty = ifelse (h == 1, 3, ifelse (h == 2, 2, 1)), 
           lwd = 2, col = tColours [['colour']] [t])
  }
  
  # add panel descriptor
  #--------------------------------------------------------------------------------------
  #text (x = as.POSIXct ('2019-04-05'),
  #      y = 2.8, cex = 2, pos = 4, 
  #      labels = ifelse (t == 1, 'control', ifelse (t == 4, 'compressed', 'chilled')))
  
  # add critical dates
  #--------------------------------------------------------------------------------------
  criticalDates (group = 5, asDate = FALSE)
  
  # add legend
  #--------------------------------------------------------------------------------------
  #if (t == 5) {
  #  legend (x = as.POSIXct ('2019-07-15'), bg = 'transparent',
  #          y = 3.1, box.lty = 0, lty = 1:3, lwd = 2, legend = c ('above','middle','below'))}
}  
dev.off () 

# plot the 2019 leaf sugar concentration data by tree
#----------------------------------------------------------------------------------------
layout (matrix (1:8, nrow = 2, byrow = TRUE))
for (t in c (1,3,5,8,2,4,6,7)) {
  # plot below treatment
  #--------------------------------------------------------------------------------------
  con <- leafData2019 [['treeID']] == t 
  plot (x = leafData2019 [['DateOfSampleCollection']] [con],
        y = leafData2019 [['ConcentrationSugarPerDW']] [con], typ = 'l',
        xlab = 'date', ylab = 'sugar concentration (%DW)', las = 1,
        xlim = c (as_datetime ('2019-04-15'), as_datetime ('2019-10-01')), ylim = c (0, 10), 
        lty = 1, lwd = 2,
        col = tColours [['colour']] [unique (leafData2019 [['treatment']] [con])])
  
  # add a tree panel identifier
  #--------------------------------------------------------------------------------------
  text (x = as_datetime ('2019-05-01'), y = 9, labels = t, cex = 2)
  
  # add critical dates
  #--------------------------------------------------------------------------------------
  res <- criticalDates (group = unique (leafData2019 [['treatment']] [con]), asDate = FALSE)
  
}

# plot the 2019 leaf starch concentration data by tree
#----------------------------------------------------------------------------------------
layout (matrix (1:8, nrow = 2, byrow = TRUE))
for (t in c (1,3,5,8,2,4,6,7)) {
  # plot below treatment
  #--------------------------------------------------------------------------------------
  con <- leafData2019 [['treeID']] == t 
  plot (x = leafData2019 [['DateOfSampleCollection']] [con],
        y = leafData2019 [['ConcentrationStarchPerDW']] [con], typ = 'l',
        xlab = 'date', ylab = 'starch concentration (%DW)', las = 1,
        xlim = c (as_datetime ('2019-04-15'), as_datetime ('2019-10-01')), ylim = c (0, 4), 
        lty = 1, lwd = 2,
        col = tColours [['colour']] [unique (leafData2019 [['treatment']] [con])])
  
  # add a tree panel identifier
  #--------------------------------------------------------------------------------------
  text (x = as_datetime ('2019-05-01'), y = 3.7, labels = t, cex = 2)
  
  # add critical dates
  #--------------------------------------------------------------------------------------
  res <- criticalDates (group = unique (leafData2019 [['treatment']] [con]), asDate = FALSE)
  
}

# summarise the data by treatment and sampling height
#----------------------------------------------------------------------------------------
summaryDataLeaves <- leafData2019 %>% 
  group_by (DateOfSampleCollection, treatment) %>% 
  summarise (meanSugar = mean (ConcentrationSugarPerDW), 
             sdSugar   = sd (ConcentrationSugarPerDW), 
             seSugar   = se (ConcentrationSugarPerDW),
             meanStarch = mean (ConcentrationStarchPerDW), 
             sdStarch   = sd (ConcentrationStarchPerDW), 
             seStarch   = se (ConcentrationStarchPerDW),
             .groups = 'keep')

# plot the 2019 leaf sugar concentration data by treatment
#----------------------------------------------------------------------------------------
png (filename = './fig/leafSugarConcentrationByTreatmentExp2019.png', width = 700, height = 300)
layout (matrix (1:2, nrow = 1, byrow = TRUE))
for (t in c (1, 5)) {
  if (t == 1) {
    par (mar = c (3, 5, 1, 1))
  } else {
    par (mar = c (3, 1, 1, 1))
  }
  
  con <- summaryDataLeaves [['treatment']] == t &
    !is.na (summaryDataLeaves [['meanSugar']])
  plot (x = summaryDataLeaves [['DateOfSampleCollection']] [con],
        y = summaryDataLeaves [['meanSugar']] [con], lwd = 2,
        typ = 'l', xlab = '', 
        ylab = ifelse (t == 1,'leaf sugar concentration (% dry weight)',''), las = 1,
        xlim = c (as_datetime ('2019-04-15'), as_datetime ('2019-10-01')), ylim = c (0, 8), 
        col = 'white', axes = FALSE)
  axis (side = 1, at = c (as_datetime ('2019-05-01'),
                          as_datetime ('2019-06-01'), as_datetime ('2019-07-01'),
                          as_datetime ('2019-08-01'), as_datetime ('2019-09-01'),
                          as_datetime ('2019-10-01'), as_datetime ('2019-11-01')),
        labels = c ('May','Jun','Jul','Aug','Sep','Oct','Nov'))
  if (t == 1) {
    axis (side = 2, at  = seq (0, 8, by = 2), las = 1)
  } else {
    axis (side = 2, at  = seq (0, 8, by = 2), las = 1, labels = rep ('', 5))
  }
  
  polygon (x = c (summaryDataLeaves [['DateOfSampleCollection']] [con], 
                  rev (summaryDataLeaves [['DateOfSampleCollection']] [con])),
           y = c (summaryDataLeaves [['meanSugar']] [con] - summaryDataLeaves [['seSugar']] [con], 
                  rev (summaryDataLeaves [['meanSugar']] [con] + summaryDataLeaves [['seSugar']] [con])),
           col = addOpacity (tColours [['colour']] [t], 0.3), lty = 0)
  lines (x = summaryDataLeaves [['DateOfSampleCollection']] [con],
         y = summaryDataLeaves [['meanSugar']] [con], lwd = 2,
         col = tColours [['colour']] [t])
  
  # add panel descriptor
  #--------------------------------------------------------------------------------------
  #text (x = as.POSIXct ('2019-04-07'),
  #     y = 7.5, cex = 2, pos = 4, 
  #     labels = ifelse (t == 1, 'control', ifelse (t == 4, 'compressed', 'chilled')))
  
  # add critical dates
  #--------------------------------------------------------------------------------------
  res <- criticalDates (group = 5, asDate = FALSE)
  
}
dev.off ()

# plot the 2019 leaf starch concentration data by treatment
#----------------------------------------------------------------------------------------
png (filename = './fig/stemStarchConcentrationByTreatmentExp2019.png', width = 700, height = 300)
layout (matrix (1:2, nrow = 1, byrow = TRUE))
for (t in c (1, 5)) {
  if (t == 1) {
    par (mar = c (3, 5, 1, 1))
  } else {
    par (mar = c (3, 1, 1, 1))
  }
  con <- summaryDataLeaves [['treatment']] == t &
    !is.na (summaryDataLeaves [['meanSugar']])
  plot (x = summaryDataLeaves [['DateOfSampleCollection']] [con],
        y = summaryDataLeaves [['meanStarch']] [con], lwd = 2,
        typ = 'l', xlab = '', 
        ylab = ifelse (t == 1,'leaf starch concentration (% dry weight)',''), las = 1,
        xlim = c (as_datetime ('2019-04-15'), as_datetime ('2019-10-01')), ylim = c (0, 3.5), 
        col = 'white', axes = FALSE)
  axis (side = 1, at = c (as_datetime ('2019-05-01'),
                          as_datetime ('2019-06-01'), as_datetime ('2019-07-01'),
                          as_datetime ('2019-08-01'), as_datetime ('2019-09-01'),
                          as_datetime ('2019-10-01'), as_datetime ('2019-11-01')),
        labels = c ('May','Jun','Jul','Aug','Sep','Oct','Nov'))
  if (t == 1) {
    axis (side = 2, at  = seq (0, 3.5, by = 0.5), las = 1)
  } else {
    axis (side = 2, at  = seq (0, 3.5, by = 0.5), las = 1, labels = rep ('', 8))
  }
  polygon (x = c (summaryDataLeaves [['DateOfSampleCollection']] [con], 
                  rev (summaryDataLeaves [['DateOfSampleCollection']] [con])),
           y = c (summaryDataLeaves [['meanStarch']] [con] - summaryDataLeaves [['seStarch']] [con], 
                  rev (summaryDataLeaves [['meanStarch']] [con] + summaryDataLeaves [['seStarch']] [con])),
           col = addOpacity (tColours [['colour']] [t], 0.3), lty = 0)
  lines (x = summaryDataLeaves [['DateOfSampleCollection']] [con],
         y = summaryDataLeaves [['meanStarch']] [con], lwd = 2,
         col = tColours [['colour']] [t])
  
  # add panel descriptor
  #--------------------------------------------------------------------------------------
  #text (x = as.POSIXct ('2019-04-07'),
  #     y = 3.2, cex = 2, pos = 4, 
  #      labels = ifelse (t == 1, 'control', ifelse (t == 4, 'compressed', 'chilled')))
  
  # add critical dates
  #--------------------------------------------------------------------------------------
  res <- criticalDates (group = 5, asDate = FALSE)
  
}
dev.off ()

# plot the 2019 root sugar concentration data by tree
#----------------------------------------------------------------------------------------
layout (matrix (1:8, nrow = 2, byrow = TRUE))
for (t in c (1,3,5,8,2,4,6,7)) {
  # plot below treatment
  #--------------------------------------------------------------------------------------
  con <- rootData2019 [['treeID']] == t 
  plot (x = rootData2019 [['DateOfSampleCollection']] [con],
        y = rootData2019 [['ConcentrationSugarPerDW']] [con], typ = 'l',
        xlab = 'date', ylab = 'sugar concentration (%DW)', las = 1,
        xlim = c (as_datetime ('2019-04-01'), as_datetime ('2019-10-01')), ylim = c (0, 6), 
        lty = 1, lwd = 2,
        col = tColours [['colour']] [unique (rootData2019 [['treatment']] [con])])
  
  # add a tree panel identifier
  #--------------------------------------------------------------------------------------
  text (x = as_datetime ('2019-05-01'), y = 5.5, labels = t, cex = 2)
  
  # add critical dates
  #--------------------------------------------------------------------------------------
  res <- criticalDates (group = unique (rootData2019 [['treatment']] [con]), asDate = FALSE)
  
}

# plot the 2019 root starch concentration data by tree
#----------------------------------------------------------------------------------------
layout (matrix (1:8, nrow = 2, byrow = TRUE))
for (t in c (1,3,5,8,2,4,6,7)) {
  # plot below treatment
  #--------------------------------------------------------------------------------------
  con <- rootData2019 [['treeID']] == t 
  plot (x = rootData2019 [['DateOfSampleCollection']] [con],
        y = rootData2019 [['ConcentrationStarchPerDW']] [con], typ = 'l',
        xlab = 'date', ylab = 'starch concentration (%DW)', las = 1,
        xlim = c (as_datetime ('2019-04-15'), as_datetime ('2019-10-01')), ylim = c (0, 6), 
        lty = 1, lwd = 2,
        col = tColours [['colour']] [unique (rootData2019 [['treatment']] [con])])
  
  # add a tree panel identifier
  #--------------------------------------------------------------------------------------
  text (x = as_datetime ('2019-05-01'), y = 5.5, labels = t, cex = 2)
  
  # add critical dates
  #--------------------------------------------------------------------------------------
  res <- criticalDates (group = unique (rootData2019 [['treatment']] [con]), asDate = FALSE)
  
}

# summarise the data by treatment and sampling height
#----------------------------------------------------------------------------------------
summaryDataRoot <- rootData2019 %>% 
  group_by (DateOfSampleCollection, treatment) %>% 
  summarise (meanSugar = mean (ConcentrationSugarPerDW), 
             sdSugar   = sd (ConcentrationSugarPerDW), 
             seSugar   = se (ConcentrationSugarPerDW),
             meanStarch = mean (ConcentrationStarchPerDW), 
             sdStarch   = sd (ConcentrationStarchPerDW), 
             seStarch   = se (ConcentrationStarchPerDW),
             .groups = 'keep')

# plot the 2019 root sugar concentration data by treatment
#----------------------------------------------------------------------------------------
png (filename = './fig/rootSugarConcentrationByTreatmentExp2019.png', width = 700, height = 300)
layout (matrix (1:2, nrow = 1, byrow = TRUE))
for (t in c (1, 5)) {
  if (t == 1) {
    par (mar = c (3, 5, 1, 1))
  } else {
    par (mar = c (3, 1, 1, 1))
  }
  con <- summaryDataRoot [['treatment']] == t &
    !is.na (summaryDataRoot [['meanSugar']])
  plot (x = summaryDataRoot [['DateOfSampleCollection']] [con],
        y = summaryDataRoot [['meanSugar']] [con], lwd = 2,
        typ = 'l', xlab = '', 
        ylab = ifelse (t == 1,'root sugar concentration (% dry weight)',''), las = 1,
        xlim = c (as_datetime ('2019-04-15'), as_datetime ('2019-10-01')), ylim = c (0, 6), 
        col = 'white', axes = FALSE)
  axis (side = 1, at = c (as_datetime ('2019-05-01'),
                          as_datetime ('2019-06-01'), as_datetime ('2019-07-01'),
                          as_datetime ('2019-08-01'), as_datetime ('2019-09-01'),
                          as_datetime ('2019-10-01'), as_datetime ('2019-11-01')),
        labels = c ('May','Jun','Jul','Aug','Sep','Oct','Nov'))
  if (t == 1) {
    axis (side = 2, at  = 0:6, las = 1)
  } else {
    axis (side = 2, at  = 0:6, las = 1, labels = rep ('', 7))
  }
  polygon (x = c (summaryDataRoot [['DateOfSampleCollection']] [con], 
                  rev (summaryDataRoot [['DateOfSampleCollection']] [con])),
           y = c (summaryDataRoot [['meanSugar']] [con] - summaryDataRoot [['seSugar']] [con], 
                  rev (summaryDataRoot [['meanSugar']] [con] + summaryDataRoot [['seSugar']] [con])),
           col = addOpacity (tColours [['colour']] [t], 0.3), lty = 0)
  lines (x = summaryDataRoot [['DateOfSampleCollection']] [con],
         y = summaryDataRoot [['meanSugar']] [con], lwd = 2,
         col = tColours [['colour']] [t])
  
  # add panel descriptor
  #--------------------------------------------------------------------------------------
  #text (x = as.POSIXct ('2019-04-07'),
  #      y = 5.5, cex = 2, pos = 4, 
  #      labels = ifelse (t == 1, 'control', ifelse (t == 4, 'compressed', 'chilled')))
  
  # add critical dates
  #--------------------------------------------------------------------------------------
  res <- criticalDates (group = 5, asDate = FALSE)
  
}
dev.off ()

# plot the 2019 root starch concentration data by treatment
#----------------------------------------------------------------------------------------
png (filename = './fig/rootStarchConcentrationByTreatmentExp2019.png', width = 700, height = 300)
layout (matrix (1:2, nrow = 1, byrow = TRUE))
for (t in c (1, 5)) {
  if (t == 1) {
    par (mar = c (3, 5, 1, 1))
  } else {
    par (mar = c (3, 1, 1, 1))
  }
  con <- summaryDataRoot [['treatment']] == t &
    !is.na (summaryDataRoot [['meanSugar']])
  plot (x = summaryDataRoot [['DateOfSampleCollection']] [con],
        y = summaryDataRoot [['meanStarch']] [con], lwd = 2,
        typ = 'l', xlab = '', 
        ylab = ifelse (t == 1,'root starch concentration (% dry weight)',''), las = 1,
        xlim = c (as_datetime ('2019-04-15'), as_datetime ('2019-10-01')), ylim = c (0, 4.5), 
        col = 'white')
  polygon (x = c (summaryDataRoot [['DateOfSampleCollection']] [con], 
                  rev (summaryDataRoot [['DateOfSampleCollection']] [con])),
           y = c (summaryDataRoot [['meanStarch']] [con] - summaryDataRoot [['seStarch']] [con], 
                  rev (summaryDataRoot [['meanStarch']] [con] + summaryDataRoot [['seStarch']] [con])),
           col = addOpacity (tColours [['colour']] [t], 0.3), lty = 0)
  lines (x = summaryDataRoot [['DateOfSampleCollection']] [con],
         y = summaryDataRoot [['meanStarch']] [con], lwd = 2,
         col = tColours [['colour']] [t])
  
  # add panel descriptor
  #--------------------------------------------------------------------------------------
  #text (x = as.POSIXct ('2019-04-07'),
  #      y = 4.3, cex = 2, pos = 4, 
  #      labels = ifelse (t == 1, 'control', ifelse (t == 4, 'compressed', 'chilled')))
  
  # add critical dates
  #--------------------------------------------------------------------------------------
  res <- criticalDates (group = 5, asDate = FALSE)
  
}
dev.off ()

#========================================================================================
