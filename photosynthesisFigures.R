#========================================================================================
# Script to compare photosynthesis and fluorescence measurements of chilled and control 
# trees from the chilling experiment on red maple at Harvard Forest. Photosynthesis 
# measurements were made using two different LI-6400 starting in the end of June and 
# stretching into the second week of July. Fluorescence measurements were made with the ...
# The chilling was switched of on the 10th of July 2019.
#----------------------------------------------------------------------------------------

# load dependencies
#----------------------------------------------------------------------------------------
if (!exists ('read_excel')) library ('readxl')
if (!exists ('%>%')) library ('tidyverse')

# read the raw photosynthesis data
#----------------------------------------------------------------------------------------
read_csv (file = './data/instantaneous_photosynthesis_Exp2019.csv')

# read fluorescence data
#----------------------------------------------------------------------------------------
fluorescenceData <- read_csv (file = '/media/tim/dataDisk/PlantGrowth/data/photosynthesis/leaf_fluorescence_Exp2019.csv',
                              col_types = cols (
                                study      = col_character (),
                                tree.id    = col_number (),
                                treatment  = col_character (),
                                leaf.id    = col_character (),
                                position   = col_character (),
                                Fo.1       = col_number (),
                                Fm.1       = col_number (),
                                FvOverFm.1 = col_number (),
                                Fo.2       = col_number (),
                                Fm.2       = col_number (),
                                FvOverFm.2 = col_number (),
                                Fo.3       = col_number (),
                                Fm.3       = col_number (),
                                FvOverFm.3 = col_number (),
                                Fo.4       = col_number (),
                                Fm.4       = col_number (),
                                FvOverFm.4 = col_number (),
                                Fo.5       = col_number (),
                                Fm.5       = col_number (),
                                FvOverFm.5 = col_number (),
                                Fo.dark.1  = col_number (),
                                Fm.dark.1  = col_number (),
                                FvOverFm.dark.1 = col_number (),
                                Fo.dark.2  = col_number (),
                                Fm.dark.2  = col_number (),
                                FvOverFm.dark.2 = col_number (),
                                Fo.dark.3  = col_number (),
                                Fm.dark.3  = col_number (),
                                FvOverFm.dark.3 = col_number (),
                                Fo.dark.4  = col_number (),
                                Fm.dark.4  = col_number (),
                                FvOverFm.dark.4 = col_number (),
                                Fo.dark.5  = col_number (),
                                Fm.dark.5  = col_number (),
                                FvOverFm.dark.5 = col_number (),
                                fresh.weight.g  = col_number (),
                                dry.weight.g = col_number (),
                                moisture.content = col_number (),
                                comments = col_character ()
                              )) %>%
  mutate (datetime = as.POSIXct (paste (date, time), format = '%Y-%m-%d %H:%M')) %>%
  select (-date, -time, -comments) %>%
  rowwise %>% 
  mutate (Fo       = mean (c (Fo.1, Fo.2, Fo.3, Fo.4, Fo.5), na.rm = TRUE),
          Fm       = mean (c (Fm.1, Fm.2, Fm.3, Fm.4, Fm.5), na.rm = TRUE),
          FvOverFm = mean (c (FvOverFm.1, FvOverFm.2, FvOverFm.3, FvOverFm.4, FvOverFm.5), na.rm = TRUE),
          se.Fo       = se (c (Fo.1, Fo.2, Fo.3, Fo.4, Fo.5)),
          se.Fm       = se (c (Fo.1, Fo.2, Fo.3, Fo.4, Fo.5)),
          se.FvOverFm = se (c (Fo.1, Fo.2, Fo.3, Fo.4, Fo.5)),
          Fo.dark       = mean (c (Fo.dark.1, Fo.dark.2, Fo.dark.3, Fo.dark.4, Fo.dark.5), na.dark.rm = TRUE),
          Fm.dark       = mean (c (Fm.dark.1, Fm.dark.2, Fm.dark.3, Fm.dark.4, Fm.dark.5), na.dark.rm = TRUE),
          FvOverFm.dark = mean (c (FvOverFm.dark.1, FvOverFm.dark.2, FvOverFm.dark.3, FvOverFm.dark.4, FvOverFm.dark.5), na.dark.rm = TRUE),
          se.dark.Fo       = se (c (Fo.dark.1, Fo.dark.2, Fo.dark.3, Fo.dark.4, Fo.dark.5)),
          se.dark.Fm       = se (c (Fo.dark.1, Fo.dark.2, Fo.dark.3, Fo.dark.4, Fo.dark.5)),
          se.dark.FvOverFm = se (c (Fo.dark.1, Fo.dark.2, Fo.dark.3, Fo.dark.4, Fo.dark.5)))

# Fo straight away versus dark-adapted
#----------------------------------------------------------------------------------------
png (filename = './fig/Exp2019Fo.png', width = 400, height = 400)
par (mfrow = c (1, 1))
par (mar = c (5, 5, 1, 1))
con <- fluorescenceData [['treatment']] == 'control' & fluorescenceData [['position']] == 'top'
plot (x = fluorescenceData [['Fo']] [con],
      y = fluorescenceData [['Fo.dark']] [con], las = 1,
      xlab = 'Fo', ylab = '', axes = FALSE,
      col = tColours [['colour']] [1], pch = 19, xlim = c (0.005, 0.02), ylim = c (0.005, 0.02))
abline (b = 1, a = 0, col = '#666666')
axis (side = 1, at = seq (0.00, 0.02, by = 0.005))
axis (side = 2, at = seq (0.00, 0.02, by = 0.005), las = 1)
con <- fluorescenceData [['treatment']] == 'control' & fluorescenceData [['position']] == 'bottom'
points (x = fluorescenceData [['Fo']] [con],
        y = fluorescenceData [['Fo.dark']] [con],
        col = tColours [['colour']] [1], pch = 1)
con <- fluorescenceData [['treatment']] == 'chilled' & fluorescenceData [['position']] == 'top'
points (x = fluorescenceData [['Fo']] [con],
        y = fluorescenceData [['Fo.dark']] [con],
        col = tColours [['colour']] [4], bg = tColours [['colour']] [4], pch = 23)
con <- fluorescenceData [['treatment']] == 'chilled' & fluorescenceData [['position']] == 'bottom'
points (x = fluorescenceData [['Fo']] [con],
        y = fluorescenceData [['Fo.dark']] [con],
        col = tColours [['colour']] [4], pch = 5)
mtext (side = 2, line = 3.5, text = 'Fo (dark adapted)')
dev.off ()

# Fm straight away versus dark-adapted
#----------------------------------------------------------------------------------------
png (filename = './fig/Exp2019Fm.png', width = 400, height = 400)
par (mfrow = c (1, 1))
par (mar = c (5, 5, 1, 1))
con <- fluorescenceData [['treatment']] == 'control'  & fluorescenceData [['position']] == 'top'
plot (x = fluorescenceData [['Fm']] [con],
      y = fluorescenceData [['Fm.dark']] [con], las = 1,
      xlab = 'Fm', ylab = 'Fm (dark adapted)', axes = FALSE,
      col = tColours [['colour']] [1], pch = 19, xlim = c (0.015, 0.100), ylim = c (0.015, 0.100))
abline (b = 1, a = 0, col = '#666666')
axis (side = 1, at = seq (0.00, 0.1, by = 0.02))
axis (side = 2, at = seq (0.00, 0.1, by = 0.02), las = 1)
con <- fluorescenceData [['treatment']] == 'control' & fluorescenceData [['position']] == 'bottom'
points (x = fluorescenceData [['Fm']] [con],
        y = fluorescenceData [['Fm.dark']] [con],
        col = tColours [['colour']] [1], pch = 1)
con <- fluorescenceData [['treatment']] == 'chilled' & fluorescenceData [['position']] == 'top'
points (x = fluorescenceData [['Fm']] [con],
        y = fluorescenceData [['Fm.dark']] [con],
        col = tColours [['colour']] [4], bg = tColours [['colour']] [4], pch = 23)
con <- fluorescenceData [['treatment']] == 'chilled' & fluorescenceData [['position']] == 'bottom'
points (x = fluorescenceData [['Fm']] [con],
        y = fluorescenceData [['Fm.dark']] [con],
        col = tColours [['colour']] [4], pch = 5)
dev.off ()

# Fv/Fm straight away versus dark-adapted
#----------------------------------------------------------------------------------------
png (filename = './fig/Exp2019FvOverFm.png', width = 400, height = 400)
par (mfrow = c (1, 1))
par (mar = c (5, 5, 1, 1))
con <- fluorescenceData [['treatment']] == 'control' & fluorescenceData [['position']] == 'top'
plot (x = fluorescenceData [['FvOverFm']] [con],
      y = fluorescenceData [['FvOverFm.dark']] [con], las = 1,
      xlab = 'Fv/Fm', ylab = 'Fv/Fm (dark adapted)', axes = FALSE,
      col = tColours [['colour']] [1], pch = 19, xlim = c (0.33, 0.85), ylim = c (0.33, 0.85))
abline (b = 1, a = 0, col = '#666666')
axis (side = 1, at = seq (0.3, 0.8, by = 0.1))
axis (side = 2, at = seq (0.3, 0.8, by = 0.1), las = 1)
con <- fluorescenceData [['treatment']] == 'control' & fluorescenceData [['position']] == 'bottom'
points (x = fluorescenceData [['FvOverFm']] [con],
        y = fluorescenceData [['FvOverFm.dark']] [con],
        xlab = 'Fv/Fm', ylab = 'Fv/Fm (dark adapted)',
        col = tColours [['colour']] [1], pch = 1)
con <- fluorescenceData [['treatment']] == 'chilled' & fluorescenceData [['position']] == 'top'
points (x = fluorescenceData [['FvOverFm']] [con],
        y = fluorescenceData [['FvOverFm.dark']] [con],
        xlab = 'Fv/Fm', ylab = 'Fv/Fm (dark adapted)',
        col = tColours [['colour']] [4], bg = tColours [['colour']] [4], pch = 23)
con <- fluorescenceData [['treatment']] == 'chilled' & fluorescenceData [['position']] == 'bottom'
points (x = fluorescenceData [['FvOverFm']] [con],
        y = fluorescenceData [['FvOverFm.dark']] [con],
        xlab = 'Fv/Fm', ylab = 'Fv/Fm (dark adapted)',
        col = tColours [['colour']] [4], pch = 5)
dev.off ()
#========================================================================================
