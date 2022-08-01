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
if (!exists ('fitacis')) library ('plantecophys')
#if (!exists ('')) library ('photosynthesis')
#if (!exists ('')) library ('lmtest')
#if (!exists ('')) library ('MASS')
if (!exists ('lmer')) library ('lme4')
if (!exists ('cAIC')) library ('cAIC4')

# read the raw photosynthesis data
#----------------------------------------------------------------------------------------
photoData <- read_csv (file = '/media/tim/dataDisk/PlantGrowth/data/photosynthesis/instantaneous_photosynthesis_HF_Exp2019.csv',
                       col_types = cols ()) %>%
  mutate (datetime = as.POSIXct (paste (date, time), format = '%Y-%m-%d %H:%M')) %>%
  select (-date, -comments) %>%
  rowwise %>% 
  mutate (photosynthetic.rate = mean (c (photosynthetic.rate.1, photosynthetic.rate.2, 
                                         photosynthetic.rate.3, photosynthetic.rate.4, 
                                         photosynthetic.rate.5), na.rm = TRUE),
          se.photosynthetic.rate = se (c (photosynthetic.rate.1, photosynthetic.rate.2, 
                                          photosynthetic.rate.3, photosynthetic.rate.4, 
                                          photosynthetic.rate.5)),
          tree = factor (tree),
          treatment = factor (treatment),
          position = factor (position))

# plot instantaneous photosynthetic rates of chilled versus control trees
#----------------------------------------------------------------------------------------
png (filename = './fig/Exp2019instantaneousPhotosynthesis.png', width = 300, height = 400)
con <- photoData [['treatment']] == 'control' & photoData [['position']] == 'top' & photoData [['tree.id']] != 1908
plot (x = jitter (rep (0.8, sum (con)), 2),
      y = photoData [['photosynthetic.rate']] [con],
      xlim = c (0.6, 1.4), ylim = c (0, 11), axes = FALSE,
      pch = 19, col = addOpacity (tColours [['colour']] [1], 0.6),
      xlab = '', 
      ylab = expression (paste ('Instantaneous photosynthetic rate (',mu, mol,' ', m^-2,' ', s^-1,')', sep = ' ')))
points (x = 0.8,
        y = mean (photoData [['photosynthetic.rate']] [con], na.rm = TRUE),
        col = tColours [['colour']] [1], 
        bg = addOpacity (tColours [['colour']] [1], 0.6),
        pch = 21, cex = 4, lwd = 3)
con <- photoData [['treatment']] == 'control' & photoData [['position']] == 'bottom'
points (x = jitter (rep (0.8, sum (con)), 2),
        y = photoData [['photosynthetic.rate']] [con],
        pch = 21, col = tColours [['colour']] [1])
points (x = 0.8,
        y = mean (photoData [['photosynthetic.rate']] [con], na.rm = TRUE),
        col = tColours [['colour']] [1], bg = addOpacity ('white', 0.6),
        pch = 21, cex = 4, lwd = 3)
con <- photoData [['treatment']] == 'chilled' & photoData [['position']] == 'top'
points (x = jitter (rep (1.2, sum (con)), 1),
        y = photoData [['photosynthetic.rate']] [con],
        col = tColours [['colour']] [4], 
        bg = addOpacity (tColours [['colour']] [4], 0.6), pch = 23)
points (x = 1.2,
        y = mean (photoData [['photosynthetic.rate']] [con], na.rm = TRUE),
        col = tColours [['colour']] [4], 
        bg = addOpacity (tColours [['colour']] [4], 0.6), 
        pch = 23, cex = 4, lwd = 3)
con <- photoData [['treatment']] == 'chilled' & photoData [['position']] == 'bottom'
points (x = jitter (rep (1.2, sum (con)), 1),
        y = photoData [['photosynthetic.rate']] [con],
        col = tColours [['colour']] [4], pch = 23)
points (x = 1.2,
        y = mean (photoData [['photosynthetic.rate']] [con], na.rm = TRUE),
        col = tColours [['colour']] [4], bg = addOpacity ('white', 0.6), 
        pch = 23, cex = 4, lwd = 3)
axis (side = 1, at = c (0.8, 1.2), labels = c ('Control','Chilled'))
axis (side = 2, at = seq (0, 10, by = 2), las = 1)
dev.off ()

# read A/Ci curves 
#----------------------------------------------------------------------------------------
ACiData <- 
  read_csv (file = '/media/tim/dataDisk/PlantGrowth/data/photosynthesis/photosynthesis_ACi_curves_HF_Exp2019.csv',
            col_types = cols ()) %>%
  filter (study == 'Exp') %>%
  mutate (datetime = as.POSIXct (paste (date, time), format = '%Y-%m-%d %H:%M:%S')) %>%
  select (-date, -time, -position, -study, -FTime) %>%
  mutate (treatment = ifelse (tree %in% c (1901, 1903, 1905, 1908), 1, 5)) %>%
  rename (Photo = photosynthetic.rate, # change names for processing using plantecophys package
          Ci = ci,
          Tleaf = t.leaf,
          PARi = par.i,
          Ci_Pa = ci.Pa,
          Tleaf_K = t.air.k) %>% 
  filter (!(tree == 1903 & leaf %in% c ('II','K','LL','MM')))

# plot A/Ci data 
#----------------------------------------------------------------------------------------
png (filename = './fig/Exp2019ACiCurves.png', width = 600, height = 400)
par (mar = c (5, 5, 1, 1))
plot (x = ACiData [['Ci']] [ACiData [['treatment']] == 1],
      y = ACiData [['Photo']] [ACiData [['treatment']] == 1],
      xlim = c (0, 1800), ylim = c (0, 30), axes = FALSE, col = 'white', pch = 19,
      xlab = 'Ci', ylab = expression (paste ('Assimilation rate (',mu, mol,' ', m^-2,' ', s^-1,')', sep = ' ')))
axis (side = 1, at = seq (0, 1500, 500))
axis (side = 2, at = seq (0, 30, 6), las = 1)
points (x = ACiData [['Ci']] [ACiData [['treatment']] == 5],
        y = ACiData [['Photo']] [ACiData [['treatment']] == 5],
        pch = 23, col = tColours [['colour']] [5], bg = addOpacity (tColours [['colour']] [5], 0.6))
points (x = ACiData [['Ci']] [ACiData [['treatment']] == 1],
        y = ACiData [['Photo']] [ACiData [['treatment']] == 1],
        pch = 19, col = addOpacity (tColours [['colour']] [1], 0.6))

# fit A/Ci curves
#----------------------------------------------------------------------------------------
fit_control <- plantecophys::fitaci (data = ACiData [ACiData [['treatment']] == 1, ],
                                     fitmethod = "bilinear")
fit_chilled <- plantecophys::fitaci (data = ACiData [ACiData [['treatment']] == 5, ],
                                     fitmethod = "bilinear")
plot (fit_control, add = TRUE, linecol = tColours [['colour']] [1:3], 
      #linecol_highlight = tColours [['colour']] [3], 
      what = 'model', lwd = 2)
plot (fit_chilled, add = TRUE, linecol = tColours [['colour']] [4:6], 
      #linecol_highlight = tColours [['colour']] [5:7], 
      what = 'model', lty = 2, lwd = 2)
dev.off ()

# read data for light response curves
#----------------------------------------------------------------------------------------
lightResponseData <- 
  read_csv (file = '/media/tim/dataDisk/PlantGrowth/data/photosynthesis/photosynthesis_light_response_curves_HF_Exp2019.csv',
            col_types = cols ()) %>%
  filter (study == 'Exp') %>%
  mutate (datetime = as.POSIXct (paste (date, time), format = '%Y-%m-%d %H:%M:%S')) %>%
  select (-date, -time, -position, -study, -FTime) %>%
  mutate (treatment = ifelse (tree %in% c (1901, 1903, 1905, 1908), 1, 5)) %>% 
  filter (!(tree == 1905 & leaf == 'L'),
          !(tree == 1907 & leaf == 'L'))
  
# plot light response curves 
#----------------------------------------------------------------------------------------
png (filename = './fig/Exp2019LightResponseCurves.png', width = 600, height = 400)
par (mar = c (5, 5, 1, 1))
plot (x = lightResponseData [['PARi']] [lightResponseData [['treatment']] == 1],
      y = lightResponseData [['photosynthetic.rate']] [lightResponseData [['treatment']] == 1],
      xlim = c (0, 1800), ylim = c (0, 13), axes = FALSE, col = 'white', pch = 19,
      xlab = 'Incident PAR', ylab = expression (paste ('Assimilation rate (',mu, mol,' ', m^-2,' ', s^-1,')', sep = ' ')))
axis (side = 1, at = seq (0, 1500, 500))
axis (side = 2, at = seq (0, 12, 3), las = 1)
points (x = lightResponseData [['PARi']] [lightResponseData [['treatment']] == 5],
        y = lightResponseData [['photosynthetic.rate']] [lightResponseData [['treatment']] == 5],
        pch = 23, col = addOpacity (tColours [['colour']] [5], 0.6))
points (x = lightResponseData [['PARi']] [lightResponseData [['treatment']] == 1],
        y = lightResponseData [['photosynthetic.rate']] [lightResponseData [['treatment']] == 1],
        pch = 19, col = addOpacity (tColours [['colour']] [1], 0.6))
dev.off ()

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
        col = tColours [['colour']] [1], pch = 1)
con <- fluorescenceData [['treatment']] == 'chilled' & fluorescenceData [['position']] == 'top'
points (x = fluorescenceData [['FvOverFm']] [con],
        y = fluorescenceData [['FvOverFm.dark']] [con],
        col = tColours [['colour']] [4], bg = tColours [['colour']] [4], pch = 23)
con <- fluorescenceData [['treatment']] == 'chilled' & fluorescenceData [['position']] == 'bottom'
points (x = fluorescenceData [['FvOverFm']] [con],
        y = fluorescenceData [['FvOverFm.dark']] [con],
        col = tColours [['colour']] [4], pch = 5)
dev.off ()

# plot dark-adapted FvOverFm
#----------------------------------------------------------------------------------------
par (mfrow = c (1, 1))
par (mar = c (5, 5, 1, 1))
con <- fluorescenceData [['treatment']] == 'control' & fluorescenceData [['position']] == 'top'
plot (x = jitter (rep (0.8, sum (con)), 2),
      y = fluorescenceData [['FvOverFm.dark']] [con], las = 1,
      xlab = 'Fv/Fm', ylab = 'Fv/Fm (dark adapted)', axes = FALSE,
      col = tColours [['colour']] [1], pch = 19, xlim = c (0.6, 1.4), ylim = c (0.63, 0.85))
axis (side = 1, at = c (0.8, 1.2), labels = c ('Control','Chilled'))
axis (side = 2, at = seq (0.65, 0.85, by = 0.10), las = 1)
con <- fluorescenceData [['treatment']] == 'control' & fluorescenceData [['position']] == 'bottom'
points (x = jitter (rep (0.8, sum (con)), 2),
        y = fluorescenceData [['FvOverFm.dark']] [con],
        col = tColours [['colour']] [1], pch = 1)
con <- fluorescenceData [['treatment']] == 'chilled' & fluorescenceData [['position']] == 'top'
points (x = jitter (rep (1.2, sum (con)), 1),
        y = fluorescenceData [['FvOverFm.dark']] [con],
        col = tColours [['colour']] [5], bg = tColours [['colour']] [5], pch = 23)
con <- fluorescenceData [['treatment']] == 'chilled' & fluorescenceData [['position']] == 'bottom'
points (x = jitter (rep (1.2, sum (con)), 1),
        y = fluorescenceData [['FvOverFm.dark']] [con],
        col = tColours [['colour']] [5],  pch = 23)

# test whether there is a difference between chilled and control trees
#----------------------------------------------------------------------------------------
tmpData <- fluorescenceData %>% select (tree.id, leaf.id, treatment, position, FvOverFm.dark) %>%
  mutate (position = ifelse (position == 'bottom', 'bottom', 'top')) %>%
  mutate (tree = factor (tree.id),
          leaf = factor (leaf.id),
          position = factor (position),
          treatment = factor (treatment)) 
mod <-  lmer (formula = FvOverFm.dark ~ (1 | tree), 
              REML = TRUE, data = tmpData)
summary (mod)  
cAIC (mod)
mod1 <-  lmer (formula = FvOverFm.dark ~ (1 | tree) + position, 
               REML = TRUE, data = tmpData)
summary (mod1)  
cAIC (mod1)
mod2 <-  lmer (formula = FvOverFm.dark ~ (1 | tree)+ treatment, 
              REML = TRUE, data = tmpData)
summary (mod2)  
cAIC (mod2)
mod3 <-  lmer (formula = FvOverFm.dark ~ (1 | tree) + position + treatment, 
               REML = TRUE, data = tmpData)
summary (mod3)  
cAIC (mod3)
mod4 <-  lmer (formula = FvOverFm.dark ~ (1 | tree) + position * treatment, 
              REML = TRUE, data = tmpData)
summary (mod4)  
cAIC (mod4)
#========================================================================================
