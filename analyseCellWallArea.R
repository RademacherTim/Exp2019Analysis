#========================================================================================
# Script to analyse the micrcorre images using a thresholdind procedure to estimate 
# cell-wall area
#----------------------------------------------------------------------------------------

# load dependencies
#----------------------------------------------------------------------------------------
if (!existsFunction ('grayscale'))   library ('imager')
if (!existsFunction ('as_date'))     library ('lubridate')
if (!existsFunction ('tibble'))      library ('tidyverse')
if (!existsFunction ('lmer'))        library ('lme4')
if (!existsFunction ('cAIC'))        library ('cAIC4')

# read in the ring width data that Patrick prepared 
#----------------------------------------------------------------------------------------
if (!exists ('xyloData')) source ('readXylogenesisData.R')

# read ploting functions and colour schemes
#----------------------------------------------------------------------------------------
if (!exists ('tColours')) source ('plotingFunctions.R')

# define image directory
#----------------------------------------------------------------------------------------
iDir <- '/media/tim/dataDisk/PlantGrowth/data/microcores/'

# list all bright image files
#----------------------------------------------------------------------------------------
fileNames <- list.files (path = paste0 (iDir,'images/Exp2019/JPG/'), 
                         pattern = '_Bright.jpg')
len <- length (fileNames)
decomposedNames <- unlist (strsplit (fileNames, split = '_')) 
tree.id <- as.numeric (substr (decomposedNames [seq (1, len*5, by = 5)], 2, 3)) + 1900
sample.height <- as.numeric (decomposedNames [seq (2, len*5, by = 5)])
sample.height [which (sample.height == 1)] <- 0.5
sample.height [which (sample.height == 2)] <- 1.5
sample.height [which (sample.height == 3)] <- 2.5
sample.date <- as_date (decomposedNames [seq (3, len * 5, by = 5)])
files <- tibble (image.name = fileNames,
                 tree.id = tree.id,
                 sample.height = sample.height,
                 sample.date = sample.date) %>% 
  filter (sample.date == as_date ('2019-09-25')) # TR - Start by only looking at the end of the season slides

# read file with region of interest boundaries for the 2018 and 2019 ring  
#----------------------------------------------------------------------------------------
ROIbounds <- read_csv (paste0 (iDir,'Exp2019BoundariesForRegionOfInterest.csv'), 
                       col_types = cols (
                         image.name   = col_character (),
                         xDim         = col_number (),
                         yDim         = col_number (),
                         img.rotation = col_number (),
                         xMin2018   = col_number (),
                         xMax2018   = col_number (),
                         yMin2018   = col_number (),
                         yMax2018   = col_number (),
                         xMin2019   = col_number (),
                         xMax2019   = col_number (),
                         yMin2019   = col_number (),
                         yMax2019   = col_number ()), 
                       na = 'NA')

# compare the files lists and bind them together
#----------------------------------------------------------------------------------------
data <- files %>% dplyr::left_join (ROIbounds, by = 'image.name'); rm (ROIbounds)

# add the area of the 2018 and 2019 region of interest (micrometer^2)
#----------------------------------------------------------------------------------------
res <- 1.5 # resolution of image scans (pixels per micrometer) 
data <- data %>% 
  mutate (A2018ROI = ((xMax2018 - xMin2018) * (yMax2018 - yMin2018)) / res^2,
          A2019ROI = ((xMax2019 - xMin2019) * (yMax2019 - yMin2019)) / res^2) 

# Add the ring width from the end of the season sample for the 2018 and 2019 ring to 
# estimate biomass differencese
#----------------------------------------------------------------------------------------
data <- data %>% 
  mutate (RW2018 = xyloData %>% filter (sample.date == '2019-09-25', year == 2018) %>% 
            select (ring.width) %>% unlist (),
          RW2019 = xyloData %>% filter (sample.date == '2019-09-25', year == 2019) %>% 
            select (ring.width) %>% unlist ())

# add column with percentage CWA for the 2018 and 2019 ring region of interest 
#----------------------------------------------------------------------------------------
data <- add_column (data, 
                    perCWA2018 = NA, 
                    perCWA2019 = NA,
                    nVessels2018 = NA,
                    nVessels2019 = NA,
                    mVesselR2018 = NA,
                    mVesselR2019 = NA,
                    sdVesselR2018 = NA,
                    sdVesselR2019 = NA,
                    rhoCW2018 = NA,
                    rhoCW2019 = NA)

# ser grayscale threshold to determine whether a pixel is cell-wall or lumen
#----------------------------------------------------------------------------------------
threshold <- 0.8
rhoCW <- 1.459 # Cell-wall density (g cm-3) for red maple according to Table 1 of 
               # Kellogg et al. (1975), which comes from Kellogg & Wangaard (1969) 

# loop over image names
#----------------------------------------------------------------------------------------
for (i in 1:dim (data) [1]) {
  
  # get image name 
  #--------------------------------------------------------------------------------------
  iName <- data [['image.name']] [i]
  
  # load image 
  #--------------------------------------------------------------------------------------
  img <- imager::load.image (paste0 (iDir,'images/Exp2019/JPG/',iName))
  #img <- magick::image_read (path = paste0 (iDir,'images/Exp2019/JPG/',iName))
  
  # Whether to plot the image with the regions of interest
  #--------------------------------------------------------------------------------------
  PLOT <- FALSE

  # plot image with cropped area highlighted
  #--------------------------------------------------------------------------------------
  if (PLOT) {
    plot (img)
    rect (xleft   = data [['xMin2018']] [i],
          xright  = data [['xMax2018']] [i],
          ybottom = data [['yMin2018']] [i],
          ytop    = data [['yMax2018']] [i],
          border  = tColours [['colour']] [3], lwd = 2)
    rect (xleft   = data [['xMin2019']] [i],
          xright  = data [['xMax2019']] [i],
          ybottom = data [['yMin2019']] [i],
          ytop    = data [['yMax2019']] [i],
          border  = tColours [['colour']] [6], lwd = 2)
  }

  # Whether to plot the regions of interest
  #--------------------------------------------------------------------------------------
  PLOT <- FALSE

  # crop image to region of interest for 2018 ring
  #--------------------------------------------------------------------------------------
  if (!is.na (data [['xMax2018']] [i])) {
    imgROI2018 <- imager::imrotate (img, angle = data [['img.rotation']] [i]) %>%
      imager::imsub (x < data [['xMax2018']] [i],
                     x > data [['xMin2018']] [i],
                     y < data [['yMax2018']] [i],
                     y > data [['yMin2018']] [i])
    imgROI2018.g <- grayscale (imgROI2018) 
    if (PLOT) plot (imgROI2018.g)
  } else {
    imgROI2018.g <- NA
  }

  # crop image to region of interest for 2019 ring
  #--------------------------------------------------------------------------------------
  if (!is.na (data [['xMax2019']] [i])) {
    imgROI2019 <- imager::imrotate (img, angle = data [['img.rotation']] [i]) %>%
      imager::imsub (x < data [['xMax2019']] [i],
                     x > data [['xMin2019']] [i],
                     y < data [['yMax2019']] [i], 
                     y > data [['yMin2019']] [i])
    imgROI2019.g <- grayscale (imgROI2019)
    if (PLOT) plot (imgROI2019.g)
  } else {
    imgROI2019.g <- NA
  }
  
  # estimate percentage of cell-wall area in image using simple threshold
  #--------------------------------------------------------------------------------------
  if (!is.na (imgROI2018.g [1])) {
    thrImg2018 <- imgROI2018.g %>% threshold (thr = threshold) #%>% 
      #plot (main = paste (i, data [['tree.id']] [i], data [['sample.height']] [i]))  
    data [['perCWA2018']] [i] <- sum (thrImg2018) / (dim (thrImg2018) [1] * dim (thrImg2018) [2]) * 100
  }
  if (!is.na (imgROI2019.g [1])) {
    thrImg2019 <- imgROI2019.g %>% threshold (thr = threshold) #%>% 
      #plot (main = paste (i, data [['tree.id']] [i], data [['sample.height']] [i]))
    data [['perCWA2019']] [i] <- sum (thrImg2019) / (dim (thrImg2019) [1] * dim (thrImg2019) [2]) * 100
  }
  
  # estimate  mean structural biomass per micrometer to scale to sturctural biomass of 
  # the ring (% structure per micrometer of ring)
  #--------------------------------------------------------------------------------------
  data [['rhoCW2018']] [i] <- mean (colMeans (thrImg2018) [, 1, 1]) / res * rhoCW
  data [['rhoCW2019']] [i] <- mean (colMeans (thrImg2019) [, 1, 1]) / res * rhoCW
  
  #--------------------------------------------------------------------------------------
  # De-noising with isoblur reduces the amount of cell-wall area substantially, but 
  # results remain highly correlated with the same threshold, suggesting that they are 
  # robust.
  # Choosing a different threshold of 0.7 reduces the correlation slightly, but it still
  # the overall results still hold up robustly.
  
  
  # De-noise image to make blob detection easier
  #--------------------------------------------------------------------------------------
  if (!is.na (imgROI2018.g [1])) imgROI2018.d <- imgROI2018.g %>% isoblur (12)
  if (!is.na (imgROI2019.g [1])) imgROI2019.d <- imgROI2019.g %>% isoblur (12)
  
  # apply hessian filter and label individual blobs below threshold
  #--------------------------------------------------------------------------------------
  dt2018 <- imhessian (imgROI2018.d) %$% { xx*yy - xy^2 } %>% 
    threshold (thr = 0) %>% 
    label ()
  dt2019 <- imhessian (imgROI2019.d) %$% { xx*yy - xy^2 } %>% 
    threshold (thr = 0) %>% 
    label ()
  
  # identify the location of large (i.e., 500 micrometer^2) blobs only
  #--------------------------------------------------------------------------------------
  vessels2018 <- as.data.frame (dt2018) %>% 
    dplyr::group_by (value) %>% 
    dplyr::filter (n () > 500*res^2) %>% # Only in blobs larger than 500 micrometer^2
    dplyr::summarise (mx = mean (x), my = mean (y), LA = n () / res ^2, r = sqrt (LA)) %>%
    dplyr:: filter (r > 0 & r < 100) # filter out blob with negative radius or a radius larger than 100 micrometers 
  vessels2019 <- as.data.frame (dt2019) %>% 
    dplyr::group_by (value) %>% 
    dplyr::filter (n () > 500*res^2) %>% # Only in blobs larger than 500 micrometer^2
    dplyr::summarise (mx = mean (x), my = mean (y), LA = n () / res ^2, r = sqrt (LA)) %>%
    dplyr:: filter (r > 0 & r < 100) # filter out blob with negative radius or a radius larger than 100 micrometers
  
  # get mean brightness of 20 by 20 pixel value for each blob
  #--------------------------------------------------------------------------------------
  vessels2018 <- vessels2018 %>% add_column (mb = NA)
  for (r in 1:dim (vessels2018) [1]){
    w <- 20 # window width
    x1 <- round (vessels2018 [['mx']] [r])
    y1 <- round (vessels2018 [['my']] [r])
    if (x1 < w) w <- x1
    if (y1 < w) w <- y1
    if (x1+w > dim (imgROI2018.d) [1]) w <- dim (imgROI2018.d) [1] - x1
    if (y1+w > dim (imgROI2018.d) [2]) w <- dim (imgROI2018.d) [2] - y1
    vessels2018 [['mb']] [r] <- mean (imgROI2018.d [(x1-w):(x1+w),(y1-w):(y1+w),,])  
  }
  vessels2019 <- vessels2019 %>% add_column (mb = NA)
  for (r in 1:dim (vessels2019) [1]){
    w <- 20 # window width
    x1 <- round (vessels2019 [['mx']] [r])
    y1 <- round (vessels2019 [['my']] [r])
    if (x1 < w) w <- x1
    if (y1 < w) w <- y1
    if (x1+w > dim (imgROI2019.d) [1]) w <- dim (imgROI2019.d) [1] - x1
    if (y1+w > dim (imgROI2019.d) [2]) w <- dim (imgROI2019.d) [2] - y1
    vessels2019 [['mb']] [r] <- mean (imgROI2019.d [(x1-w):(x1+w),(y1-w):(y1+w),,])  
  }
  # filter out dark blobs
  #--------------------------------------------------------------------------------------
  vessels2018 <- vessels2018 %>% filter (mb > 0.8)
  vessels2019 <- vessels2019 %>% filter (mb > 0.8)
  
  # Whether to plot the identified vessels
  #--------------------------------------------------------------------------------------
  PLOT <- FALSE
  
  # plot the identified blobs 
  #--------------------------------------------------------------------------------------
  if (PLOT) {
    plot (imgROI2018.d, 
          main = paste (data [['tree.id']] [i], data [['sample.height']] [i],'2018'))
    points (vessels2018 [['mx']], vessels2018 [['my']], col ='red')
    plot (imgROI2019.d, 
          main = paste (data [['tree.id']] [i], data [['sample.height']] [i],'2019'))
    points (vessels2019 [['mx']], vessels2019 [['my']], col ='red')
  }
  
  # add estimated number of vessels in the regions of interest to data tibble
  #--------------------------------------------------------------------------------------
  data [['nVessels2018']] [i] <- dim (vessels2018) [1]
  data [['nVessels2019']] [i] <- dim (vessels2019) [1]
  data [['mVesselR2018']] [i] <- mean (vessels2018 [['r']], na.rm = TRUE)
  data [['mVesselR2019']] [i] <- mean (vessels2019 [['r']], na.rm = TRUE)
  data [['sdVesselR2018']] [i] <- sd (vessels2018 [['r']], na.rm = TRUE)
  data [['sdVesselR2019']] [i] <- sd (vessels2019 [['r']], na.rm = TRUE)
  
} # end file loop 

# get value of grams of carbon in each ring by multiplying ring width and mean density
# this is g of carbon per increment core with an area of 1 cm2
#----------------------------------------------------------------------------------------
data <- data %>% mutate (mass2018 = rhoCW2018 * RW2018 / 1000,
                         mass2019 = rhoCW2019 * RW2019 / 1000)

# add treatment to the tibble
#----------------------------------------------------------------------------------------
data <- data %>% 
  mutate (treatment = ifelse (tree.id %in% c (1901, 1903, 1905, 1908), 'control','chilled'))

# add vessel density (number of vessels per area, ; n per mm^2)
#----------------------------------------------------------------------------------------
data <- data %>%
  mutate (rhoV2018 = nVessels2018 / (A2018ROI / 1e6),
          rhoV2019 = nVessels2019 / (A2019ROI / 1e6))

# add a scaled column of region of interest as a rough proxy for growth
#----------------------------------------------------------------------------------------
data <-  data %>%
  mutate (A2018ROI.s = scale (A2018ROI),
          A2019ROI.s = scale (A2019ROI)) %>%
  mutate (minA2018ROI.s = min (A2018ROI.s, na.rm = TRUE),
          minA2019ROI.s = min (A2019ROI.s, na.rm = TRUE)) %>%
  mutate (A2018ROI.sc = A2018ROI.s - minA2018ROI.s,
          A2019ROI.sc = A2019ROI.s - minA2019ROI.s)

# plot 2018 versus 2019 cell-wall area estimate coloured by treatment
#----------------------------------------------------------------------------------------
par (mar = c (5, 5, 1, 1))
con <- data [['treatment']] == 'control' & data [['sample.height']] == 0.5
plot (x = data [['perCWA2018']] [con],
      y = data [['perCWA2019']] [con],
      xlim = c (10, 55), ylim = c (10, 55), las = 1,
      xlab = 'Percentage cell-wall area in 2018 ring (%)', 
      ylab = 'Percentage cell-wall area in 2019 ring (%)', pch = 19, 
      col = tColours [['colour']] [tColours [['treatment']] == 'control'])
con <- data [['treatment']] == 'chilled' & data [['sample.height']] == 0.5
points (x = data [['perCWA2018']] [con],
        y = data [['perCWA2019']] [con], pch = 23, lwd = 2,
        col = tColours [['colour']] [tColours [['treatment']] == 'chilled'])
con <- data [['treatment']] == 'control' & data [['sample.height']] == 1.5
points (x = data [['perCWA2018']] [con],
        y = data [['perCWA2019']] [con], pch = 19, 
        col = tColours [['colour']] [tColours [['treatment']] == 'control'])
con <- data [['treatment']] == 'chilled' & data [['sample.height']] == 1.5
points (x = data [['perCWA2018']] [con],
        y = data [['perCWA2019']] [con], pch = 23, lwd = 2,
        col = tColours [['colour']] [tColours [['treatment']] == 'chilled'])
con <- data [['treatment']] == 'control' & data [['sample.height']] == 2.5
points (x = data [['perCWA2018']] [con],
        y = data [['perCWA2019']] [con], pch = 19, 
        col = tColours [['colour']] [tColours [['treatment']] == 'control'])
con <- data [['treatment']] == 'chilled' & data [['sample.height']] == 2.5
points (x = data [['perCWA2018']] [con],
        y = data [['perCWA2019']] [con], pch = 23, lwd = 2,
        col = tColours [['colour']] [tColours [['treatment']] == 'chilled'])
con <- data [['treatment']] == 'control' & data [['sample.height']] == 4.0
points (x = data [['perCWA2018']] [con],
        y = data [['perCWA2019']] [con], pch = 19, 
        col = tColours [['colour']] [tColours [['treatment']] == 'control'])
con <- data [['treatment']] == 'chilled' & data [['sample.height']] == 4.0
points (x = data [['perCWA2018']] [con],
        y = data [['perCWA2019']] [con], pch = 23, lwd = 2,
        col = tColours [['colour']] [tColours [['treatment']] == 'chilled'])

# plot percentage cell-wall area against size of the region of interest
#----------------------------------------------------------------------------------------
con <- data [['treatment']] == 'control'
plot (x = data [['A2018ROI']] [con] / 1e6,
      y = data [['perCWA2018']] [con],
      xlim = c (0, 10), 
      ylim = c (10, 60), las = 1,
      pch = 19, 
      xlab = expression (paste ('ROI area (',mm^2,')', sep = '')), 
      ylab = 'Percentage cell-wall area (%)',
      col = tColours [['colour']] [tColours [['treatment']] == 'control'])
points (x = data [['A2019ROI']] [con] / 1e6,
        y = data [['perCWA2019']] [con],
        pch = 1,
        col = tColours [['colour']] [tColours [['treatment']] == 'control'])
con <- data [['treatment']] == 'chilled'
points (x = data [['A2018ROI']] [con] / 1e6,
        y = data [['perCWA2018']] [con],
        pch = 23, lwd = 2,
        col = tColours [['colour']] [tColours [['treatment']] == 'chilled'],
        bg = tColours [['colour']] [tColours [['treatment']] == 'chilled'])
points (x = data [['A2019ROI']] [con] / 1e6,
        y = data [['perCWA2019']] [con],
        pch = 23, lwd = 2,
        col = tColours [['colour']] [tColours [['treatment']] == 'chilled'])

# plot vessel density
#----------------------------------------------------------------------------------------
par (mar = c (5, 5, 1, 1))
con <- data [['treatment']] == 'control' & data [['sample.height']] == 0.5
plot (x = data [['rhoV2018']] [con],
      y = data [['rhoV2019']] [con],
      xlim = c (0, 120), 
      ylim = c (0, 120), las = 1,
      pch = 19, cex = 2*data [['A2019ROI.sc']] [con],
      xlab = expression (paste ('2018 vessel density (n ',mm^-2,')', sep = '')), 
      ylab = expression (paste ('2019 vessel density (n ',mm^-2,')', sep = '')),
      col = tColours [['colour']] [tColours [['treatment']] == 'control'])
con <- data [['treatment']] == 'control' & data [['sample.height']] == 1.5
points (x = data [['rhoV2018']] [con],
        y = data [['rhoV2019']] [con],
        pch = 19, cex = 2*data [['A2019ROI.sc']], 
        col = tColours [['colour']] [tColours [['treatment']] == 'control'])
con <- data [['treatment']] == 'control' & data [['sample.height']] == 2.5
points (x = data [['rhoV2018']] [con],
        y = data [['rhoV2019']] [con],
        pch = 19, cex = 2*data [['A2019ROI.sc']],
        col = tColours [['colour']] [tColours [['treatment']] == 'control'])
con <- data [['treatment']] == 'control' & data [['sample.height']] == 4.0
points (x = data [['rhoV2018']] [con],
        y = data [['rhoV2019']] [con],
        pch = 19, cex = 2*data [['A2019ROI.sc']],
        col = tColours [['colour']] [tColours [['treatment']] == 'control'])

# Add chilled trees
con <- data [['treatment']] == 'chilled' & data [['sample.height']] == 0.5
points (x = data [['rhoV2018']] [con],
        y = data [['rhoV2019']] [con],
        pch = 23, lwd = 2, cex = 2*data [['A2019ROI.sc']],
        col = tColours [['colour']] [tColours [['treatment']] == 'chilled'])
con <- data [['treatment']] == 'chilled' & data [['sample.height']] == 1.5
points (x = data [['rhoV2018']] [con],
        y = data [['rhoV2019']] [con],
        pch = 23, lwd = 2, cex = 2*data [['A2019ROI.sc']],
        col = tColours [['colour']] [tColours [['treatment']] == 'chilled'])
con <- data [['treatment']] == 'chilled' & data [['sample.height']] == 2.5
points (x = data [['rhoV2018']] [con],
        y = data [['rhoV2019']] [con],
        pch = 23, lwd = 2, cex = 2*data [['A2019ROI.sc']],
        col = tColours [['colour']] [tColours [['treatment']] == 'chilled'])
con <- data [['treatment']] == 'chilled' & data [['sample.height']] == 4.0
points (x = data [['rhoV2018']] [con],
        y = data [['rhoV2019']] [con],
        pch = 23, lwd = 2, cex = 2*data [['A2019ROI.sc']],
        col = tColours [['colour']] [tColours [['treatment']] == 'chilled'])
# Especially higher up the stem more growth 


# plot vessel size 
#----------------------------------------------------------------------------------------
par (mar = c (5, 5, 1, 1))
con <- data [['treatment']] == 'control' & data [['sample.height']] == 0.5
plot (x = data [['mVesselR2018']] [con],
      y = data [['mVesselR2019']] [con],
      xlim = c (20, 80), 
      ylim = c (20, 80), las = 1,
      pch = 19, cex = 2*data [['A2019ROI.sc']] [con],
      xlab = expression (paste ('2018 vessel density (n ',mm^-2,')', sep = '')), 
      ylab = expression (paste ('2019 vessel density (n ',mm^-2,')', sep = '')),
      col = tColours [['colour']] [tColours [['treatment']] == 'control'])
con <- data [['treatment']] == 'control' & data [['sample.height']] == 1.5
points (x = data [['mVesselR2018']] [con],
        y = data [['mVesselR2019']] [con],
        pch = 19, cex = 2*data [['A2019ROI.sc']], 
        col = tColours [['colour']] [tColours [['treatment']] == 'control'])
con <- data [['treatment']] == 'control' & data [['sample.height']] == 2.5
points (x = data [['mVesselR2018']] [con],
        y = data [['mVesselR2019']] [con],
        pch = 19, cex = 2*data [['A2019ROI.sc']],
        col = tColours [['colour']] [tColours [['treatment']] == 'control'])
con <- data [['treatment']] == 'control' & data [['sample.height']] == 4.0
points (x = data [['mVesselR2018']] [con],
        y = data [['mVesselR2019']] [con],
        pch = 19, cex = 2*data [['A2019ROI.sc']],
        col = tColours [['colour']] [tColours [['treatment']] == 'control'])

# Add chilled trees
con <- data [['treatment']] == 'chilled' & data [['sample.height']] == 0.5
points (x = data [['mVesselR2018']] [con],
        y = data [['mVesselR2019']] [con],
        pch = 23, lwd = 2, cex = 2*data [['A2019ROI.sc']],
        col = tColours [['colour']] [tColours [['treatment']] == 'chilled'])
con <- data [['treatment']] == 'chilled' & data [['sample.height']] == 1.5
points (x = data [['mVesselR2018']] [con],
        y = data [['mVesselR2019']] [con],
        pch = 23, lwd = 2, cex = 2*data [['A2019ROI.sc']],
        col = tColours [['colour']] [tColours [['treatment']] == 'chilled'])
con <- data [['treatment']] == 'chilled' & data [['sample.height']] == 2.5
points (x = data [['mVesselR2018']] [con],
        y = data [['mVesselR2019']] [con],
        pch = 23, lwd = 2, cex = 2*data [['A2019ROI.sc']],
        col = tColours [['colour']] [tColours [['treatment']] == 'chilled'])
con <- data [['treatment']] == 'chilled' & data [['sample.height']] == 4.0
points (x = data [['mVesselR2018']] [con],
        y = data [['mVesselR2019']] [con],
        pch = 23, lwd = 2, cex = 2*data [['A2019ROI.sc']],
        col = tColours [['colour']] [tColours [['treatment']] == 'chilled'])

# Convert data into long format
#----------------------------------------------------------------------------------------
tmpData <- data %>% 
  select (tree.id, treatment, sample.height, sample.date, perCWA2018, perCWA2019) %>%
  pivot_longer (cols = 5:6,
                names_to = 'year',
                names_prefix = 'perCWA',
                values_to = 'perCWA')
tmpData1 <- data %>% 
    select (tree.id, treatment, sample.height, sample.date, rhoV2018, rhoV2019) %>%
    pivot_longer (cols = 5:6,
                  names_to = 'year',
                  names_prefix = 'rhoV',
                  values_to = 'rhoV')
tmpData2 <- data %>% 
  select (tree.id, treatment, sample.height, sample.date, mVesselR2018, mVesselR2019) %>%
  pivot_longer (cols = 5:6,
                names_to = 'year',
                names_prefix = 'mVesselR',
                values_to = 'rV')
tmpData3 <- data %>% 
  select (tree.id, treatment, sample.height, sample.date, rhoCW2018, rhoCW2019) %>%
  pivot_longer (cols = 5:6,
                names_to = 'year',
                names_prefix = 'rhoCW',
                values_to = 'rhoCW')
tmpData4 <- data %>% 
  select (tree.id, treatment, sample.height, sample.date, mass2018, mass2019) %>%
  pivot_longer (cols = 5:6,
                names_to = 'year',
                names_prefix = 'mass',
                values_to = 'mass')
tmpData5 <- data %>% 
  select (tree.id, treatment, sample.height, sample.date, RW2018, RW2019) %>%
  pivot_longer (cols = 5:6,
                names_to = 'year',
                names_prefix = 'RW',
                values_to = 'RW')
tmpData <- tmpData %>% 
  dplyr::left_join (tmpData1, by = c ('tree.id','treatment','sample.height','sample.date','year')) %>%
  dplyr::left_join (tmpData2, by = c ('tree.id','treatment','sample.height','sample.date','year')) %>%
  dplyr::left_join (tmpData3, by = c ('tree.id','treatment','sample.height','sample.date','year')) %>%
  dplyr::left_join (tmpData4, by = c ('tree.id','treatment','sample.height','sample.date','year')) %>%
  dplyr::left_join (tmpData5, by = c ('tree.id','treatment','sample.height','sample.date','year')) %>%
  mutate (tree.id = factor (tree.id),
          treatment = factor (treatment),
          sampleHeight = sample.height,
          sample.height = factor (sample.height),
          year = factor (year))
rm (tmpData1, tmpData2, tmpData3, tmpData4, tmpData5)

# Check whether there was a treatment effect on vessel density
#----------------------------------------------------------------------------------------
mod0 <- lmer (rhoV ~ (1 | tree.id), 
              data = tmpData,
              REML = TRUE)
summary (mod0)
cAIC (mod0) # conditional AIC = 568.28
mod1 <- lmer (rhoV ~ sample.height + (1 | tree.id), 
              data = tmpData,
              REML = TRUE)
summary (mod1)
cAIC (mod1) # conditional AIC = 572.60
mod2 <- lmer (rhoV ~ year + (1 | tree.id), 
              data = tmpData,
              REML = TRUE)
summary (mod2)
cAIC (mod2) # conditional AIC = 559.38
mod3 <- lmer (rhoV ~ year:treatment + (1 | tree.id), 
              data = tmpData,
              REML = TRUE)
summary (mod3)
cAIC (mod3) # conditional AIC = 560.59
# mod3: Chilled tree showed substantially lower vessel density in 2019 compared to 2018 
# and control trees
mod4 <- lmer (rhoV ~ year:treatment:sample.height + (1 | tree.id), 
              data = tmpData,
              REML = TRUE)
summary (mod4)
cAIC (mod4) # conditional AIC = 568.45
mod5 <- lmer (rhoV ~ year + year:treatment:sample.height + (1 | tree.id), 
              data = tmpData,
              REML = TRUE)
summary (mod5)
cAIC (mod5) # conditional AIC = 568.45
# mod5: When broken down by sample height chilled trees have substantially lower vessel 
#       density in 2019 at 4.0 m, slightly lower at 2.5 m relative to control, but no 
#       clear changes at 1.5 m. At 0.5 m, the vessel density increase in chilled trees. 
#       1.5 m most closely follows the control group.

# Check whether there was a treatment effect on vessel size
#----------------------------------------------------------------------------------------
mod0 <- lmer (rV ~ (1 | tree.id), 
              data = tmpData,
              REML = TRUE)
summary (mod0)
cAIC (mod0) # conditional AIC = 355.94
mod1 <- lmer (rV ~ sample.height + (1 | tree.id), 
              data = tmpData,
              REML = TRUE)
summary (mod1)
cAIC (mod1) # conditional AIC = 353.53
mod2 <- lmer (rV ~ year + (1 | tree.id), 
              data = tmpData,
              REML = TRUE)
summary (mod2)
cAIC (mod2) # conditional AIC = 338.16
mod3 <- lmer (rV ~ year:treatment + (1 | tree.id), 
              data = tmpData,
              REML = TRUE)
summary (mod3)
cAIC (mod3) # conditional AIC = 340.36
mod4 <- lmer (rV ~ year:treatment:sample.height + (1 | tree.id), 
              data = tmpData,
              REML = TRUE)
summary (mod4)
cAIC (mod4) # conditional AIC = 344.28
mod5 <- lmer (rV ~ year + year:treatment:sample.height + (1 | tree.id), 
              data = tmpData,
              REML = TRUE)
summary (mod5)
cAIC (mod5) # conditional AIC = 344.28
# mod5: Overall the effects on vessel size appear to be marginal! Which was visually 
#       confirmed.

# Check whether there was a treatment effect on percentage cell-wall area
#----------------------------------------------------------------------------------------
mod0 <- lmer (perCWA ~ (1 | tree.id), 
              data = tmpData,
              REML = TRUE)
summary (mod0)
cAIC (mod0) # conditional AIC = 417.50
mod1 <- lmer (perCWA ~ sample.height + (1 | tree.id), 
              data = tmpData,
              REML = TRUE)
summary (mod1)
cAIC (mod1) # conditional AIC = 421.28
mod2 <- lmer (perCWA ~ year + (1 | tree.id), 
              data = tmpData,
              REML = TRUE)
summary (mod2)
cAIC (mod2) # conditional AIC = 397.30
mod3 <- lmer (perCWA ~ year:treatment + (1 | tree.id), 
              data = tmpData,
              REML = TRUE)
summary (mod3)
cAIC (mod3) # conditional AIC = 399.07
mod4 <- lmer (perCWA ~ year:treatment:sample.height + (1 | tree.id), 
              data = tmpData,
              REML = TRUE)
summary (mod4)
cAIC (mod4) # conditional AIC = 403.05
mod5 <- lmer (perCWA ~ year + year:treatment:sample.height + (1 | tree.id), 
              data = tmpData,
              REML = TRUE)
summary (mod5)
cAIC (mod5) # conditional AIC = 403.05
# No clear differences in percentage cell-wall area 

# plot difference in mass (mass is the mass per square centimetre along the radial file)
#----------------------------------------------------------------------------------------
boxplot (rhoCW ~ sample.height + treatment, data = tmpData %>% filter (year == 2019))

par (mar = c (5,5,1,1))
con <- tmpData [['year']] == 2019 & tmpData [['treatment']] == 'control'
plot (x = tmpData [['sampleHeight']] [con], 
      y = tmpData [['mass']] [con] / 1000,
      las = 1, pch = 19, col = tColours [['colour']] [1],
      xlim = c (0, 4.5), ylim = c (0, 3),
      xlab = 'Sample height (m)', 
      ylab = expression (paste ('Mass (g ',cm^2,' along the radial file)',sep = ''))) 
con <- tmpData [['year']] == 2019 & tmpData [['treatment']] == 'chilled'
points (x = tmpData [['sampleHeight']] [con], 
        y = tmpData [['mass']] [con] / 1000,
        pch = 23, col = tColours [['colour']] [4], lwd = 2)

imageData <- tmpData
rm (data, tmpData, files, mod0, mod1, vessels2018, vessels2019, con, decomposedNames, 
    dt2018, dt2019, fileNames, i, iDir, img, imgROI2018, imgROI2018.d, imgROI2018.g,
    imgROI2019, imgROI2019.d, imgROI2019.g, iName, len, originalDir, PLOT, r, res, 
    rhoCW, sample.date, sample.height, t, threshold, thrImg2018, thrImg2019, tree.id,
    w, x1, y1, mod0, mod1, mod2, mod3, mod4, mod5)
#========================================================================================
