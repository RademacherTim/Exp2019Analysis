#========================================================================================
# Script to analyse the micrcorre images using a thresholdind procedure to estimate 
# cell-wall area
#----------------------------------------------------------------------------------------

# load dependencies
#----------------------------------------------------------------------------------------
if (!existsFunction ('grayscale'))   library ('imager')
if (!existsFunction ('image_write')) library ('magick')
if (!existsFunction ('as_date'))     library ('lubridate')
if (!existsFunction ('tibble'))      library ('tidyverse')
#if (!existsFunction ('readJPEG'))  library ('jpeg')


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
  mutate (A2018ROI = ((xMax2018 - xMin2018) * (yMax2018 - yMin2018)) / res,
          A2019ROI = ((xMax2019 - xMin2019) * (yMax2019 - yMin2019)) / res) 

# add column with percentage CWA for the 2018 and 2019 ring region of interest 
#----------------------------------------------------------------------------------------
data <- add_column (data, 
                    perCWA2018 = NA, 
                    perCWA2019 = NA)

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
  # The lower the value the whiter the image in grayscale

  # Whether to plot the image with the regions of interest
  #--------------------------------------------------------------------------------------
  PLOT <- FALSE

  # plot image with cropped area highlighted
  #--------------------------------------------------------------------------------------
  if (PLOT) {
    plot (img)
    rect (xleft   = files [['xMin2018']] [i],
          xright  = files [['xMax2018']] [i],
          ybottom = files [['yMin2018']] [i],
          ytop    = files [['yMax2018']] [i],
          border  = tColours [['colour']] [3], lwd = 2)
    rect (xleft   = files [['xMin2019']] [i],
          xright  = files [['xMax2019']] [i],
          ybottom = files [['yMin2019']] [i],
          ytop    = files [['yMax2019']] [i],
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
  threshold <- 0.8
  # TR I could go several thresholds and choose the one before there is a big jump in "cell-wal area", which would correspond to when vessels are included
  
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
  
  #--------------------------------------------------------------------------------------
  # De-noising with isoblur reduces the amount of cell-wall area substantially, but 
  # results remain highly correlated with the same threshold, suggesting that they are 
  # robust.
  # Choosing a different threshold of 0.7 reduces the correlation slightly, but it still
  # the overall results still hold up robustly.
  
  
  # Need to label the vessels to count how many there are in each image
  #--------------------------------------------------------------------------------------
} # end file loop 

# add treatment to the tibble
#----------------------------------------------------------------------------------------
data <- data %>% 
  mutate (treatment = ifelse (tree.id %in% c (1901, 1903, 1905, 1908), 'control','chilled'))

# plot 2018 versus 2019 cell-wall area estimate coloured by treatment
#----------------------------------------------------------------------------------------
par (mar = c (5, 5, 1, 1))
con <- data [['treatment']] == 'control' & data [['sample.height']] == 0.5
plot (x = data [['perCWA2018']] [con],
      y = data [['perCWA2019']] [con],
      xlim = c (10, 55), ylim = c (10, 55), 
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
      xlim = c (0, 15.0), 
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

# Check whether there are statistical differences
#----------------------------------------------------------------------------------------

# Also detect the number of vessels and convert it to a vessel density for comparison
#----------------------------------------------------------------------------------------
#========================================================================================
