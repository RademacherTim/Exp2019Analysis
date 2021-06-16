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
  
  # plot image with cropped area highlighted
  #--------------------------------------------------------------------------------------
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
  
  # crop image to region of interest for 2018 and 2019 ring
  #--------------------------------------------------------------------------------------
  imgROI2018 <- imager::imrotate (img, angle = data [['img.rotation']] [i]) %>%
    imager::imsub (x < data [['xMax2018']] [i],
                   x > data [['xMin2018']] [i],
                   y < data [['yMax2018']] [i],
                   y > data [['yMin2018']] [i])
  imgROI2018.g <- grayscale (imgROI2018)# %>% isoblur (10)
  plot (imgROI2018.g)
  imgROI2019 <- imager::imrotate (img, angle = data [['img.rotation']] [i]) %>%
    imager::imsub (x < data [['xMax2019']] [i],
                   x > data [['xMin2019']] [i],
                   y < data [['yMax2019']] [i], 
                   y > data [['yMin2019']] [i])
  imgROI2019.g <- grayscale (imgROI2019) #%>% isoblur (10)
  plot (imgROI2019.g)
  
  #imgCrop.g <- grayscale (imgCrop) %>% isoblur (10)
  #plot (imgCrop.g)
  #thrImg <- imgCrop.g %>% threshold (thr = 0.8) %>% plot () # TR I could go several thresholds and choose the one before there is a big jump in "cell-wal area", which would correspond to when vessels are included 
  #perCWA <- sum (thrImg) / (dim (thrImg) [1] * dim (thrImg) [2]) * 100
  
  # Need to label the vessels to count how many there are in the image
} # end file loop 
#========================================================================================