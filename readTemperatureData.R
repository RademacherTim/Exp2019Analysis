#========================================================================================
# script to read the temperature data from the project chill shed and the phloem of all 
# trees at 1.0 and 2.0 m.
#----------------------------------------------------------------------------------------

# load dependencies
#----------------------------------------------------------------------------------------
library ('tidyverse')
library ('lubridate')

# read file
#----------------------------------------------------------------------------------------
tempData <- read_csv (file = './data/temperatureData_HF_Exp2019.csv', 
                      col_types = cols ())

# average temperature data over 15-minute intervals
#----------------------------------------------------------------------------------------
tempData <- tempData  %>% 
  group_by (datetime = cut (datetime, breaks = '15 min')) %>% 
  summarise (u.battery      = mean (u.battery,  na.rm = TRUE),
             t.panel        = mean (t.panel,    na.rm = TRUE),
             t.oak.1p5m     = mean (t.oak.1p5m, na.rm = TRUE),
             t.acer.02.2p0m = mean (t.acer.02.2p0m , na.rm = TRUE),
             t.acer.02.1p0m = mean (t.acer.02.1p0m , na.rm = TRUE),
             t.acer.04.2p0m = mean (t.acer.04.2p0m , na.rm = TRUE),
             t.acer.04.1p0m = mean (t.acer.04.1p0m , na.rm = TRUE),
             t.acer.06.2p0m = mean (t.acer.06.2p0m , na.rm = TRUE),
             t.acer.06.1p0m = mean (t.acer.06.1p0m , na.rm = TRUE),
             t.acer.07.2p0m = mean (t.acer.07.2p0m , na.rm = TRUE),
             t.acer.07.1p0m = mean (t.acer.07.1p0m , na.rm = TRUE),
             t.acer.01.2p0m = mean (t.acer.01.2p0m , na.rm = TRUE),
             t.acer.01.1p0m = mean (t.acer.01.1p0m , na.rm = TRUE),
             t.acer.03.2p0m = mean (t.acer.03.2p0m , na.rm = TRUE),
             t.acer.03.1p0m = mean (t.acer.03.1p0m , na.rm = TRUE),
             t.acer.05.2p0m = mean (t.acer.05.2p0m , na.rm = TRUE),
             t.acer.05.1p0m = mean (t.acer.05.1p0m , na.rm = TRUE),
             t.acer.08.2p0m = mean (t.acer.08.2p0m , na.rm = TRUE),
             t.acer.08.1p0m = mean (t.acer.08.1p0m , na.rm = TRUE),
             t.misc1 = mean (t.misc1 , na.rm = TRUE),
             t.misc2 = mean (t.misc2 , na.rm = TRUE))

# convert datetime back from factor to datetime
#----------------------------------------------------------------------------------------
tempData [['datetime']] <- as_datetime (tempData [['datetime']])

# check how often the datalogger battery voltage dropped below 11.0 watts at some point
#----------------------------------------------------------------------------------------
res <- sum (tempData [['u.battery']] <= 9.5, na.rm = TRUE)
tempData <- tempData %>% filter (u.battery > 9.5 | is.na (u.battery)) 

# source temperature data from sapflow sensors
#----------------------------------------------------------------------------------------
source ('readSapFlowData.R')

# select only relevant columns in the sapflow temperature data (i.e., t.initial.1 and 
# t.initial.4, which are the up- and downstream thermistors closest to the bark) 
#----------------------------------------------------------------------------------------
addTempData <- sapFlowData %>% 
  select (datetime, tree, t.initial.1, t.initial.4) %>% rowwise () %>% 
  mutate (t.1p5m = mean (c (t.initial.1, t.initial.4), na.rm = TRUE)) %>%
  ungroup %>% select (-t.initial.1, -t.initial.4) %>%
  filter (datetime >= min (tempData [['datetime']], na.rm = TRUE), 
          datetime <= max (tempData [['datetime']], na.rm = TRUE))
rm (sapFlowData)

# reconvert datetime to a datetime
#----------------------------------------------------------------------------------------
addTempData [['datetime']] <- addTempData [['datetime']] %>%
  floor_date (unit = '15 mins')

# average temperature data over 15-minute intervals
#----------------------------------------------------------------------------------------
addTempData <- addTempData %>%
  group_by (tree, datetime = cut (datetime, breaks = '15 min')) %>% 
  summarise (t.1p5m = mean (t.1p5m, na.rm = TRUE)) %>% ungroup %>%
  mutate (datetime = as_datetime (datetime))

# wrangle data for compatibility with temperature data in wide format
#----------------------------------------------------------------------------------------
addTempData <- 
  pivot_wider (data = addTempData, id_cols = c (datetime, tree),
               names_from = tree, values_from = t.1p5m,
               names_prefix = 't.acer.0') 
colnames (addTempData) [2:9] <- paste0 (colnames (addTempData) [2:9], '.1p5m')

# read Fisher station temperature data from Harvard Forest Data Archive and add it to the 
# tibble
#----------------------------------------------------------------------------------------
LOCAL <- TRUE
if (LOCAL) {
  tmp <- read_csv (file = './data/hf001-10-15min-m-airt-only.csv',
                   col_types = cols ())
} else {
  fileURL <- url ('https://harvardforest.fas.harvard.edu/data/p00/hf001/hf001-10-15min-m.csv')
  tmp <- read_csv (file = fileURL, col_types = cols (
    f.airt = col_character (),
    f.prec = col_character (),
    f.rh   = col_character (),
    f.dewp = col_character (),
    f.slrr = col_character (),
    f.parr = col_character (),
    f.bar  = col_character (),
    f.wspd = col_character (),
    f.wres = col_character (),
    f.wdir = col_character (),
    f.wdev = col_character (),
    f.gspd = col_character (),
    f.s10t = col_character ())
  ) %>% 
    select (datetime, airt) %>% 
    filter (datetime >= min (tempData [['datetime']], na.rm = TRUE), 
            datetime <= max (tempData [['datetime']], na.rm = TRUE)) 
  # Save a local copy 
  #--------------------------------------------------------------------------------------
  write_csv (tmp, './data/hf001-10-15min-m-airt-only.csv')
  LOCAL <- TRUE
}

# add the 15 minutes air temperature data, and sapflow sensor derived stem temperatures
#----------------------------------------------------------------------------------------
tempData <- right_join (addTempData, tempData, by = 'datetime', all.x = TRUE) %>% 
  right_join (tmp, by = 'datetime') %>% rename (t.air.1p5m = airt)

# get rid of the data after the 3rd of November, when measurements stopped 
#----------------------------------------------------------------------------------------
tempData <- tempData %>% filter (datetime < as_datetime ('2019-11-03'))

# select only relevant temperature variables
#----------------------------------------------------------------------------------------
tempData <- tempData %>% select (-u.battery, -t.panel, -t.misc1, -t.misc2)

# clean up 
#----------------------------------------------------------------------------------------
rm (tmp, addTempData, LOCAL, res)
if (exists ('fileURL')) rm (fileURL)
#========================================================================================
