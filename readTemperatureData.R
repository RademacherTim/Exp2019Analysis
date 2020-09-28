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

# average temperature data over one hour intervals
#----------------------------------------------------------------------------------------
tempData <- tempData %>% 
  group_by (datetime = cut (datetime, breaks = '1 hour')) %>% 
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
#========================================================================================
