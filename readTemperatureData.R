#========================================================================================
# Script to read the temperature data from the project chill shed and the phloem of all 
# trees at 1.0 and 2.0 m.
#----------------------------------------------------------------------------------------

# load dependencies
#----------------------------------------------------------------------------------------
library ('tidyverse')
library ('lubridate')

# read file
#----------------------------------------------------------------------------------------
header <- strsplit (readLines ('./data/Exp2019_TemperatureTable.dat', n = 2), ',')
header <- substr (header [[2]], 2, nchar (header [[2]])-1)
tempData <- read_csv (file = './data/Exp2019_TemperatureTable.dat', skip = 4, 
                      col_types = cols (), col_names = header)
