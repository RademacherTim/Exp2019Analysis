#========================================================================================
# script to read the leaf and branch water potential data for the 2019 chilling 
# experiment on the red maples at Harvard Forest.
#----------------------------------------------------------------------------------------

# load dependencies
#----------------------------------------------------------------------------------------
library ('tidyverse')
library ('lubridate')

# read water potential data file
#----------------------------------------------------------------------------------------
phi <- read_csv (file = './data/waterPotentialMeasurementsExp2019.csv', 
                 col_types = cols ())

#========================================================================================
