#========================================================================================
# This script reads in the ring width data processed by Patrick Fonti from excel 
# spreadsheets to analyse volume growth over time from the microsections of the 2019 
# chilling experiment at Harvard Forest on red maple (Acer rubrum). 
#----------------------------------------------------------------------------------------

# load dependencies 
#----------------------------------------------------------------------------------------
if (!exists ('read_excel'))   library ('readxl')
if (!exists ('pivot_longer')) library ('tidyr')
if (!exists ('tibble'))       library ('tibble')
if (!exists ('filter'))       library ('dplyr')
if (!exists ('as_date'))      library ('lubridate')

# get original working directory
#----------------------------------------------------------------------------------------
originalDir <- getwd ()

# set working directory
#----------------------------------------------------------------------------------------
setwd ('/media/tim/dataDisk/PlantGrowth/data/microcores/woodAnatomy/Exp2019/')

# read one spredsheets per tree with ring width measurements for all eight trees
#----------------------------------------------------------------------------------------
for (t in 1:8) {
  assign (paste0 ('A0',t), read_excel (path = paste0 ('A0',t,'.xlsx'), 
                                       sheet = paste0 ('A0',t,'.new'), na = 'NA', 
                                       col_types = rep ('numeric', ifelse (t == 2, 61, 61))) %>%
    pivot_longer (cols = 2:ifelse (t == 2, 61, 61), 
                  names_to = c ('tree.id','sample.height', 'sample.date'), 
                  names_sep = '_', values_to = 'ring.width'))
}
xyloData <- rbind (A01, A02, A03, A04, A05, A06, A07, A08)
rm (A01, A02, A03, A04, A05, A06, A07, A08)

# drop all rows with no ring width
#----------------------------------------------------------------------------------------
xyloData <- xyloData %>% filter (!is.na (ring.width))

# convert tree.id, add treatment column to the tibble, and convert sample.date to date type
#----------------------------------------------------------------------------------------
xyloData <- xyloData %>% 
  mutate (tree.id = as.numeric (substr (tree.id, 3, 3)) + 1900,
          treatment = ifelse (tree.id %in% c (1901, 1903, 1905, 1908), 'control','chilled'),
          sample.date = as_date (sample.date),
          sample.doy  = yday (sample.date),
          sample.height = ifelse (sample.height == 1, 0.5, 
                                  ifelse (sample.height == 2, 1.5, 
                                          ifelse (sample.height == 3, 2.5, 4.0)))) %>%
  rename (year = Year)

# change sample.doy for sample taken in 2020 to 365, as the 2019 ring was already fully formed 
#----------------------------------------------------------------------------------------
xyloData [['sample.doy']] [xyloData [['sample.date']] == as_date ('2020-08-04')] <- 365

# reset original working directory
#----------------------------------------------------------------------------------------
setwd (originalDir)

#========================================================================================