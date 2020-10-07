#========================================================================================
# Script to read corrected sap flow files including sap flow velocity (mm s-1) and sap 
# flux (cm3 h-1)
#----------------------------------------------------------------------------------------

# Load dependencies
#----------------------------------------------------------------------------------------
library ('tidyverse')

# Read files for each tree and add tree ID and treatment (1 = control; 5 = chilled)
#----------------------------------------------------------------------------------------
columns <-  cols ('datetime'   = col_datetime (),
                  'record'      = col_integer (),
                  'u.battery'      = col_double (),
                  't.panel'     = col_double (),
                  't.initial.1' = col_double (),
                  't.initial.2' = col_double (),
                  't.initial.3' = col_double (),
                  't.initial.4' = col_double (),
                  't.initial.5' = col_double (),
                  't.initial.6' = col_double (),
                  't.60.1'      = col_double (),
                  't.60.2'      = col_double (),
                  't.60.3'      = col_double (),
                  't.60.4'      = col_double (),
                  't.60.5'      = col_double (),
                  't.60.6'      = col_double (),
                  'v.sap.1'     = col_double (),
                  'v.sap.2'     = col_double (),
                  'v.sap.3'     = col_double ())
data19p1 <- read_csv (file = './data/Exp2019_19p1_sapflow.csv', col_types = columns) %>% 
  add_column (tree = 1, treatment = 1)
data19p2 <- read_csv (file = './data/Exp2019_19p2_sapflow.csv', col_types = columns) %>% 
  add_column (tree = 2, treatment = 5)
data19p3 <- read_csv (file = './data/Exp2019_19p3_sapflow.csv', col_types = columns) %>% 
  add_column (tree = 3, treatment = 1)
data19p4 <- read_csv (file = './data/Exp2019_19p4_sapflow.csv', col_types = columns)  %>% 
  add_column (tree = 4, treatment = 5)
data19p5 <- read_csv (file = './data/Exp2019_19p5_sapflow.csv', col_types = columns) %>% 
  add_column (tree = 5, treatment = 1)
data19p6 <- read_csv (file = './data/Exp2019_19p6_sapflow.csv', col_types = columns) %>% 
  add_column (tree = 6, treatment = 5)
data19p7 <- read_csv (file = './data/Exp2019_19p7_sapflow.csv', col_types = columns) %>% 
  add_column (tree = 7, treatment = 5)
data19p8 <- read_csv (file = './data/Exp2019_19p8_sapflow.csv', col_types = columns) %>% 
  add_column (tree = 8, treatment = 1)

# Wrangle data to have long format
#----------------------------------------------------------------------------------------
sapFlowData <- rbind (data19p1, data19p2, data19p3, data19p4, data19p5, data19p6, 
                      data19p7, data19p8)

# check for low data logger battery voltage and sort them out
#----------------------------------------------------------------------------------------
sapFlowData <- sapFlowData %>% filter (u.battery > 9.0)

# Clean up
#----------------------------------------------------------------------------------------
rm (data19p1, data19p2, data19p3, data19p4, data19p5, data19p6, data19p7, data19p8,
    columns)
#========================================================================================
