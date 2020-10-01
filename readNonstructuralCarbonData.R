#========================================================================================
# script to read and process nonstructural carbon data from colorimetric assays for the 
# 2019 chilling experiment on red maple at Harvard Forest.
# Processing is done using the NSCProcessR package.
#----------------------------------------------------------------------------------------

# get original working directory and switch to directory with NSCprocessR package 
#----------------------------------------------------------------------------------------
originalDir <- getwd ()
packageDir <- '/home/tim/projects/PlantGrowth/nonstructuralCarbon/' 
setwd (packageDir)

# load dependencies
#----------------------------------------------------------------------------------------
if (!existsFunction ('dplyr::filter')) library ('tidyverse')
if (!existsFunction ('NSCprocessR::processNSC')) library ('NSCprocessR')
if (!existsFunction ('lubridate::month')) library ('lubridate')
if (!existsFunction ('write.xlsx')) library ('openxlsx')

# change working directory
#----------------------------------------------------------------------------------------
setwd (originalDir)

# read data from csv files with new column names
#----------------------------------------------------------------------------------------
temp1 <- read_csv (file = './data/nonstructuralCarbonData_additionalPhloem_HF_Exp2019.csv', 
                   col_types = cols (
                     rc.lab.number             = col_character (),
                     sample.id                 = col_character (),
                     tissue                    = col_character (),
                     batch.id                  = col_integer (),
                     sample.location           = col_character (),
                     date.of.sample.collection = col_date (format = '%d-%b-%y'), 
                     date.of.sugar.analysis    = col_date (format = '%d-%b-%y'), 
                     date.of.starch.analysis   = col_date (format = '%d-%b-%y'), 
                     mass.of.empty.tube        = col_double (), 
                     mass.of.tube.and.sample   = col_double (), 
                     absorbance490.1           = col_double (),
                     absorbance490.2           = col_double (),
                     absorbance490.blank       = col_double (), 
                     absorbance525.1           = col_double (),
                     absorbance525.2           = col_double (), 
                     dilution.factor.sugar     = col_integer (),
                     volume.sugar              = col_integer (), 
                     dilution.factor.starch    = col_integer (), 
                     volume.starch             = col_integer (), 
                     comments                  = col_character () 
                   ))
temp2 <- read_csv (file = './data/nonstructuralCarbonData_additionalWood2_HF_Exp2019.csv', 
                   col_types = cols (
                     rc.lab.number             = col_character (),
                     sample.id                 = col_character (),
                     tissue                    = col_character (),
                     batch.id                  = col_integer (),
                     sample.location           = col_character (),
                     date.of.sample.collection = col_date (format = '%d-%b-%y'), 
                     date.of.sugar.analysis    = col_date (format = '%d-%b-%y'), 
                     date.of.starch.analysis   = col_date (format = '%d-%b-%y'), 
                     mass.of.empty.tube        = col_double (), 
                     mass.of.tube.and.sample   = col_double (), 
                     absorbance490.1           = col_double (),
                     absorbance490.2           = col_double (),
                     absorbance490.blank       = col_double (), 
                     absorbance525.1           = col_double (),
                     absorbance525.2           = col_double (), 
                     dilution.factor.sugar     = col_integer (),
                     volume.sugar              = col_integer (), 
                     dilution.factor.starch    = col_integer (), 
                     volume.starch             = col_integer (), 
                     comments                  = col_character () 
                   ))
temp3 <- read_csv (file = './data/nonstructuralCarbonData_additionalWood_HF_Exp2019.csv', 
                   col_types = cols (
                     rc.lab.number             = col_character (),
                     sample.id                 = col_character (),
                     tissue                    = col_character (),
                     batch.id                  = col_integer (),
                     sample.location           = col_character (),
                     date.of.sample.collection = col_date (format = '%d-%b-%y'), 
                     date.of.sugar.analysis    = col_date (format = '%d-%b-%y'), 
                     date.of.starch.analysis   = col_date (format = '%d-%b-%y'), 
                     mass.of.empty.tube        = col_double (), 
                     mass.of.tube.and.sample   = col_double (), 
                     absorbance490.1           = col_double (),
                     absorbance490.2           = col_double (),
                     absorbance490.blank       = col_double (), 
                     absorbance525.1           = col_double (),
                     absorbance525.2           = col_double (), 
                     dilution.factor.sugar     = col_integer (),
                     volume.sugar              = col_integer (), 
                     dilution.factor.starch    = col_integer (), 
                     volume.starch             = col_integer (), 
                     comments                  = col_character () 
                   ))
temp4 <- read_csv (file = './data/nonstructuralCarbonData_leaves_HF_Exp2019.csv', 
                   col_types = cols (
                     rc.lab.number             = col_character (),
                     sample.id                 = col_character (),
                     tissue                    = col_character (),
                     batch.id                  = col_integer (),
                     sample.location           = col_character (),
                     date.of.sample.collection = col_date (format = '%d-%b-%y'), 
                     date.of.sugar.analysis    = col_date (format = '%d-%b-%y'), 
                     date.of.starch.analysis   = col_date (format = '%d-%b-%y'), 
                     mass.of.empty.tube        = col_double (), 
                     mass.of.tube.and.sample   = col_double (), 
                     absorbance490.1           = col_double (),
                     absorbance490.2           = col_double (),
                     absorbance490.blank       = col_double (), 
                     absorbance525.1           = col_double (),
                     absorbance525.2           = col_double (), 
                     dilution.factor.sugar     = col_integer (),
                     volume.sugar              = col_integer (), 
                     dilution.factor.starch    = col_integer (), 
                     volume.starch             = col_integer (), 
                     comments                  = col_character () 
                   ))
temp5 <- read_csv (file = './data/nonstructuralCarbonData_phloem_HF_Exp2019.csv', 
                   col_types = cols (
                     rc.lab.number             = col_character (),
                     sample.id                 = col_character (),
                     tissue                    = col_character (),
                     batch.id                  = col_integer (),
                     sample.location           = col_character (),
                     date.of.sample.collection = col_date (format = '%d-%b-%y'), 
                     date.of.sugar.analysis    = col_date (format = '%d-%b-%y'), 
                     date.of.starch.analysis   = col_date (format = '%d-%b-%y'), 
                     mass.of.empty.tube        = col_double (), 
                     mass.of.tube.and.sample   = col_double (), 
                     absorbance490.1           = col_double (),
                     absorbance490.2           = col_double (),
                     absorbance490.blank       = col_double (), 
                     absorbance525.1           = col_double (),
                     absorbance525.2           = col_double (), 
                     dilution.factor.sugar     = col_integer (),
                     volume.sugar              = col_integer (), 
                     dilution.factor.starch    = col_integer (), 
                     volume.starch             = col_integer (), 
                     comments                  = col_character () 
                   ))
temp6 <- read_csv (file = './data/nonstructuralCarbonData_phloemLCS_HF_Exp2019.csv', 
                   col_types = cols (
                     rc.lab.number             = col_character (),
                     sample.id                 = col_character (),
                     tissue                    = col_character (),
                     batch.id                  = col_integer (),
                     sample.location           = col_character (),
                     date.of.sample.collection = col_date (format = '%d-%b-%y'), 
                     date.of.sugar.analysis    = col_date (format = '%d-%b-%y'), 
                     date.of.starch.analysis   = col_date (format = '%d-%b-%y'), 
                     mass.of.empty.tube        = col_double (), 
                     mass.of.tube.and.sample   = col_double (), 
                     absorbance490.1           = col_double (),
                     absorbance490.2           = col_double (),
                     absorbance490.blank       = col_double (), 
                     absorbance525.1           = col_double (),
                     absorbance525.2           = col_double (), 
                     dilution.factor.sugar     = col_integer (),
                     volume.sugar              = col_integer (), 
                     dilution.factor.starch    = col_integer (), 
                     volume.starch             = col_integer (), 
                     comments                  = col_character () 
                   ))
temp7 <- read_csv (file = './data/nonstructuralCarbonData_roots_HF_Exp2019.csv', 
                   col_types = cols (
                     rc.lab.number             = col_character (),
                     sample.id                 = col_character (),
                     tissue                    = col_character (),
                     batch.id                  = col_integer (),
                     sample.location           = col_character (),
                     date.of.sample.collection = col_date (format = '%d-%b-%y'), 
                     date.of.sugar.analysis    = col_date (format = '%d-%b-%y'), 
                     date.of.starch.analysis   = col_date (format = '%d-%b-%y'), 
                     mass.of.empty.tube        = col_double (), 
                     mass.of.tube.and.sample   = col_double (), 
                     absorbance490.1           = col_double (),
                     absorbance490.2           = col_double (),
                     absorbance490.blank       = col_double (), 
                     absorbance525.1           = col_double (),
                     absorbance525.2           = col_double (), 
                     dilution.factor.sugar     = col_integer (),
                     volume.sugar              = col_integer (), 
                     dilution.factor.starch    = col_integer (), 
                     volume.starch             = col_integer (), 
                     comments                  = col_character () 
                   ))
temp8 <- read_csv (file = './data/nonstructuralCarbonData_wood_HF_Exp2019.csv', 
                   col_types = cols (
                     rc.lab.number             = col_character (),
                     sample.id                 = col_character (),
                     tissue                    = col_character (),
                     batch.id                  = col_integer (),
                     sample.location           = col_character (),
                     date.of.sample.collection = col_date (format = '%d-%b-%y'), 
                     date.of.sugar.analysis    = col_date (format = '%d-%b-%y'), 
                     date.of.starch.analysis   = col_date (format = '%d-%b-%y'), 
                     mass.of.empty.tube        = col_double (), 
                     mass.of.tube.and.sample   = col_double (), 
                     absorbance490.1           = col_double (),
                     absorbance490.2           = col_double (),
                     absorbance490.blank       = col_double (), 
                     absorbance525.1           = col_double (),
                     absorbance525.2           = col_double (), 
                     dilution.factor.sugar     = col_integer (),
                     volume.sugar              = col_integer (), 
                     dilution.factor.starch    = col_integer (), 
                     volume.starch             = col_integer (), 
                     comments                  = col_character () 
                   ))
temp9 <- read_csv (file = './data/nonstructuralCarbonData_woodLCS_HF_Exp2019.csv', 
                   col_types = cols (
                     rc.lab.number             = col_character (),
                     sample.id                 = col_character (),
                     tissue                    = col_character (),
                     batch.id                  = col_integer (),
                     sample.location           = col_character (),
                     date.of.sample.collection = col_date (format = '%d-%b-%y'), 
                     date.of.sugar.analysis    = col_date (format = '%d-%b-%y'), 
                     date.of.starch.analysis   = col_date (format = '%d-%b-%y'), 
                     mass.of.empty.tube        = col_double (), 
                     mass.of.tube.and.sample   = col_double (), 
                     absorbance490.1           = col_double (),
                     absorbance490.2           = col_double (),
                     absorbance490.blank       = col_double (), 
                     absorbance525.1           = col_double (),
                     absorbance525.2           = col_double (), 
                     dilution.factor.sugar     = col_integer (),
                     volume.sugar              = col_integer (), 
                     dilution.factor.starch    = col_integer (), 
                     volume.starch             = col_integer (), 
                     comments                  = col_character () 
                   ))

# change column names to the NSCprocessR standards using CamelCasing and underscores
#----------------------------------------------------------------------------------------
colNames <- c ('RCLabNumber','SampleID','Tissue','BatchID','SampleLocation',
               'DateOfSampleCollection','DateOfSugarAnalysis','DateOfStarchAnalysis',
               'MassOfEmptyTube','MassOfTubeAndSample','Absorbance490_1',
               'Absorbance490_2','Absorbance490_Blank','Absorbance525_1',
               'Absorbance525_2','DilutionFactorSugar','VolumeSugar',
               'DilutionFactorStarch','VolumeStarch','Comments')
names (temp1) <- colNames
names (temp2) <- colNames
names (temp3) <- colNames
names (temp4) <- colNames
names (temp5) <- colNames
names (temp6) <- colNames
names (temp7) <- colNames
names (temp8) <- colNames
names (temp9) <- colNames

# write data to .xlsx format for NSCprocessR readRawNSCData () function
#----------------------------------------------------------------------------------------
openxlsx::write.xlsx (temp1, file = './data/nonstructuralCarbonData_additionalPhloem_HF_Exp2019.xlsx')
openxlsx::write.xlsx (temp2, file = './data/nonstructuralCarbonData_additionalWood2_HF_Exp2019.xlsx')
openxlsx::write.xlsx (temp3, file = './data/nonstructuralCarbonData_additionalWood_HF_Exp2019.xlsx')
openxlsx::write.xlsx (temp4, file = './data/nonstructuralCarbonData_leaves_HF_Exp2019.xlsx')
openxlsx::write.xlsx (temp5, file = './data/nonstructuralCarbonData_phloem_HF_Exp2019.xlsx')
openxlsx::write.xlsx (temp6, file = './data/nonstructuralCarbonData_phloemLCS_HF_Exp2019.xlsx')
openxlsx::write.xlsx (temp7, file = './data/nonstructuralCarbonData_roots_HF_Exp2019.xlsx')
openxlsx::write.xlsx (temp8, file = './data/nonstructuralCarbonData_wood_HF_Exp2019.xlsx')
openxlsx::write.xlsx (temp9, file = './data/nonstructuralCarbonData_woodLCS_HF_Exp2019.xlsx')

# read data from the spreadsheets
#----------------------------------------------------------------------------------------
rawDataLeaves2019 <- readRawNSCData (fileDir = getwd (),
                                     fileName = './data/nonstructuralCarbonData_leaves_HF_Exp2019.xlsx',
                                     IDs = c ('RCLabNumber', 'SampleID', 'Tissue', 'BatchID'))
rawDataLCS2019 <- readRawNSCData (fileDir = getwd (),
                                  fileName = './data/nonstructuralCarbonData_phloemLCS_HF_Exp2019.xlsx',
                                  IDs = c ('RCLabNumber', 'SampleID', 'Tissue', 'BatchID'))
rawDataPhloem2019 <- readRawNSCData (fileDir = getwd (),
                                     fileName = './data/nonstructuralCarbonData_phloem_HF_Exp2019.xlsx',
                                     IDs = c ('RCLabNumber', 'SampleID', 'Tissue', 'BatchID'))
rawDataWoodAdd22019  <- readRawNSCData (fileDir = getwd (),
                                        fileName = './data/nonstructuralCarbonData_additionalWood2_HF_Exp2019.xlsx',
                                        IDs = c ('RCLabNumber', 'SampleID', 'Tissue', 'BatchID'))
rawDataPhloemAdd2019 <- readRawNSCData (fileDir = getwd (),
                                        fileName = './data/nonstructuralCarbonData_additionalPhloem_HF_Exp2019.xlsx',
                                        IDs = c ('RCLabNumber', 'SampleID', 'Tissue', 'BatchID'))
rawDataWoodAdd2019  <- readRawNSCData (fileDir = getwd (),
                                       fileName = './data/nonstructuralCarbonData_additionalWood_HF_Exp2019.xlsx',
                                       IDs = c ('RCLabNumber', 'SampleID', 'Tissue', 'BatchID'))
rawDataWood2019  <- readRawNSCData (fileDir = getwd (),
                                    fileName = './data/nonstructuralCarbonData_wood_HF_Exp2019.xlsx',
                                    IDs = c ('RCLabNumber', 'SampleID', 'Tissue', 'BatchID'))
rawDataWoodLCS2019  <- readRawNSCData (fileDir = getwd (),
                                       fileName = './data/nonstructuralCarbonData_woodLCS_HF_Exp2019.xlsx',
                                       IDs = c ('RCLabNumber', 'SampleID', 'Tissue', 'BatchID'))
rawDataRoot2019  <- readRawNSCData (fileDir = getwd (),
                                    fileName = './data/nonstructuralCarbonData_roots_HF_Exp2019.xlsx',
                                    IDs = c ('RCLabNumber', 'SampleID', 'Tissue', 'BatchID'))

#----------------------------------------------------------------------------------------
# process the raw data taking into account different calibration curve for the samples 
# and the LCS Oak and LCS Potato
#----------------------------------------------------------------------------------------
processedDataLeaves2019 <- processNSCs (rawData = rawDataLeaves2019,
                                        cvLimitSample = 0.25,
                                        cvLimitTube = 0.25,
                                        LCS = 'Oak',
                                        minimumSampleMass = 5.0)
processedDataPhloem2019 <- processNSCs (rawData = rawDataPhloem2019,
                                        cvLimitSample = 0.25,
                                        cvLimitTube = 0.25,
                                        LCS = 'Oak') # Five samples are excluded due to low samples mass: HF19_1327, HF19_1329, HF19_1355, HF19_1228, and HF19_1349.
processedDataWoodAdd22019  <- processNSCs (rawData = rawDataWoodAdd22019,
                                           cvLimitSample = 0.25,
                                           cvLimitTube = 0.25,
                                           LCS = 'Oak')
processedDataLCS2019 <- processNSCs (rawData = rawDataLCS2019,
                                     cvLimitSample = 0.25,
                                     cvLimitTube = 0.25,
                                     LCS = 'Oak',
                                     minimumSampleMass = 5.0)
prescribedStarch <- unique (processedDataLCS2019 [['MeanStarchRecovery']])
processedDataPhloemAdd2019 <- processNSCs (rawData = rawDataPhloemAdd2019,
                                           cvLimitSample = 0.25,
                                           cvLimitTube = 0.25,
                                           LCS = 'NA',
                                           prescribedStarchRecoveryFraction = prescribedStarch)
processedDataWoodAdd2019  <- processNSCs (rawData = rawDataWoodAdd2019,
                                          cvLimitSample = 0.25,
                                          cvLimitTube = 0.25,
                                          LCS = 'NA',
                                          prescribedStarchRecoveryFraction = prescribedStarch)
processedDataWoodLCS2019  <- processNSCs (rawData = rawDataWoodLCS2019,
                                          cvLimitSample = 0.25,
                                          cvLimitTube = 0.25,
                                          LCS = 'Oak')

# compile a list of unique batches and dates for starch extractions
#--------------------------------------------------------------------------------------
batches <- unique (rawDataWood2019 [['BatchID']])
for (batch in batches) {
  dates <- unique (rawDataWood2019 [['DateOfStarchAnalysis']] [rawDataWood2019 [['BatchID']] == batch])
  prescribedStarch <- unique (processedDataWoodLCS2019 [['MeanStarchRecovery']] [processedDataWoodLCS2019 [['BatchID']] == batch])
  if (batch == batches [1] & !is.na (dates [1])) {
    extractionsStarch <- tibble (batch = batch, date = dates, starchRecovery = prescribedStarch)
  } else if (!is.na (dates [1])) {
    extractionsStarch <- add_row (extractionsStarch, batch = batch, date = dates, starchRecovery = prescribedStarch)
  }
}
# Process each batch with prescribed starch recovery from the processedDataWoodLCS2019 data
#--------------------------------------------------------------------------------------
for (i in 1:dim (extractionsStarch) [1]) {
  temp <- processNSCs (rawData = rawDataWood2019,
                       cvLimitSample = 0.25,
                       cvLimitTube = 0.25,
                       LCS = '',
                       prescribedStarchRecoveryFraction = extractionsStarch [['starchRecovery']] [i])
  # Only choose the data from the appropraite batch 
  #------------------------------------------------------------------------------------
  if (i == 1) {
    processedDataWood2019 <- temp [temp [['BatchID']] == extractionsStarch [['batch']] [i] & 
                                     temp [['DateOfStarchAnalysis']] == extractionsStarch [['date']] [i], ]
  } else {
    processedDataWood2019 <- rbind (processedDataWood2019, 
                                    temp [temp [['BatchID']] == extractionsStarch [['batch']] [i] & 
                                            temp [['DateOfStarchAnalysis']] == extractionsStarch [['date']] [i], ])
  }
}

# process root data
#--------------------------------------------------------------------------------------
processedDataRoots2019 <- processNSCs (rawData = rawDataRoot2019,
                                       cvLimitSample = 0.25,
                                       cvLimitTube = 0.25,
                                       LCS = 'Oak') # Four samples with too little mass to be analysed.

rm (rawDataLeaves2019, rawDataLCS2019, rawDataPhloemAdd2019, rawDataPhloem2019, 
    rawDataWoodAdd2019, rawDataWoodAdd22019, rawDataWood2019, rawDataWoodLCS2019, 
    rawDataRoot2019)

# produce pdf files with calibration curves, when needed
#----------------------------------------------------------------------------------------
PLOTCAL <- FALSE
if (PLOTCAL) {
  res <- plotCalibrationCurves (data = processedDataLeaves2019)
  if (res != 0) print ('Error: plotCalibrationCurves () did not work.')
  res <- plotCalibrationCurves (data = processedDataLCS2019)
  if (res != 0) print ('Error: plotCalibrationCurves () did not work.')
  res <- plotCalibrationCurves (data = processedDataPhloemAdd2019)
  if (res != 0) print ('Error: plotCalibrationCurves () did not work.')
  res <- plotCalibrationCurves (data = processedDataPhloem2019)
  if (res != 0) print ('Error: plotCalibrationCurves () did not work.')
  res <- plotCalibrationCurves (data = processedDataWoodAdd22019)
  if (res != 0) print ('Error: plotCalibrationCurves () did not work.')
  res <- plotCalibrationCurves (data = processedDataAddPhloem2019)
  if (res != 0) print ('Error: plotCalibrationCurves () did not work.')
  res <- plotCalibrationCurves (data = processedDataWoodAdd2019)
  if (res != 0) print ('Error: plotCalibrationCurves () did not work.')
  res <- plotCalibrationCurves (data = processedDataWood2019)
  if (res != 0) print ('Error: plotCalibrationCurves () did not work.')
  res <- plotCalibrationCurves (data = processedDataWoodLCS2019) # Waiting to hear from Jim about the funny outlier in the REf100/100 value in cell L33 
  if (res != 0) print ('Error: plotCalibrationCurves () did not work.')
  res <- plotCalibrationCurves (data = processedDataRoots2019)
  if (res != 0) print ('Error: plotCalibrationCurves () did not work.')
  
  rm (res)
}
rm (PLOTCAL)

# combine all processed data into one tibble
#----------------------------------------------------------------------------------------
dataExp2019 <- rbind (processedDataLeaves2019, processedDataPhloem2019, 
                      add_column (processedDataPhloemAdd2019, SRFHigh = 'N', LCSOakDeviation = NA),
                      add_column (processedDataWood2019,      SRFHigh = 'N', LCSOakDeviation = NA),
                      add_column (processedDataWoodAdd2019,   SRFHigh = 'N', LCSOakDeviation = NA),
                      processedDataWoodAdd22019,
                      processedDataRoots2019)
rm (processedDataLeaves2019,  processedDataLCS2019,      processedDataPhloemAdd2019, 
    processedDataPhloem2019,  processedDataWoodLCS2019,  processedDataWood2019, 
    processedDataWoodAdd2019, processedDataWoodAdd22019, processedDataRoots2019)

# add and fill columns for treeID, and treatment to 2018 tibble 
#----------------------------------------------------------------------------------------
condition <- substr (dataExp2019 [['SampleID']], 1, 3) == 'EXP'
dataExp2019 [['treeID']] [condition] <-
  as.numeric (substr (dataExp2019 [['SampleID']] [condition], 9, 10))
dataExp2019 [['sampleHeight']] [condition] <-
  substr (dataExp2019 [['Tissue']] [condition], 
          nchar (dataExp2019 [['Tissue']] [condition]), 
          nchar (dataExp2019 [['Tissue']] [condition]))
dataExp2019 [['sampleHeight']] [dataExp2019 [['sampleHeight']] == 't' | 
                                  dataExp2019 [['sampleHeight']] == 'e'] <- NA
dataExp2019 [['sampleHeight']] <- as.numeric (dataExp2019 [['sampleHeight']] )
dataExp2019 [['treatment']] <- dataExp2019 [['treeID']]
dataExp2019 [['treatment']] [dataExp2019 [['treatment']] %in% c (1, 3, 5, 8)] <- 1
dataExp2019 [['treatment']] [dataExp2019 [['treatment']] %in% c (2, 4, 6, 7)] <- 5

# Arrange the data by date
#--------------------------------------------------------------------------------------
dataExp2019 <- dataExp2019 %>% arrange (DateOfSampleCollection)

# extract tissue-specific data, currently only stems
#----------------------------------------------------------------------------------------
leafData2019   <- dataExp2019 [substr (dataExp2019 [['Tissue']], 1, 7) == 'Foliage', ]
phloemData2019 <- dataExp2019 [substr (dataExp2019 [['Tissue']], 1, 6) == 'Phloem', ]
stemData2019   <- dataExp2019 [substr (dataExp2019 [['Tissue']], 1, 4) == 'Stem', ]
rootData2019   <- dataExp2019 [substr (dataExp2019 [['Tissue']], 1, 4) == 'Root', ]

# average across initial and re-run for the roots because they have pretty similar values
#--------------------------------------------------------------------------------------
rootData2019 <- rootData2019 %>% group_by (treeID, treatment, DateOfSampleCollection) %>% 
  summarise (ConcentrationSugarPerDW  = mean (ConcentrationSugarPerDW,  na.rm = TRUE),
             ConcentrationStarchPerDW = mean (ConcentrationStarchPerDW, na.rm = TRUE)) %>% 
  ungroup ()

# get the coefficient of variation for lab controls 
#--------------------------------------------------------------------------------------
COV <- filter (dataExp2019, substr (SampleID, 1, 7) == 'LCS Oak' |
                 substr (SampleID, 1, 7) == 'LCS Pot') %>%
  group_by (SampleID) %>% 
  select (SampleID, ConcentrationSugarPerDW, ConcentrationStarchPerDW) %>% 
  summarise (covSugar = sd (ConcentrationSugarPerDW) / mean (ConcentrationSugarPerDW),
             covStarch = sd (ConcentrationStarchPerDW) / mean (ConcentrationStarchPerDW))

# Get the number of blanks and controls per batch
#----------------------------------------------------------------------------------------
temp <- filter (dataExp2019, SampleID == 'B' | SampleID == 'TB') %>% 
  group_by (BatchID, DateOfSugarAnalysis) %>% 
  summarise (n = sum (SampleID == 'B' | SampleID == 'TB'))
# range (temp [['n']])
temp <- filter (dataExp2019, substr (SampleID, 1, 6) == 'REF100' | 
                  substr (SampleID, 1, 3) == 'LCS') %>%
  group_by (BatchID, DateOfSugarAnalysis) %>% 
  summarise (n = sum (substr (SampleID, 1, 6) == 'REF100' | 
                        substr (SampleID, 1, 3) == 'LCS'))
# temp [['n']]

# switch back to original working directory
#----------------------------------------------------------------------------------------
setwd (originalDir)

# clean up
#----------------------------------------------------------------------------------------
rm (extractionsStarch, temp1, temp2, temp3, temp4, temp5, temp6, temp7, temp8, temp9, 
    batch, batches, colNames, condition, i, temp, COV, dates, originalDir, packageDir, 
    prescribedStarch, dataExp2019)
#========================================================================================