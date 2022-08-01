#========================================================================================
# This script tests for differences between treatments in leaf and roots soluble sugar 
# and starch concentrations and treatment and sampling height in xylem and phloem soluble 
# sugar and starch concentrations for the 2019 phloem chilling experiment on red maple at 
# Harvard Forest. 
#----------------------------------------------------------------------------------------

# load dependencies
#----------------------------------------------------------------------------------------
if (!existsFunction ('lmer')) library('lme4')
if (!existsFunction ('%>%'))  library('tidyverse')
if (!exists ('tColours')) source('./plotingFunctions.R')

# source processed data
#----------------------------------------------------------------------------------------
if (!exists ('leafData2019')) source('./readNonstructuralCarbonData.R') 

# select only necessary columns and wrangle data to contain factors
#----------------------------------------------------------------------------------------
leafData2019 <- leafData2019 %>% 
  select (DateOfSampleCollection, treeID, treatment, 
          ConcentrationSugarPerDW, ConcentrationStarchPerDW) %>% 
  rename (date = DateOfSampleCollection, 
          tree = treeID,
          sugar = ConcentrationSugarPerDW, 
          starch = ConcentrationStarchPerDW) %>%
  mutate (date = as_date (date))
stemData2019 <- stemData2019 %>% 
  select (DateOfSampleCollection, treeID, sampleHeight, treatment, 
          ConcentrationSugarPerDW, ConcentrationStarchPerDW) %>% 
  rename (date = DateOfSampleCollection, tree = treeID, height = sampleHeight,
          sugar = ConcentrationSugarPerDW, starch = ConcentrationStarchPerDW) %>%
  mutate (date = as_date (date))
phloemData2019 <- phloemData2019 %>% 
  select (DateOfSampleCollection, treeID, sampleHeight, treatment, 
          ConcentrationSugarPerDW, ConcentrationStarchPerDW) %>% 
  rename (date = DateOfSampleCollection, tree = treeID, height = sampleHeight,
          sugar = ConcentrationSugarPerDW, starch = ConcentrationStarchPerDW) %>%
  mutate (date = as_date (date))
rootData2019 <- rootData2019 %>% 
  rename (date = DateOfSampleCollection, tree = treeID,
          sugar = ConcentrationSugarPerDW, starch = ConcentrationStarchPerDW) %>%
  mutate (date = as_date (date))

# get start and end date of the chilling
#----------------------------------------------------------------------------------------
startDate <- criticalDates (group = '5', asDate = TRUE, startOnly = TRUE) 
endDate   <- criticalDates (group = '5', asDate = TRUE, endOnly   = TRUE)

# add column based on period (e.g., before, during, and after treatment)
#----------------------------------------------------------------------------------------
period <- ifelse (leafData2019 [['date']] < startDate, 'before', 
                  ifelse (leafData2019 [['date']] > endDate, 'after','during'))
periodAlt <- ifelse (leafData2019 [['date']] < startDate | 
                       leafData2019 [['date']] > endDate,'non-chilling','chilling')
leafData2019 <- leafData2019 %>% mutate (period, periodAlt)
period <- ifelse (stemData2019 [['date']] < startDate, 'before', 
                  ifelse (stemData2019 [['date']] > endDate, 'after','during'))
periodAlt <- ifelse (stemData2019 [['date']] < startDate | 
                       stemData2019 [['date']] > endDate,'non-chilling','chilling')
stemData2019 <- stemData2019 %>% mutate (period, periodAlt)
period <- ifelse (phloemData2019 [['date']] < startDate, 'before', 
                  ifelse (phloemData2019 [['date']] > endDate, 'after','during'))
periodAlt <- ifelse (phloemData2019 [['date']] < startDate | 
                       phloemData2019 [['date']] > endDate,'non-chilling','chilling')
phloemData2019 <- phloemData2019 %>% mutate (period, periodAlt)
period <- ifelse (rootData2019 [['date']] < startDate, 'before', 
                  ifelse (rootData2019 [['date']] > endDate, 'after','during'))
periodAlt <- ifelse (rootData2019 [['date']] < startDate | 
                       rootData2019 [['date']] > endDate,'non-chilling','chilling')
rootData2019 <- rootData2019 %>% mutate (period, periodAlt)

# convert variable to factors
#----------------------------------------------------------------------------------------
leafData2019 <- leafData2019 %>%
  mutate (date      = factor (date),
          tree      = factor (tree),
          treatment = factor (treatment, levels = c (5, 1)),
          period    = factor (period,    levels = c ('during','after','before')),
          periodAlt = factor (periodAlt, levels = c ('chilling','non-chilling')))
stemData2019 <- stemData2019 %>%
  mutate (date      = factor (date),
          tree      = factor (tree),
          height    = factor (height,    levels = 3:1),
          treatment = factor (treatment, levels = c (5, 1)),
          period    = factor (period,    levels = c ('during','after','before')),
          periodAlt = factor (periodAlt, levels = c ('chilling','non-chilling')))
phloemData2019 <- phloemData2019 %>%
  mutate (date      = factor (date),
          tree      = factor (tree),
          height    = factor (height,    levels = 3:1),
          treatment = factor (treatment, levels = c (5, 1)),
          period    = factor (period,    levels = c ('during','after','before')),
          periodAlt = factor (periodAlt, levels = c ('chilling','non-chilling')))
rootData2019 <- rootData2019 %>%
  mutate (date      = factor (date),
          tree      = factor (tree),
          treatment = factor (treatment, levels = c (5, 1)),
          period    = factor (period,    levels = c ('during','after','before')),
          periodAlt = factor (periodAlt, levels = c ('chilling','non-chilling')))


# fit mixed effects model to wood sugar concentrations with tree or tree and sampling 
# height as (nested) random effects to account for idiosyncratic differences due to 
# factors such as variations in exact azimuth or systematic difference between trees
#----------------------------------------------------------------------------------------
M0 <- lmer (formula = sugar ~ (1 | tree) + period + period:treatment, 
            data = leafData2019,
            REML = TRUE)
summary (M0)
cAIC (M0)
M1 <- lmer (formula = sugar ~ (1 | tree) + date + period:treatment, 
            data = leafData2019,
            REML = TRUE)
summary (M1)
cAIC (M1)
# Report the second model (M1) with date instead of period in publication

M2 <- lmer (formula = starch ~ (1 | tree) + period + period:treatment, 
            data = leafData2019,
            REML = TRUE)
summary (M2)
cAIC (M2)
M3 <- lmer (formula = starch ~ (1 | tree) + date + period:treatment, 
            data = leafData2019,
            REML = TRUE)
summary (M3)
cAIC (M3)
# Report the second model (M3) with date instead of period in publication

# phloem sugar concentrations with nested tree and sample height random effect
#----------------------------------------------------------------------------------------
M4 <- lmer (formula = sugar ~ (1 | tree / height) + period + period:treatment:height, 
            data = phloemData2019,
            REML = TRUE)
summary (M4)
cAIC (M4)
M5 <- lmer (formula = sugar ~ (1 | tree / height) + date + period:treatment:height, 
            data = phloemData2019,
            REML = TRUE)
summary (M5)
cAIC (M5)
# Also tested model with just tree as random effect without the nested sample height, 
# but they lost more information according to conditional AIC. Consequently, we report 
# the first model (M4) which has a slightly lower AIC.

# phloem starch
M6 <- lmer (formula = starch ~ (1 | tree / height) + period + period:treatment:height, 
            data = phloemData2019,
            REML = TRUE)
summary (M6)
cAIC (M6)
M7 <- lmer (formula = starch ~ (1 | tree / height) + date + period:treatment:height, 
            data = phloemData2019,
            REML = TRUE)
summary (M7)
cAIC (M7)
# Also tested model with just tree as random effect without the nested sample height. 
# Just using tree id as random variable had marginally lower condition AIC, but I report 
# the second model (M7) as described above including the nested random effect for 
# consistency.

# stems sugar concentrations with nested tree and sample height random effect
#----------------------------------------------------------------------------------------
M8 <- lmer (formula = sugar ~ (1 | tree / height) + period + period:treatment:height, 
            data = stemData2019,
            REML = TRUE)
summary (M8)
cAIC (M8)
M9 <- lmer (formula = sugar ~ (1 | tree / height) + date + period:treatment:height, 
            data = stemData2019,
            REML = TRUE)
summary (M9)
cAIC (M9)
# Also tested model with just tree as random effect without the nested sample height. 
# Just using tree id as random variable had marginally lower condition AIC, but I report 
# the second model (M9) as described above including the nested random effect for 
# consistency.

# stems starch concentrations with nested tree and sample height random effect
#----------------------------------------------------------------------------------------
M10 <- lmer (formula = starch ~ (1 | tree / height) + period + period:treatment:height, 
             data = stemData2019,
             REML = TRUE)
summary (M10)
cAIC (M10)
M11 <- lmer (formula = starch ~ (1 | tree / height) + date + period:treatment:height, 
             data = stemData2019,
             REML = TRUE)
summary (M11)
cAIC (M11)
# Report M11 which has the joint lowest conditional AIC with the same model with just 
# tree id as random effect.

# root sugar concentrations with random effect for tree id
#----------------------------------------------------------------------------------------
M12 <- lmer (formula = sugar ~ (1 | tree) + period + period:treatment, 
             data = rootData2019,
             REML = TRUE)
summary (M12)
cAIC (M12)
M13 <- lmer (formula = sugar ~ (1 | tree) + date + period:treatment, 
             data = rootData2019,
             REML = TRUE)
summary (M13)
cAIC (M13)
# Report the second model (M13) with date instead of period in publication

# root starch concentrations with random effect for tree id
#----------------------------------------------------------------------------------------
M14 <- lmer (formula = starch ~ (1 | tree) + period + period:treatment, 
             data = rootData2019,
             REML = TRUE)
summary (M14)
cAIC (M14)
M15 <- lmer (formula = starch ~ (1 | tree) + date + period:treatment, 
             data = rootData2019,
             REML = TRUE)
summary (M15)
cAIC (M15)
# Report the second model (M15) with date instead of period in publication

# Read leaf phenology measurements to test for differences
#----------------------------------------------------------------------------------------
# Read John O'Keefe's long-term observations for red maple (ACRU), red oak (QURU), and 
# white pine (PIST) from the Harvard Forest Data Archive
#----------------------------------------------------------------------------------------
springHF <- read_csv (url ('https://harvardforest.fas.harvard.edu/data/p00/hf003/hf003-05-spring-mean-ind.csv'),
                      col_types = cols ()) %>% filter (species =='ACRU')
fallHF <- read_csv (url ('https://harvardforest.fas.harvard.edu/data/p00/hf003/hf003-07-fall-mean-ind.csv'),
                    col_types = cols ()) %>% filter (species == 'ACRU')
leafPhenology <- left_join (x = springHF, y = fallHF, by = c ('year','tree.id','species'))
leafPhenology <- add_column (leafPhenology, study = 'Obs', treatment = 1, .before = 1) %>%
  add_column (comments = NA, contributor = 'JOK')
leafPhenology [['species']] [leafPhenology [['species']] == 'ACRU'] <- "Acer rubrum"

# Read and join Tim Rademacher's observations from spreadsheet to leafPhenology
#----------------------------------------------------------------------------------------
tmp <- add_column (readxl::read_excel (path = '/media/tim/dataDisk/PlantGrowth/data/leafPhenology/TimRademacherData/leafPhenologySpringAndFallHavardForest2017-2019.xlsx', na = '-99'),
                   contributor = 'TR')
leafPhenology <- rbind (leafPhenology, tmp) %>% 
  filter (species == 'Acer rubrum') %>%
  mutate (tree.id = factor (tree.id),
          treatment = factor (treatment)) %>% 
  select (-comments, -species)

# mean bud break date by group
#----------------------------------------------------------------------------------------
leafPhenology %>% group_by (study, year, treatment, contributor) %>% 
  summarise (meanBB = mean (bb.doy, na.rm = TRUE),
             seBB   = se   (bb.doy),
             meanLC = mean (lc.doy, na.rm = TRUE),
             seLC   = se   (lc.doy),
             meanLF = mean (lf.doy, na.rm = TRUE),
             seLF   = se   (lf.doy), 
             .groups = 'keep') %>% 
  print (n = 37)
#========================================================================================