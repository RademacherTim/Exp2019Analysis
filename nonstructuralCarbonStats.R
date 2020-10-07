#========================================================================================
# This script tests for differences between treatments in leaf and roots soluble sugar 
# and starch concentrations and treatment and sampling height in xylem and phloem soluble 
# sugar and starch concentrations for the 2019 phloem chilling experiment on red maple at 
# Harvard Forest. 
#----------------------------------------------------------------------------------------

# load dependencies
#----------------------------------------------------------------------------------------
library ('lme4')
source ('./plotingFunctions.R')

# source processed data
#----------------------------------------------------------------------------------------
source ('./readNonstructuralCarbonData.R') 

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


# fit mixed effects model to wood sugar concentrations with tree and height as random 
# effects to account for idiosyncratic differences due to factors such as variations 
# in exact azimuth or systematic difference between trees
#----------------------------------------------------------------------------------------
M1 <- lmer (formula = sugar ~ (1 | tree) + date + period:treatment, 
            data = leafData2019,
            REML = TRUE)
summary (M1)
M2 <- lmer (formula = starch ~ (1 | tree) + date + period:treatment, 
            data = leafData2019,
            REML = TRUE)
summary (M2)

# stems
#----------------------------------------------------------------------------------------
M3 <- lmer (formula = sugar ~ (tree | height) + date + period:treatment:height, 
            data = stemData2019,
            REML = TRUE)
summary (M3)
M4 <- lmer (formula = starch ~ (tree | height) + date + period:treatment:height, 
            data = stemData2019,
            REML = TRUE)
summary (M4)

# phloem
#----------------------------------------------------------------------------------------
M5 <- lmer (formula = sugar ~ (tree | height) + date + period:treatment:height, 
            data = phloemData2019,
            REML = TRUE)
summary (M5)
M6 <- lmer (formula = starch ~ (tree | height) + date + period:treatment:height, 
            data = phloemData2019,
            REML = TRUE)
summary (M6)

# root
#----------------------------------------------------------------------------------------
M7 <- lmer (formula = sugar ~ (1 | tree) + date + period:treatment, 
            data = rootData2019,
            REML = TRUE)
summary (M7)
M8 <- lmer (formula = starch ~ (1 | tree) + date + period:treatment, 
            data = rootData2019,
            REML = TRUE)
summary (M8)

#========================================================================================