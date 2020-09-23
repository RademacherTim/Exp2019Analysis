#========================================================================================
# read all processed respiration data for the 2018 experiment
#----------------------------------------------------------------------------------------

# load dependencies
#----------------------------------------------------------------------------------------
library ('tidyverse')

# read processed stem and soil respiration data for 2018 experiment and the observational 
# white pines 
#----------------------------------------------------------------------------------------
respDataAll <- read_csv (file = './data/respirationData_HF_Exp2019.csv', # TR change name eventually to Harvard Forest dataset 
                         col_types = cols (
                           file           = col_character (), # name of the raw data file
                           study          = col_character (), # name of the study
                           treatment      = col_integer   (), # treatment group (1: control, 4: double compressed, 5: chilled)
                           tree           = col_integer   (), # tree identifier
                           species        = col_character (), # tree species
                           chamber        = col_integer   (), # chamber identifier (1: 0.5m above the ground, 2: 1.5m above the ground, 3: 2.5m above the ground)
                           chamber.volume = col_double    (), # volume of the respiration chamber (m2 / missing value: NA)
                           chamber.area   = col_double    (), # stem surface area covered by the chamber
                           datetime       = col_datetime  (), # time stamp of when the measurement started
                           session        = col_character (), # identifier for the session of respiration measurements
                           flux.raw       = col_double (), # mean respiratory flux from this chamber (micromole m-2 s-1 / missing value: NA)
                           sd.flux.raw    = col_double (), # standard deviation of the respiratory flux from this chamber (unit: micromole m-2 s-1 / missing value: NA)
                           aic.raw        = col_double (), # Akaike Information Criterion of model fitted to respiration curve (unit: dimensionless / missing value: NA)
                           r2.raw         = col_double (), # R2 of fit to the respiration curve (unit: dimensionless / missing value: NA)
                           flux.atm       = col_double (), # mean respiratory flux after correction for water vapour dilution using atmospheric humidity (unit: micromole m-2 s-1 / missing value: NA)
                           sd.flux.atm    = col_double (), # standard deviation of the respiratory flux after correction for water vapour dilution using atmospheric humidity from this chamber (unit: micromole m-2 s-1 / missing value: NA)
                           aic.atm        = col_double (), # Akaike Information Criterion of model fitted to respiration curve after correction for water vapour dilution using atmospheric humidity (unit: dimensionless / missing value: NA)
                           r2.atm         = col_double (), # R2 of fit to the respiration curve after correction for water vapour dilution using atmospheric humidity (unit: dimensionless / missing value: NA)
                           flux.int       = col_double (), # mean respiratory flux after correction for water vapour dilution using internal humidity (unit: micromole m-2 s-1 / missing value: NA)
                           sd.flux.int    = col_double (), # standard deviation of the respiratory flux after correction for water vapour dilution using internal humidity (unit: micromole m-2 s-1 / missing value: NA)
                           aic.int        = col_double (), # Akaike Information Criterion of model fitted to respiration curve after correction for water vapour dilution using internal humidity (unit: dimensionless / missing value: NA)
                           r2.int         = col_double (), # R2 of fit to the respiration curve after correction for water vapour dilution using internal humidity (unit: dimensionless / missing value: NA)
                           ea.pa          = col_double (), # atmospheric pressure from the Fisher meteorological station (unit: pascal / missing value: NA)
                           airt.c         = col_double (), # surface air temperature from the Fisher meteorological station (unit: celsius / missing value: NA)
                           soilt1.c       = col_double (), # soil temperature at 2.25 cm near the Barn Tower (unit: celsius / missing value: NA)
                           soilt2.c       = col_double (), # soil temperature at 6.8 cm near the Barn Tower (unit: celsius / missing value: NA)
                           soilt3.c       = col_double (), # soil temperature at 12.85 cm near the Barn Tower (unit: celsius / missing value: NA)
                           soilt4.c       = col_double (), # soil temperature at 22.75 cm near the Barn Tower (unit: celsius / missing value: NA)
                           pres.pa        = col_double (), # atmospheric barometric pressure from the Fisher Meteorological station (unit: pascal / missing value: NA)
                           h2o.ppt.atm    = col_double (), # atmospheric humidity from the Fisher meteorological station (unit: ppt / missing value: NA)
                           h2o.ppt.int    = col_double (), # internal humidity from the Li-Cor 840 (unit: ppt / missing value: NA)
                           vwc.daily      = col_double (), # daily mean volumetric water content from probes at the Barn Tower (unit: % / missing value: NA)
                           vwc1           = col_double (), #  10-minute mean volumetric water content at 2.25 cm near the Barn Tower (unit: % / missing value: NA)
                           vwc2           = col_double (), # 10-minute mean volumetric water content at 6.8 cm near the Barn Tower (unit: % / missing value: NA)
                           vwc3           = col_double (), # 10-minute mean volumetric water content at 12.85 cm near the Barn Tower (unit: % / missing value: NA)
                           vwc4           = col_double (), # 10-minute mean volumetric water content at 22.75 cm near the Barn Tower (unit: % / missing value: NA)
                           total.rad      = col_double (), # total downwelling shortwave radiation at top of Barn Tower (unit: W m-2 / missing value: NA)
                           diffuse.rad    = col_double ()  # diffuse downwelling shortwave radiation at top of Barn Tower (unit: W m-2 / missing value: NA)
                         ))

# divide data by study (i.e., Exp2019, Obs2018, Obs2019)
#----------------------------------------------------------------------------------------
respDataExp2019      <- respDataAll %>% filter (study == 'Exp2019')
respDataObs          <- respDataAll %>% filter (study == 'Obs2018' | study == 'Obs2019')

# remove the observational data from the session of 2019-10-02, because there was an 
# issue with the pump and all values were abnormally high. The session was also not 
# finished due to a low battery and measurements taken on the 2019-10-03, once the 
# battery was charged look good again.   
#----------------------------------------------------------------------------------------
respDataObs <- filter (respDataObs, session != '20191002_1300')
