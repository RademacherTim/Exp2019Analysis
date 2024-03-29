#========================================================================================
# Script with variables and functions relevant for ploting figuresin R
#----------------------------------------------------------------------------------------

# Load dependencies
#----------------------------------------------------------------------------------------
library ('tidyverse')
library ('lubridate')

# function for calculate the standard error
#----------------------------------------------------------------------------------------
se <-  function (x) {
  sd (x, na.rm = TRUE) / sqrt (sum (!is.na (x)))
}

# Create %notin% operator
#----------------------------------------------------------------------------------------
`%notin%` <- Negate (`%in%`)

# set colours for treatments: control, girdled, compressed, double compressed and chilled
#----------------------------------------------------------------------------------------
#tColours <- tibble (colour = c ('#91b9a4','#C0334D','#F18904','#5C4A72','#0073cf'),
#                    treatment = c ('control','girdled','compressed','double compressed',
#                                   'chilled'))
tColours <- tibble (colour = c ('#aab300','#55a51c','#445026','#68ace5','#0072cf','#003e74','#C0334D'),
                    treatment = c ('control','control2','control3','chilled','chilled2',
                                   'chilled3','air'))

# set colours for carbon sinks: growth, respiration, change in NSC concentrations. 
#----------------------------------------------------------------------------------------
sColours <- tibble (colour   = c ('#8073ac','#f3bd48','#aab300'), 
                    variable = c ('NSC'    ,'resp'   ,'SC'))

# function to add opacity to a colour
#----------------------------------------------------------------------------------------
addOpacity <- function (colour, alpha = 1) {
  if (missing (colour)) stop ("Please provide a colour or vector of colours.")
  apply (sapply (colour, col2rgb) / 255, 2, 
         function (x) rgb (x [1], x [2], x [3], alpha = alpha))
}

# y-axis positions of treatment and sampling height combinations
#----------------------------------------------------------------------------------------
yPositions <- c (0.8, 1.8, 2.3, 3.3, 3.8, 4.8, 5.3, 5.8)

# Function to plot critical dates
#----------------------------------------------------------------------------------------
criticalDates <- function (group, asDate = FALSE, startOnly = FALSE, endOnly = FALSE) {
  
  # if only start or end date are required return only the appropriate date
  #------------------------------------------------------------------------------------
  if (startOnly) {
    if (asDate) {
      return (as_date ('2019-05-29'))
    } else {
      return (as_datetime ('2019-05-29 12:00:00'))
    } 
  }
  if (endOnly) {
    if (asDate) {
      return (as_date ('2019-07-10'))
    } else {
      return (as_datetime ('2019-07-10 12:00:00'))
    } 
  }

  # Figure out critical date and plot them
  #------------------------------------------------------------------------------------
  if (asDate) {
    abline (v = as_date ('2019-05-29'), col = '#99999999', lty = 2) # start date
  } else {
    abline (v = as_datetime ('2019-05-29 12:00:00'), col = '#99999999', lty = 2) # start date
  }
  if (group == 'chilled' | group == '5') {
    if (asDate) {
      abline (v = as_date ('2019-07-10'), col = '#99999999', lty = 2) # end date
    } else {
      abline (v = as_datetime ('2019-07-10 12:00:00'), col = '#99999999', lty = 2) # end date
    }
  }
  
  # Return zero exit status
  #----------------------------------------------------------------------------------------
  return (0)
}
#========================================================================================