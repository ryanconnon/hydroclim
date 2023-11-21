##############################################################################

# Defines parameter based on multiple potential user inputs

hydro_parameter <- function(parameter) 
  
{

if(grepl(paste0("(?i)", parameter), "Flows") == T | 
   grepl(paste0("(?i)", parameter), "Discharge") == T |
   grepl(paste0("(?i)", parameter), "Q") == T) {
  
  parameter <- "Flow"
  parameter_num <- 47
  y_axis_title <- expression(paste("Discharge (m"^3, " s"^-1,")"))
  
} else if(grepl(paste0("(?i)", parameter), "Water _ Levels") == T) {
  
  parameter <- "Level"
  parameter_num <- 46
  y_axis_title <- "Water Level (m)"
  
} else {
  
  stop("Parameter selection is invalid. Input must be 'Flows' or 'Levels'")
  
}
  as.list(c(parameter, parameter_num, y_axis_title))
  
}

##############################################################################

data_check <- function(parameter) {
  if (is.null(station_number))
    stop("Must select one of station_number arguments to supply data.", 
         call. = FALSE)
}

##############################################################################

# analysis_prep

analysis_prep <- function (data, water_year_start, date = FALSE) 
{
  data <- fasstr::fill_missing_dates(data = data, water_year_start = water_year_start)
  data <- fasstr::add_date_variables(data = data, water_year_start = water_year_start)
  if (date) {
    if (water_year_start == 1) {
      data$AnalysisDate <- as.Date(data$DayofYear, origin = "1899-12-31")
    }
    else if (water_year_start == 2) {
      data$AnalysisDate <- as.Date(data$DayofYear, origin = "1899-01-31")
    }
    else if (water_year_start == 3) {
      data$AnalysisDate <- as.Date(data$DayofYear, origin = "1899-02-28")
    }
    else if (water_year_start == 4) {
      data$AnalysisDate <- as.Date(data$DayofYear, origin = "1899-03-31")
    }
    else if (water_year_start == 5) {
      data$AnalysisDate <- as.Date(data$DayofYear, origin = "1899-04-30")
    }
    else if (water_year_start == 6) {
      data$AnalysisDate <- as.Date(data$DayofYear, origin = "1899-05-31")
    }
    else if (water_year_start == 7) {
      data$AnalysisDate <- as.Date(data$DayofYear, origin = "1899-06-30")
    }
    else if (water_year_start == 8) {
      data$AnalysisDate <- as.Date(data$DayofYear, origin = "1899-07-31")
    }
    else if (water_year_start == 9) {
      data$AnalysisDate <- as.Date(data$DayofYear, origin = "1899-08-31")
    }
    else if (water_year_start == 10) {
      data$AnalysisDate <- as.Date(data$DayofYear, origin = "1899-09-30")
    }
    else if (water_year_start == 11) {
      data$AnalysisDate <- as.Date(data$DayofYear, origin = "1899-10-31")
    }
    else if (water_year_start == 12) {
      data$AnalysisDate <- as.Date(data$DayofYear, origin = "1899-11-30")
    }
  }
  data
}

##############################################################################

no_values_error <- function (values) 
{
  if (all(is.na(values))) 
    stop("All daily values are NA, select or filter data for years with data.", 
         call. = FALSE)
}

##############################################################################

exclude_bennett <- function (after_bennett) 
{
  if(after_bennett == T) {
    historic_min = 1972
  }
  historic_min
}

##############################################################################

# Define `%>%` operator in current environment

`%>%` <- magrittr::`%>%`

##############################################################################

# Manually call tidyhydat.ws into active library
library(tidyhydat.ws)

##############################################################################




