#' hydro_calc_daily
#'
#' Calculates daily values of hydrometric data, along with summary stats for each day of year
#' @return A tibble of the daily values
#' @export

# Function to plot hydrometric data

hydro_calc_daily <- function(
  station_number,
  parameter = "level",
  select_years = lubridate::year(Sys.Date()),
  after_bennett = FALSE,
  historic_min = NA,
  historic_max = NA,
  water_year_start = 1)
  
{
  
  # These functions depend on hydroclim_helpers
  # Define variables

  parameter <- hydro_parameter(parameter = parameter)[[1]]
  parameter_num <- hydro_parameter(parameter = parameter)[[2]]
  
  # This function will create two dataframes: 
  # 1) a historic dataframe (accessed from tidyhydat)
  # 2) a realtime dataframe (accessed from tidyhydat.ws)

  data.finalized <- tidyhydat::hy_daily(station_number = station_number) %>%
    dplyr::filter(Parameter == parameter) %>%
    dplyr::select(STATION_NUMBER, Date, Value)

  # Download realtime data
  
  if (max(select_years) >= lubridate::year(Sys.Date() - 548)) {
    
    suppressWarnings(
    data.realtime <- tidyhydat.ws::realtime_ws(
      station_number = station_number,
      parameters = as.numeric(parameter_num),
      start_date = paste(lubridate::year(max(data.finalized$Date)) + 1, "01", "01", sep = "-"),
      end_date = ifelse(lubridate::year(Sys.Date()) > max(select_years),
                        paste(max(select_years), "12", "31", sep = "-"),
                        paste(Sys.Date())),
      token = tidyhydat.ws::token_ws())
    )
    
    data.realtime <- data.realtime %>%
      dplyr::group_by(STATION_NUMBER, lubridate::year(Date), lubridate::yday(Date)) %>%
      dplyr::summarize(Date = mean(lubridate::date(Date)),
                       Value = mean(Value), 
                       .groups = 'drop') %>%
      dplyr::select(STATION_NUMBER, Date, Value)
    
    data <- dplyr::bind_rows(data.finalized, data.realtime)

  } else {
    data <- data.finalized
  }

  flow_data <- analysis_prep(data = data, 
                             water_year_start = water_year_start)
  
  # Remove Feb. 29 data
  flow_data <- flow_data[!(format(flow_data$Date,"%m") == "02" & format(flow_data$Date, "%d") == "29"), , drop = FALSE]
  
  # Change DayofYear column to account for no Feb. 29 data
  flow_data <- dplyr::mutate(dplyr::group_by(flow_data, WaterYear),
                             DayofYear = c(1:365))
  
  # Stop if all data is NA
  no_values_error(flow_data$Value)
  
  # Filter for selected years
  if(after_bennett == T) {
    historic_min = 1972
  }
  
  stat_data <- dplyr::filter(flow_data, 
                             WaterYear >= ifelse(is.na(historic_min),
                                                            min(WaterYear),
                                                            historic_min) &
                               WaterYear <= ifelse(is.na(historic_max),
                                                   max(WaterYear),
                                                   historic_max))
  
  # Calculate annual statistics
  stat_data <- dplyr::reframe(dplyr::group_by(stat_data, STATION_NUMBER, DayofYear),
                              Max = max(Value, na.rm = T),
                              Min = min(Value, na.rm = T),
                              Median = median(Value, na.rm = T),
                              Mean = mean(Value, na.rm = T),
                              P95 = quantile(Value, 0.95, na.rm = T),
                              P90 = quantile(Value, 0.90, na.rm = T),
                              P75 = quantile(Value, 0.75, na.rm = T),
                              P50 = quantile(Value, 0.50, na.rm = T),
                              P25 = quantile(Value, 0.25, na.rm = T),
                              P10 = quantile(Value, 0.10, na.rm = T),
                              P05 = quantile(Value, 0.05, na.rm = T))  
  
  # Filter flow_data to select_years argument
  
  select_flow_data <- dplyr::filter(flow_data, WaterYear %in% select_years)
  
  daily_stats <- merge(select_flow_data, stat_data, by = c("DayofYear", "STATION_NUMBER"))
  daily_stats <- dplyr::arrange(daily_stats, Date, STATION_NUMBER, DayofYear)
  daily_stats <- dplyr::rename(daily_stats, Year = WaterYear)
  daily_stats <- dplyr::select(daily_stats, -c("CalendarYear", "Month", "MonthName"))
  
  dplyr::as_tibble(daily_stats)
  
}
  

  

