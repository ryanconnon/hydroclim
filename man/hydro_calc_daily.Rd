% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hydro_calc_daily.R
\name{hydro_calc_daily}
\alias{hydro_calc_daily}
\title{hydro_calc_daily}
\usage{
hydro_calc_daily(
  station_number,
  parameter = "level",
  select_years = lubridate::year(Sys.Date()),
  after_bennett = FALSE,
  historic_min = NA,
  historic_max = NA,
  water_year_start = 1
)
}
\value{
A tibble of the daily values
}
\description{
Calculates daily values of hydrometric data, along with summary stats for each day of year
}
