#' Clean Location Name
#'
#' This function takes in a data.frame and returns it back with the column called "LOCATION_NAME" cleaned.
#' Cleaned means the country name has been removed and the location name has been put into title case.
#'
#' @param db A data.frame of the eartquake database with a column called "LOCATION_NAME"
#'
#' @return An identical data.frame with the exception that the "LOCATION_NAME" is cleaned.
#'
#' @importFrom stringr str_to_title
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' db <- AC.R.Capstone::NOAA_eq_data
#' tail(db)
#' clean_data <- eq_location_clean(db)
#' tail(clean_data)
#' }
#'
#' @export
eq_location_clean <- function(db) {
  db$LOCATION_NAME <- sub("^.*:", "", db$LOCATION_NAME) %>% stringr::str_to_title()
  db
}


#' Clean data from NOAA's Significant Earthquake Database
#'
#' This function takes in data from NOAA's Significant Earthquake Database and cleans it into a form that the other functions in the AC.R.Capstone package take as input.
#' \itemize{
#'   \item LOCATION_NAME has country removed and is put in title case
#'   \item YEAR, MONTH, DAY is combined into a single field DATE
#'   \item LONGITUDE and LATITUDE are converted to numeric variables
#' }
#'
#' @param db A data.frame of the earthquake database with a columns "YEAR", "MONTH", "DAY", "LATITUE", "LONGITUDE", "LOCATION_NAME".
#'
#' @return A cleaned data.frame containing earthquake information
#'
#' @importFrom lubridate ymd
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#'
#' @examples
#' \dontrun{
#' db <- AC.R.Capstone::NOAA_eq_data
#' tail(db)
#' clean_data <- eq_clean_data(db)
#' tail(clean_data)
#' }
#'
#' @export
eq_clean_data <- function(db) {
  db <- db %>% tidyr::unite("DATE", YEAR, MONTH, DAY) %>%
    dplyr::mutate(DATE = lubridate::ymd(DATE))
  db$LATITUDE <- as.numeric(db$LATITUDE)
  db$LONGITUDE <- as.numeric(db$LONGITUDE)
  db <- eq_location_clean(db)
  db
}
