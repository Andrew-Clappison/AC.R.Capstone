#' Plot an interactive map of Earthquake Locations
#'
#' Plots a leaflet map of the data with points at earthquake locations. Points can be clicked on to see additional information.
#'
#' @param data A data.frame containing earthquake information including EQ_PRIMARY, LONGITUDE, and LATITUDE
#' @param annot_col A String that names the field in the earthquake data.frame to be used for annotating the earthquake points
#'
#' @return A leaflet htmlwigit that is is a interactive map which can be viewed in a browser.
#'
#' @importFrom leaflet addTiles addCircleMarkers leaflet
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' db <- AC.R.Capstone::NOAA_eq_data
#' clean_data <- eq_clean_data(db)
#' select_data <- clean_data %>%
#'  dplyr::filter(COUNTRY %in% c("ARGENTINA")) %>%
#'  dplyr::mutate(label = eq_create_label(.))
#' AC.R.Capstone::eq_map(select_data, "label")
#' }
#'
#' @export
eq_map <- function(data, annot_col) {
  leaflet() %>%
    addTiles() %>%
    addCircleMarkers(data = data,
                     radius = ~EQ_PRIMARY,
                     lng = ~LONGITUDE,
                     lat = ~LATITUDE,
                     popup = ~get(annot_col))
}


#' Create HTML Labels
#'
#' Used to create leaflet labels written in HTML that contain earthquake information. It will return a label for each row of the input data.frame.
#'
#' @param data A data.frame that contains the columns LOCATION_NAME, EQ_PRIMARY, and TOTAL_DEATHS
#'
#' @return A Character string written in HTML code that can be used as a leaflet label
#'
#' @examples
#' \dontrun{
#'   db <- AC.R.Capstone::NOAA_eq_data
#'   clean_data <- AC.R.Capstone::eq_clean_data(db)
#'   clean_data$label <- AC.R.Capstone::eq_create_label(clean_data)
#' }
#'
#' @export
eq_create_label <- function(data) {
  data$popup_text <- ""
  for (i in 1:dim(data)[1]) {
    label <- ""
    if(!is.na(data$LOCATION_NAME[i])){
      label <- paste(label, "<b>Location:</b>", data$LOCATION_NAME[i],"<br />")
    }
    if(!is.na(data$EQ_PRIMARY[i])){
      label <- paste(label, "<b>Magnitude:</b>", data$EQ_PRIMARY[i],"<br />")
    }
    if(!is.na(data$TOTAL_DEATHS[i])){
      label <- paste(label, "<b>Total Deaths:</b>", data$TOTAL_DEATHS[i],"<br />")
    }
    data$popup_text[i] <- label
  }
  data$popup_text
}
