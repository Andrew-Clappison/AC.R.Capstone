#' GeomTimeline
#'
#' This is the Geom used by geom_timeline()
#'
#' @importFrom ggplot2 ggproto draw_key_point Geom
#' @importFrom grid gTree gList pointsGrob segmentsGrob gpar
#'
GeomTimeline <- ggplot2::ggproto("GeomTimeline", ggplot2::Geom,
                        required_aes = c("x", "xmin", "xmax"),
                        default_aes = c(y = 1, fill = "grey", size = 1, alpha = 0.5),
                        draw_key = ggplot2::draw_key_point,
                        draw_panel = function(data, panel_params, coord){
                          data$y <- as.numeric(data$y)
                          coords <- coord$transform(data, panel_params)
                          tp <- grid::pointsGrob(
                            x = coords$x,
                            y = coords$y,
                            size = ggplot2::unit(coords$size, "mm"),
                            pch = 21,
                            gp = grid::gpar(col = coords$fill, fill = coords$fill, alpha = coords$alpha))
                          tl <- grid::segmentsGrob(
                            x0 = coords$xmin,
                            x1 = coords$xmax,
                            y0 = coords$y,
                            y1 = coords$y
                          )
                          gT <- grid::gTree(children = grid::gList(tl, tp))
                        })


#' Create a Timeline
#'
#' This timeline plots lines for each y category (typically would put country here), with points indicating the dates of earthquakes.
#' The size of the point can be used for the magnitude of the earthquake and the color for the number of deaths.
#' To match the plot style of the assignment, add the theme AC.R.Capstone::theme_timeline to the plot.
#'
#' @param mapping Using ggplot2::aes() to assign columns from data for use in the plot. Options relevant to this geom are:
#' \itemize{
#'   \item x - dates of points on the timeline
#'   \item xmin - date to start the timeline
#'   \item xmax - date to end the timeline
#'   \item y - variable to differentiate different lines of dates
#'   \item fill - variable to color the points by
#'   \item size - variable to determine the size of points
#'   \item alpha - the transparency of the points
#' }
#'
#' @param data a data.frame that will be used for the plot
#' @param ... Other parameters that are used by ggplot2::layer()
#'
#' @return A layer that can be added to a ggplot2 graph
#'
#' @importFrom ggplot2 layer
#'
#' @examples
#' \dontrun{
#' db <- AC.R.Capstone::NOAA_eq_data
#' clean_data <- eq_clean_data(db)
#' select_data <- clean_data %>%
#'   dplyr::filter(COUNTRY %in% c("ARGENTINA", "BRAZIL", "COLOMBIA")) %>%
#'   dplyr::filter(DATE >= lubridate::ymd("1950-01-01")) %>%
#'   dplyr::filter(DATE <= lubridate::ymd("1999-12-31"))
#' ggplot2::ggplot(data = select_data) +
#'   AC.R.Capstone::geom_timeline(mapping = ggplot2::aes(x = DATE, y = COUNTRY, size = EQ_PRIMARY, fill = DEATHS),
#'                              xmin = lubridate::ymd("1950-01-01"), xmax = lubridate::ymd("1999-12-31")) +
#'   AC.R.Capstone::theme_timeline
#' }
#'
#' @export
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimeline, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' GeomTimelineLabel
#'
#' This is the Geom used by geom_timeline_label()
#'
#' @importFrom ggplot2 ggproto draw_key_point
#' @importFrom grid gTree gList pointsGrob segmentsGrob gpar
#' @importFrom magrittr %>%
#'
GeomTimelineLabel <- ggplot2::ggproto("GeomTimelineLabel", ggplot2::Geom,
                             required_aes = c("x", "label"),
                             default_aes = c(y = 1, size = 1, n_max = 10),
                             draw_key = ggplot2::draw_key_point,
                             draw_panel = function(data, panel_params, coord){
                               data$y <- as.numeric(data$y)
                               coords <- coord$transform(data, panel_params)
                               data_ordered <- coords %>% dplyr::arrange(desc(size))
                               sel_num <- min(dim(data_ordered)[1], data_ordered$n_max[1])
                               coords <- data_ordered[1:sel_num, ]
                               sl <- grid::segmentsGrob(
                                 x0 = coords$x,
                                 x1 = coords$x,
                                 y0 = coords$y,
                                 y1 = coords$y + 0.05
                               )
                               st <- grid::textGrob(
                                 x = coords$x,
                                 y = coords$y,
                                 label = coords$label,
                                 vjust = -2,
                                 hjust = -.05,
                                 rot = 45
                               )
                               gT <- grid::gTree(children = grid::gList(sl, st))
                             })

#' Add labels to Timeline
#'
#' This geom can be used to add labels to points of a ggplot2 graph with a geom_timeline.
#'
#' @param mapping Using ggplot2::aes() to assign columns from data for use in the plot. Options relevant to this geom are:
#' \itemize{
#'   \item x - dates of points on the timeline
#'   \item label - the text field to use as a label
#'   \item y - variable to differentiate different lines of dates
#'   \item size - variable to determine the size of points
#'   \item n_max - the maximum number of points to label. Will label biggest points if the size variable is provided.
#' }
#'
#' @param data a data.frame that will be used for the plot
#' @param ... Other parameters that are used by ggplot2::layer()
#'
#' @return A layer that can be added to a ggplot2 graph
#'
#' @importFrom ggplot2 layer
#'
#' @examples
#' \dontrun{
#' db <- AC.R.Capstone::NOAA_eq_data
#' clean_data <- eq_clean_data(db)
#' select_data <- clean_data %>%
#'   dplyr::filter(COUNTRY %in% c("ARGENTINA")) %>%
#'   dplyr::filter(DATE >= lubridate::ymd("1950-01-01")) %>%
#'   dplyr::filter(DATE <= lubridate::ymd("1999-12-31"))
#' ggplot2::ggplot(data = select_data) +
#'   AC.R.Capstone::geom_timeline(mapping = ggplot2::aes(x = DATE, y = COUNTRY, size = EQ_PRIMARY, fill = DEATHS),
#'                              xmin = lubridate::ymd("1950-01-01"), xmax = lubridate::ymd("1999-12-31")) +
#'   AC.R.Capstone::geom_timeline_label(mapping = ggplot2::aes(x = DATE, label = LOCATION_NAME)) +
#'   AC.R.Capstone::theme_timeline
#' }
#'
#' @export
geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity",
                                position = "identity", na.rm = FALSE,
                                show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimelineLabel, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


#' @title Theme for timeline plots
#'
#' @description In order to have the timeline plots match the style of examples provided in the Mastering Software Development in R Course add this theme to the plot
#'
#' @importFrom ggplot2 theme_classic theme element_blank element_line
#'
#' @export
theme_timeline <- ggplot2::theme_classic() + ggplot2::theme(legend.position = "bottom", line = ggplot2::element_line(size = 1),
                                          axis.line.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
                                          axis.title.y = ggplot2::element_blank())
