testthat::test_that("geom_timeline() Tests", {
  data <- data.frame(DATE = c(lubridate::ymd("2000-12-15"), lubridate::ymd("2010-06-25"), lubridate::ymd("2015-03-05")),
                     COUNTRY = c("COUNTRY1", "COUNTRY2", "COUNTRY2"),
                     LATITUDE = c(123.45, -6.789, 0.0),
                     LONGITUDE = c(-1.23, 45.678, 0.0),
                     LOCATION_NAME = c("Location Name 1", "Location Name 2", "Location Name 3"),
                     EQ_PRIMARY = c(7, 5, 4),
                     DEATHS = c(10, 0, 0),
                     OTHER = c("some text", "some more text", "even more"),
                     stringsAsFactors = FALSE)
  testthat::expect_that(ggplot2::ggplot(data = data) +
                          AC.R.Capstone::geom_timeline(ggplot2::aes(x = DATE, y = COUNTRY, size = EQ_PRIMARY, fill = DEATHS),
                                        alpha = 0.25, xmin = lubridate::ymd("1995-01-01"), xmax = lubridate::ymd("2020-01-01")) +
                          AC.R.Capstone::theme_timeline,
                        testthat::is_a("ggplot"))
})


testthat::test_that("geom_timeline_label() Tests", {
  data <- data.frame(DATE = c(lubridate::ymd("1995-01-01"), lubridate::ymd("2015-01-01")),
                     LABEL = c("some text", "some more text"),
                     stringsAsFactors = FALSE)
  testthat::expect_that(ggplot2::ggplot(data = data) +
                          AC.R.Capstone::geom_timeline_label(ggplot2::aes(x = DATE, label = LABEL)) +
                          AC.R.Capstone::geom_timeline(ggplot2::aes(x = DATE, xmin = lubridate::ymd("1995-01-01"), xmax = lubridate::ymd("2020-01-01"))) +
                          AC.R.Capstone::theme_timeline,
                        testthat::is_a("ggplot"))
})


