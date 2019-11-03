testthat::test_that("eq_create_label() Tests", {
  data <- data.frame(LOCATION_NAME = c("Location 1", "Location 2"),
                     EQ_PRIMARY = c(NA, 4),
                     TOTAL_DEATHS = c(10, NA),
                     OTHER = c("some text", "some more text"),
                     stringsAsFactors = FALSE)
  testthat::expect_that(AC.R.Capstone::eq_create_label(data), testthat::is_a("character"))
})

testthat::test_that("eq_map() Tests", {
  data <- data.frame(LATITUDE = c(123.45, -6.789, 0.0),
                     LONGITUDE = c(-1.23, 45.678, 0.0),
                     EQ_PRIMARY = c(7, 4.5, 5),
                     ANNOT_COL = c("INFO1", "INFO2", "INFO3"),
                     stringsAsFactors = FALSE)
  testthat::expect_that(AC.R.Capstone::eq_map(data, "ANNOT_COL"), testthat::is_a("leaflet"))
})
