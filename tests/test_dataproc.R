testthat::test_that("eq_location_clean() tests", {
  loc <- data.frame(LOCATION_NAME = "COUNTRY: LOCATION NAME", stringsAsFactors = FALSE)
  testthat::expect_that(AC.R.Capstone::eq_location_clean(loc),
                        testthat::is_identical_to(data.frame(LOCATION_NAME = " Location Name", stringsAsFactors = FALSE)))
})

testthat::test_that("eq_clean_data() tests", {
  data <- data.frame(YEAR = c(2000, 2010),
                     MONTH = c(12, 6),
                     DAY = c(15, 25),
                     LATITUDE = c("123.45", "-6.789"),
                     LONGITUDE = c("-1.23", "45.678"),
                     LOCATION_NAME = c("COUNTRY: LOCATION NAME", "COUNTRY 2: LOCATION NAME 2"),
                     OTHER = c("some text", "some more text"),
                     stringsAsFactors = FALSE)
  testthat::expect_that(AC.R.Capstone::eq_clean_data(data)$LATITUDE, testthat::is_a("numeric"))
  testthat::expect_that(AC.R.Capstone::eq_clean_data(data)$DATE, testthat::is_a("Date"))
})
