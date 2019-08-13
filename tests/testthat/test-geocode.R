library(napr)
context("geocoding")

setup({
  api_key <- jsonlite::read_json(".secret")[['google_maps']]
  test_address <- "1680 South Grand Ave, Pullman, WA 99163"
})
teardown({
  unlink(api_key)
  unlink(test_address)
})

test_that("geocode_google() returns a corrected address", {
  response <- geocode_google(test_address, key = api_key)
  expect_equal(response$status, "OK")
  expect_equal(response$results$formatted_address, "1680 S Grand Ave, Pullman, WA 99163, USA")
})

test_that("geocode_census() returns a corrected address", {
  response <- geocode_census(test_address)
  expect_equal(response$result$addressMatches$matchedAddress, "1680 S GRAND AVE, PULLMAN, WA, 99163")
})