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

test_that("standardize_address() returns a corrected address", {
  response <- standardize_address(test_address, key = api_key)
  expect_equal(response$status, "OK")
  expect_equal(response$results$formatted_address, "1680 S Grand Ave, Pullman, WA 99163, USA")
})
