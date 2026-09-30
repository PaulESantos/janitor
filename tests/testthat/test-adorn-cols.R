test_that("cols provides a named tidyselect interface for adorn functions", {
  cases <- data.frame(
    region = c("East", "West"),
    year = 2015,
    recovered = c(125, 87),
    died = c(13, 12)
  )

  expect_equal(
    adorn_percentages(cases, denominator = "col", cols = recovered:died),
    adorn_percentages(cases, "col", TRUE, recovered:died)
  )
  expect_equal(
    adorn_rounding(cases, cols = recovered:died),
    adorn_rounding(cases, 1, "half to even", recovered:died)
  )

  percentages <- adorn_percentages(cases, denominator = "col", cols = recovered:died)
  expect_equal(
    adorn_pct_formatting(percentages, cols = recovered:died),
    adorn_pct_formatting(percentages, 1, "half to even", TRUE, recovered:died)
  )

  formatted <- adorn_pct_formatting(percentages, cols = recovered:died)
  adorned_ns <- adorn_ns(formatted, cols = recovered:died)
  expect_equal(adorned_ns$year, formatted$year)
  expect_match(adorned_ns$recovered[[1]], "125", fixed = TRUE)
  expect_match(adorned_ns$died[[1]], "13", fixed = TRUE)
})

test_that("cols cannot be combined with legacy dots", {
  cases <- data.frame(region = c("East", "West"), recovered = c(125, 87), died = c(13, 12))

  expect_error(adorn_percentages(cases, cols = recovered, legacy = died), "either 'cols' or '...'", fixed = TRUE)
  expect_error(adorn_rounding(cases, cols = recovered, legacy = died), "either 'cols' or '...'", fixed = TRUE)
  expect_error(adorn_pct_formatting(cases, cols = recovered, legacy = died), "either 'cols' or '...'", fixed = TRUE)
  expect_error(adorn_ns(adorn_percentages(cases), cols = recovered, legacy = died), "either 'cols' or '...'", fixed = TRUE)
})

test_that("cols is propagated to each table in a three-way tabyl", {
  three <- tabyl(mtcars, cyl, am, gear)

  expect_equal(
    adorn_percentages(three, cols = all_of("0")),
    purrr::map(three, ~ adorn_percentages(.x, cols = all_of("0")))
  )
  expect_equal(
    adorn_rounding(three, cols = all_of("0")),
    purrr::map(three, ~ adorn_rounding(.x, cols = all_of("0")))
  )
})
