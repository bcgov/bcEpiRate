counts_a <- c(10, 7, 6, 4, 2) # unproblematic
counts_b <- c(10, 7, 6, 4, NA) # contains NA STRATUM 5

popn_a <- c(300, 595, 492, 43, 422) # unproblematic
popn_b <- c(NA, 595, 492, 43, 422) # contains NA in STRATUM 1

std_popn_a <- c(807, 355, 379, 244, 781) # unproblematic
std_popn_b <- c(0, 355, 379, 244, 781) # contains 0 in STRATUM 1
std_popn_c <- c(NA, 355, 379, 244, 781) # contains NA in STRATUM 1

# TEST ABILITY TO CALCULATE DIRECTLY STANDARDIZED RATE

test_that("directly standardized rate can be calculated when all arguments are valid", {
  expect_equal(get_ds_rt(counts_a, popn_a, std_popn_a, scale = 1000), 24.20012, tolerance = 1e-6)
  expect_equal(get_ds_rt(counts_a, popn_a, std_popn_a, power = 3), 24.20012, tolerance = 1e-6)
})

test_that("error is thrown when inputs are of varying lengths", {
  expect_error(
    get_ds_rt(counts_a, popn_a, c(1:10)),
    "input vectors must be the same length"
  )
})

test_that("error is thrown when `std_popn` is not numeric", {
  expect_error(
    get_ds_rt(counts_a, popn_a, c("a", "b", "c", "d", "e")),
    "`std_popn` must be a numeric vector"
  )
})

test_that("warning is raised when NA is found in `std_popn`", {
  expect_warning(
    get_ds_rt(counts_a, popn_a, std_popn_c),
    "at least one element in `std_popn` is NA"
  )
})

test_that("warning is raised when 0 is found in `std_popn`", {
  expect_warning(
    get_ds_rt(counts_a, popn_a, std_popn_b),
    "at least one element in `std_popn` is 0"
  )
})

test_that("error is thrown when `output_type` is not valid", {
  expect_error(
    get_ds_rt(counts_a, popn_a, std_popn_a, output_type = "hello"),
    "`output_type` must be either 'rate' or 'counts'"
  )
})

test_that("error is thrown when `clean_strata` is not valid", {
  expect_error(
    get_ds_rt(counts_a, popn_a, std_popn_a, clean_strata = "hello"),
    "`clean_strata` must be one of 'none', 'exclude' or 'zero'"
  )
})

test_that("rate calculated as expected when `clean_strata` == 'none' (default)", {
  # true rate if all inputs are valid
  expect_equal(get_ds_rt(counts_a, popn_a, std_popn_a), 0.02420012, tolerance = 1e-6)

  # NA otherwise
  expect_true(is.na(suppressWarnings(get_ds_rt(counts_a, popn_a, std_popn_b))))
  expect_true(is.na(suppressWarnings(get_ds_rt(counts_a, popn_a, std_popn_c))))
  expect_true(is.na(suppressWarnings(get_ds_rt(counts_a, popn_b, std_popn_a))))
  expect_true(is.na(suppressWarnings(get_ds_rt(counts_a, popn_b, std_popn_b))))
  expect_true(is.na(suppressWarnings(get_ds_rt(counts_a, popn_b, std_popn_c))))
})

test_that("expected counts are returned properly when `clean_strata` == 'none' (default)", {
  expect_equal(
    get_ds_rt(counts_a, popn_a, std_popn_a, output_type = "counts"),
    c(26.900000, 4.176471, 4.621951, 22.697674, 3.701422),
    tolerance = 1e-6
  )
  expect_equal(
    suppressWarnings(get_ds_rt(counts_a, popn_a, std_popn_b, output_type = "counts")),
    c(0.000000, 4.176471, 4.621951, 22.697674, 3.701422),
    tolerance = 1e-6
  )
  expect_equal(
    suppressWarnings(get_ds_rt(counts_a, popn_a, std_popn_c, output_type = "counts")),
    c(NA, 4.176471, 4.621951, 22.697674, 3.701422),
    tolerance = 1e-6
  )
  expect_equal(
    suppressWarnings(get_ds_rt(counts_b, popn_a, std_popn_a, output_type = "counts")),
    c(26.900000, 4.176471, 4.621951, 22.697674, NA),
    tolerance = 1e-6
  )
  expect_equal(
    suppressWarnings(get_ds_rt(counts_b, popn_a, std_popn_b, output_type = "counts")),
    c(0, 4.176471, 4.621951, 22.697674, NA),
    tolerance = 1e-6
  )
  expect_equal(
    suppressWarnings(get_ds_rt(counts_b, popn_a, std_popn_c, output_type = "counts")),
    c(NA, 4.176471, 4.621951, 22.697674, NA),
    tolerance = 1e-6
  )
})

test_that("rate calculated as expected when `clean_strata` == 'exclude'", {
  # true rate if all inputs are valid
  expect_equal(get_ds_rt(counts_a, popn_a, std_popn_a, clean_strata = "exclude"), 0.02420012, tolerance = 1e-6)
  expect_equal(
    get_ds_rt(counts_a, popn_a, std_popn_a),
    get_ds_rt(counts_a, popn_a, std_popn_a, clean_strata = "exclude"),
    tolerance = 1e-6
  )

  # exclude strata from calculation otherwise
  expect_equal(
    suppressWarnings(get_ds_rt(counts_a, popn_a, std_popn_b, clean_strata = "exclude")),
    get_ds_rt(counts_a[2:5], popn_a[2:5], std_popn_b[2:5]),
    tolerance = 1e-6
  )
  expect_equal(
    suppressWarnings(get_ds_rt(counts_a, popn_a, std_popn_b, clean_strata = "exclude")),
    0.02000996,
    tolerance = 1e-6
  )
  expect_equal(
    suppressWarnings(get_ds_rt(counts_a, popn_a, std_popn_c, clean_strata = "exclude")),
    0.02000996,
    tolerance = 1e-6
  )
  expect_equal(
    suppressWarnings(get_ds_rt(counts_b, popn_a, std_popn_a, clean_strata = "exclude")),
    0.0327149,
    tolerance = 1e-6
  )
  expect_equal(
    suppressWarnings(get_ds_rt(counts_b, popn_a, std_popn_b, clean_strata = "exclude")),
    0.0322046,
    tolerance = 1e-6
  )
  expect_equal(
    suppressWarnings(get_ds_rt(counts_b, popn_a, std_popn_c, clean_strata = "exclude")),
    0.0322046,
    tolerance = 1e-6
  )
})

test_that("expected counts are returned properly when `clean_strata` == 'exclude'", {
  expect_equal(
    get_ds_rt(counts_a, popn_a, std_popn_a, output_type = "counts", clean_strata = "exclude"),
    c(26.900000, 4.176471, 4.621951, 22.697674, 3.701422),
    tolerance = 1e-6
  )
  expect_equal(
    suppressWarnings(get_ds_rt(counts_a, popn_a, std_popn_b, output_type = "counts", clean_strata = "exclude")),
    c(0, 4.176471, 4.621951, 22.697674, 3.701422),
    tolerance = 1e-6
  )
  expect_equal(
    suppressWarnings(get_ds_rt(counts_a, popn_a, std_popn_c, output_type = "counts", clean_strata = "exclude")),
    c(NA, 4.176471, 4.621951, 22.697674, 3.701422),
    tolerance = 1e-6
  )
  expect_equal(
    suppressWarnings(get_ds_rt(counts_b, popn_a, std_popn_a, output_type = "counts", clean_strata = "exclude")),
    c(26.900000, 4.176471, 4.621951, 22.697674, NA),
    tolerance = 1e-6
  )
  expect_equal(
    suppressWarnings(get_ds_rt(counts_b, popn_a, std_popn_b, output_type = "counts", clean_strata = "exclude")),
    c(0, 4.176471, 4.621951, 22.697674, NA),
    tolerance = 1e-6
  )
  expect_equal(
    suppressWarnings(get_ds_rt(counts_b, popn_a, std_popn_c, output_type = "counts", clean_strata = "exclude")),
    c(NA, 4.176471, 4.621951, 22.697674, NA),
    tolerance = 1e-6
  )
})

test_that("expected counts are returned properly when `clean_strata` == 'zero'", {
  # true rate if all inputs are valid
  expect_equal(get_ds_rt(counts_a, popn_a, std_popn_a, clean_strata = "zero"), 0.02420012, tolerance = 1e-6)
  expect_equal(
    get_ds_rt(counts_a, popn_a, std_popn_a, clean_strata = "exclude"),
    get_ds_rt(counts_a, popn_a, std_popn_a, clean_strata = "zero"),
    tolerance = 1e-6
  )

  # exclude strata from calculation if `std_popn` == 0
  expect_equal(
    suppressWarnings(get_ds_rt(counts_a, popn_a, std_popn_b, clean_strata = "zero")),
    0.02000996,
    tolerance = 1e-6
  )
  expect_equal(
    suppressWarnings(get_ds_rt(counts_a, popn_a, std_popn_b, clean_strata = "zero")),
    suppressWarnings(get_ds_rt(counts_a, popn_a, std_popn_b, clean_strata = "exclude")),
    tolerance = 1e-6
  )
  expect_equal(
    suppressWarnings(get_ds_rt(counts_a, popn_b, std_popn_b, clean_strata = "zero")),
    0.02000996,
    tolerance = 1e-6
  )

  # set specific rate of NA to 0, then calculate rate
  expect_equal(
    suppressWarnings(get_ds_rt(counts_a, popn_b, std_popn_a, clean_strata = "zero")),
    0.01371688,
    tolerance = 1e-6
  )

  # return NA otherwise (i.e. NA in standard population)
  expect_true(is.na(suppressWarnings(get_ds_rt(counts_a, popn_a, std_popn_c, clean_strata = "zero"))))
  expect_true(is.na(suppressWarnings(get_ds_rt(counts_a, popn_b, std_popn_c, clean_strata = "zero"))))
})

test_that("expected counts are returned properly when `clean_strata` == zero", {
  expect_equal(
    suppressWarnings(get_ds_rt(counts_a, popn_a, std_popn_a, output_type = "counts", clean_strata = "zero")),
    c(26.900000, 4.176471, 4.621951, 22.697674, 3.701422),
    tolerance = 1e-6
  )
  expect_equal(
    suppressWarnings(get_ds_rt(counts_a, popn_a, std_popn_b, output_type = "counts", clean_strata = "zero")),
    c(0, 4.176471, 4.621951, 22.697674, 3.701422),
    tolerance = 1e-6
  )
  expect_equal(
    suppressWarnings(get_ds_rt(counts_a, popn_a, std_popn_c, output_type = "counts", clean_strata = "zero")),
    c(NA, 4.176471, 4.621951, 22.697674, 3.701422),
    tolerance = 1e-6
  )
  expect_equal(
    suppressWarnings(get_ds_rt(counts_a, popn_b, std_popn_a, output_type = "counts", clean_strata = "zero")),
    c(0.000000, 4.176471, 4.621951, 22.697674, 3.701422),
    tolerance = 1e-6
  )
  expect_equal(
    suppressWarnings(get_ds_rt(counts_a, popn_b, std_popn_b, output_type = "counts", clean_strata = "zero")),
    c(0.000000, 4.176471, 4.621951, 22.697674, 3.701422),
    tolerance = 1e-6
  )
  expect_equal(
    suppressWarnings(get_ds_rt(counts_a, popn_b, std_popn_c, output_type = "counts", clean_strata = "zero")),
    c(NA, 4.176471, 4.621951, 22.697674, 3.701422),
    tolerance = 1e-6
  )
})

counts_c <- c(10, NA, 6, NA, 2) # contains NA in STRATUMS 2 AND 4
popn_c <- c(NA, 595, NA, 43, NA) # contains NA in STRATUMS 2 AND 4

counts_d <- c(NA, NA, NA, 4, 2) # contains NA in STRATUMS 1-3
popn_d <- c(300, 595, 492, 43, 422) # unproblematic
std_popn_d <- c(807, 355, 379, NA, NA) # contains NA in STRATUMS 4 AND 5

test_that("edge cases are handled properly", {
  # all outputs of get_spec_rt is NA
  expect_true(is.na(suppressWarnings(get_ds_rt(counts_c, popn_c, std_popn_a))))
  expect_true(is.na(suppressWarnings(get_ds_rt(counts_c, popn_c, std_popn_a, clean_strata = "exclude")))) # all strata are excluded
  expect_equal(
    suppressWarnings(get_ds_rt(counts_c, popn_c, std_popn_a, clean_strata = "zero")),
    0,
    tolerance = 1e-6
  )

  # NA in each stratum (strata without NA in specific rate has NA in standard population)
  expect_true(is.na(suppressWarnings(get_ds_rt(counts_d, popn_d, std_popn_d))))
  expect_true(is.na(suppressWarnings(get_ds_rt(counts_d, popn_d, std_popn_d, clean_strata = "exclude"))))
  expect_true(is.na(suppressWarnings(get_ds_rt(counts_d, popn_d, std_popn_d, clean_strata = "zero"))))
})

# TEST ABILITY TO CONSTRUCT CONFIDENCE INTERVALS

test_that("function throws an error when `dist` is supplied, but not `interval`, and vice versa", {
  expect_error(
    get_ds_rt(counts_a, popn_a, std_popn_a, interval = 0.95),
    "both `dist` and `interval` must be supplied to construct confidence intervals"
  )
  expect_error(
    get_ds_rt(counts_a, popn_a, std_popn_a, dist = "normal"),
    "both `dist` and `interval` must be supplied to construct confidence intervals"
  )
})

test_that("function throws a warning when the use of the normal distribution should be reconsidered", {
  expect_warning(
    get_ds_rt(
      counts = c(1, 5, 10, 15, 30),
      popn = c(100, 100, 1200, 50000, 500000),
      std_popn = c(1, 2, 3, 4, 5),
      dist = "normal",
      interval = 0.95
    ),
    "more than 0.15% of the probability mass lies below 0 for the estimate, consider using a different probability distribution"
  )
})

test_that("function can only construct confidence intervals when `dist` is 'normal' or 'log normal'", {
  expect_error(
    get_ds_rt(counts_a, popn_a, std_popn_a, dist = "poisson", interval = 0.95),
    "this function can only construct confidence intervals using normal and log normal distributions"
  )
})

test_that("function cannot construct confidence intervals when `clean_strata = 'zero'", {
  expect_error(
    get_ds_rt(counts_a, popn_a, std_popn_a, clean_strata = "zero", dist = "normal", interval = 0.95),
    "construction of confidence interval with clean_strata = 'zero' not yet implemented"
  )
})

test_that("type and shape of output data are expected based on input parameters", {
  # Option A: only directly standardized rate
  output_a <- get_ds_rt(counts_a, popn_a, std_popn_a, scale = 1000)
  expect_true(is.double(output_a))
  expect_true(length(output_a) == 1)
  expect_false(is.data.frame(output_a))

  # Option B: directly standardized rate with confidence interval
  output_b <- get_ds_rt(counts_a, popn_a, std_popn_a, scale = 1000, dist = "log normal", interval = 0.95)
  expect_true(is.data.frame(output_b))
  expect_equal(ncol(output_b), 4)
  expect_true(setequal(c("dsr", "lower", "upper", "interval"), colnames(output_b)))

  # Option C: expected counts
  output_c <- get_ds_rt(counts_a, popn_a, std_popn_a, scale = 1000, output_type = "counts")
  expect_true(is.double(output_c))
  expect_true(purrr::map_dbl(list(counts_a, popn_a, std_popn_a, output_c), length) %>% unique() %>% length() == 1)
  expect_false(is.data.frame(output_c))
})
