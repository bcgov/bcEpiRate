# TEST BASIC FUNCTIONALITY

# define necessary arguments
counts <- c(1, 2, 3, 4, 5)
popn <- c(10, 50, 100, 500, 1000)
df <- data.frame(counts, popn)

test_that("rates can be calculated when all arguments are valid", {
  expect_equal(get_spec_rt(counts, popn, scale = 1000), c(100, 40, 30, 8, 5))
  expect_equal(get_spec_rt(counts, popn, power = 3), c(100, 40, 30, 8, 5))
})

test_that("rates can be calculated when neither `scale` nor `power` is provided", {
  expect_equal(get_spec_rt(counts, popn), c(0.1, 0.04, 0.03, 0.008, 0.005))
})

test_that("rates can be calculated when vectors are extracted from a data frame", {
  expect_equal(get_spec_rt(df$counts, df$popn, scale = 1000), c(100, 40, 30, 8, 5))
})

test_that("function works with dplyr::mutate()", {
  df <- df %>%
    dplyr::mutate(spec_rate = get_spec_rt(counts, popn, power = 5))

  expect_equal(df$spec_rate, c(10000, 4000, 3000, 800, 500))
})

test_that("function throws an error when either vector isn't numeric", {
  char_vec <- c("apple", "banana", "carrot", "dragonfruit", "eggplant")

  expect_error(get_spec_rt(char_vec, popn), "`counts` must be a numeric vector")
  expect_error(get_spec_rt(counts, char_vec), "`popn` must be a numeric vector")
})

test_that("function throws an error when either vector contains negative elements", {
  counts_one_neg <- c(-1, 2, 3, 4, 5)
  counts_all_neg <- c(-1, -2, -3, -4, -5)
  popn_one_neg <- c(-10, 50, 100, 500, 1000)
  popn_all_neg <- c(-10, -50, -100, -500, -1000)

  expect_error(
    get_spec_rt(counts_one_neg, popn),
    "`counts` should contain non-negative elements"
  )
  expect_error(
    get_spec_rt(counts_all_neg, popn),
    "`counts` should contain non-negative elements"
  )
  expect_error(
    get_spec_rt(counts, popn_one_neg),
    "`popn` should contain non-negative elements"
  )
  expect_error(
    get_spec_rt(counts, popn_all_neg),
    "`popn` should contain non-negative elements"
  )
})

test_that("function throws an error when vectors aren't the same length", {
  expect_error(
    get_spec_rt(counts, popn[1:4]),
    "`counts` and `popn` must be the same length"
  )
})

test_that("function throws an error when both `scale` and `power` are supplied", {
  expect_error(
    get_spec_rt(counts, popn, scale = 100000, power = 5),
    "must supply exactly one of `scale` and `power`"
  )
})

test_that("function throws an error when `scale` isn't a single number", {
  expect_error(
    get_spec_rt(counts, popn, scale = c(1, 1000)),
    "`scale` must be a single number"
  )
})

test_that("function throws an error when `scale` isn't a positive whole number", {
  expect_error(
    get_spec_rt(counts, popn, scale = 0),
    "`scale` must be a positive whole number"
  )
  expect_error(
    get_spec_rt(counts, popn, scale = -1),
    "`scale` must be a positive whole number"
  )
  expect_error(
    get_spec_rt(counts, popn, scale = 0.1),
    "`scale` must be a positive whole number"
  )
})

test_that("function throws an error when `power` isn't a single number", {
  expect_error(get_spec_rt(counts, popn, power = c(1, 2, 3)),
               "`power` must be a single number")
})

test_that("function throws an error when `power` isn't a non-negative whole number", {
  expect_error(get_spec_rt(counts, popn, power = -1),
               "`power` must be a non-negative whole number")
  expect_error(get_spec_rt(counts, popn, power = 0.1),
               "`power` must be a non-negative whole number")
})

# define vectors with NA
counts_na <- c(NA, 2, 3, 4, 5)
popn_na <- c(NA, 50, 100, 500, 1000)

test_that("function raises a warning when one or more elements in the inputs are NA", {
  expect_warning(
    get_spec_rt(counts_na, popn),
    "one or more elements in `counts` and/or `popn` are NA, pass `output_status = TRUE` to check"
  )
  expect_warning(
    get_spec_rt(counts, popn_na),
    "one or more elements in `counts` and/or `popn` are NA, pass `output_status = TRUE` to check"
  )
  expect_warning(
    get_spec_rt(counts_na, popn_na),
    "one or more elements in `counts` and/or `popn` are NA, pass `output_status = TRUE` to check"
  )
})

# define vectors with 0
popn_0 <- c(0, 50, 100, 500, 1000)
popn_0s <- c(0, 0, 0, 0, 0)

test_that("function raises a warning when one or more elements in `popn` are 0", {
  expect_warning(
    get_spec_rt(counts, popn_0),
    "one or more elements in `popn` are 0, pass `output_status = TRUE` to check"
  )
  expect_warning(
    get_spec_rt(counts, popn_0s),
    "one or more elements in `popn` are 0, pass `output_status = TRUE` to check"
  )
})

test_that("function raises both warnings, if necessary", {
  warnings <- capture_warnings(get_spec_rt(counts_na, popn_0s))

  expect_equal(2, length(warnings))
  expect_equal(warnings[1],
               "one or more elements in `counts` and/or `popn` are NA, pass `output_status = TRUE` to check")
  expect_equal(warnings[2],
               "one or more elements in `popn` are 0, pass `output_status = TRUE` to check")
})

test_that("function outputs expected statuses", {
  expect_equal(
    get_spec_rt(counts, popn, output_status = TRUE)$status,
    c("success", "success", "success", "success", "success")
  )
  expect_equal(
    get_spec_rt(c(0, 0, 0, NA), c(0, NA, 10, 0), output_status = TRUE)$status,
    c("denom_0", "has_NA", "success", "has_NA")
  )
  expect_equal(
    get_spec_rt(c(NA, 10, 100, 1000), c(10, 0, NA, 10000), output_status = TRUE)$status,
    c("has_NA", "denom_0", "has_NA", "success")
  )
})

# TEST ABILITY TO CONSTRUCT CONFIDENCE INTERVALS

test_that("function throws a warning to reconsider using the normal distribution", {
  expect_warning(
    get_spec_rt(c(1, 8, 1, 8), c(1, 1, 100000, 100000), dist = "normal", interval = 0.95),
    "more than 0.15% of the probability mass lies below 0 for at least one of the estimates, consider using a different probability distribution"
  )
})

test_that("limits are NA when specific rates evaluate to 0", {
  counts_0 <- c(0, 50, 55, 60, 65)

  # using the normal distribution
  # suppressing warning saying this distribution should be avoided
  suppressWarnings(get_spec_rt(counts_0, popn, power = 3, dist = "normal", interval = 0.95)) %>%
    dplyr::mutate(is_expected = dplyr::if_else(
      rate == 0,
      is.na(lower) & is.na(upper), # if rate is 0, check that limits are NA
      is.finite(lower) & is.finite(upper)
    )) %>%
    # otherwise, check that limits are finite
    dplyr::pull(is_expected) %>%
    all() %>% # should be all TRUE
    expect_true()

  # using the log normal distribution
  get_spec_rt(counts_0, popn, scale = 100000, dist = "lognormal", interval = 0.9) %>%
    dplyr::mutate(is_expected = dplyr::if_else(
      rate == 0,
      is.na(lower) & is.na(upper), # if rate is 0, check that limits are NA
      is.finite(lower) & is.finite(upper)
    )) %>%
    # otherwise, check that limits are finite
    dplyr::pull(is_expected) %>%
    all() %>% # should be all TRUE
    expect_true()

  # using the Poisson distribution
  get_spec_rt(counts_0, popn, dist = "poisson", interval = 0.99) %>%
    dplyr::mutate(is_expected = dplyr::if_else(
      rate == 0,
      is.na(lower) & is.na(upper), # if rate is 0, check that limits are NA
      is.finite(lower) & is.finite(upper)
    )) %>%
    # otherwise, check that limits are finite
    dplyr::pull(is_expected) %>%
    all() %>% # should be all TRUE
    expect_true()
})

test_that("limits are NA when specific rates evaluate to NA", {
  # using the normal distribution
  suppressWarnings(get_spec_rt(c(1, NA, NA, 4, 5), popn, power = 3, dist = "normal", interval = 0.9)) %>%
    dplyr::mutate(is_expected = dplyr::if_else(
      is.na(rate),
      is.na(lower) & is.na(upper), # if rate is NA, check that limits are also NA
      is.finite(lower) & is.finite(upper)
    )) %>%
    # otherwise, limits should be finite numbers
    dplyr::pull(is_expected) %>%
    # expect all strata to evaluate to TRUE
    all() %>%
    expect_true()

  # using the Poisson distribution
  suppressWarnings(get_spec_rt(c(1, NA, NA, 4, 5), popn, power = 3, dist = "poisson", interval = 0.99)) %>%
    dplyr::mutate(is_expected = dplyr::if_else(
      is.na(rate),
      is.na(lower) & is.na(upper),
      is.finite(lower) & is.finite(upper)
    )) %>%
    dplyr::pull(is_expected) %>%
    all() %>%
    expect_true()
})

test_that("function throws an error when `dist` is supplied, but not `interval`, and vice versa", {
  expect_error(
    get_spec_rt(counts, popn, power = 3, interval = 0.95),
    "both `dist` and `interval` must be supplied to construct confidence intervals"
  )
  expect_error(
    get_spec_rt(counts, popn, power = 3, dist = "normal"),
    "both `dist` and `interval` must be supplied to construct confidence intervals"
  )
})

test_that("type and shape of output data are as expected based on input", {
  # Option A: no CIs, no output status
  output_a <- get_spec_rt(counts, popn, scale = 1000)
  expect_true(is.double(output_a))
  expect_false(is.data.frame(output_a))

  # Option B: no CIs, include output status
  output_b <- get_spec_rt(counts, popn, scale = 1000, output_status = TRUE)
  expect_true(is.data.frame(output_b))
  expect_equal(ncol(output_b), 2)
  expect_true(setequal(c("rate", "status"), colnames(output_b)))

  # Option C: CIs, no output status
  output_c <- get_spec_rt(c(30, 40, 50, 60, 70), c(1000, 1200, 1400, 1600, 1800), scale = 1000, dist = "normal", interval = 0.99)
  expect_true(is.data.frame(output_c))
  expect_equal(ncol(output_c), 4)
  expect_true(setequal(c("rate", "interval", "lower", "upper"), colnames(output_c)))

  # Option D: CIs, include output status
  output_d <- get_spec_rt(c(30, 40, 50, 60, 70), c(1000, 1200, 1400, 1600, 1800), scale = 1000, dist = "normal", interval = 0.99, output_status = TRUE)
  expect_true(is.data.frame(output_d))
  expect_equal(ncol(output_d), 5)
  expect_true(setequal(c("rate", "interval", "lower", "upper", "status"), colnames(output_d)))
})
