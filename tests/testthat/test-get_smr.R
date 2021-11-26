# define necessary arguments
count <- 1500
popn <- c(13358, 13029, 34378, 39649, 11671)
std_r <- c(0.004, 0.003, 0.007, 0.011, 0.013)

# test input validation ---------------------------

test_that("error thrown when `count` is invalid", {
  expect_error(get_smr(count = "hello", popn = popn, std_r = std_r),
               "`count` must be a non-negative integer of length 1")
  expect_error(get_smr(count = c(1500, 1785), popn = popn, std_r = std_r),
               "`count` must be a non-negative integer of length 1")
  expect_error(get_smr(count = -1, popn = popn, std_r = std_r),
               "`count` must be a non-negative integer of length 1")
  expect_error(get_smr(count = 0.1, popn = popn, std_r = std_r),
               "`count` must be a non-negative integer of length 1")
})

test_that("error thrown when input lengths are incompatible", {
  expect_error(get_smr(count = count, popn = popn, std_r = std_r[1:3]),
               "lengths of `popn` and `std_r` must be the same")
})


test_that("error thrown when `popn` is invalid", {
  expect_error(get_smr(count = count, popn = rep("hello", 5), std_r = std_r),
               "`popn` must be a numeric vector of non-negative integers")
  expect_error(get_smr(count = count, popn = popn * -1, std_r = std_r),
               "`popn` must be a numeric vector of non-negative integers")
})

# TODO: have different error messages when risk SMR is implemented
test_that("error thrown when `std_r` is invalid", {
  expect_error(get_smr(count = count, popn = popn, std_r = rep("hello", 5)),
              "`std_r` must be a numeric vector")
})

# TODO: relax test when risk SMR is implemented
test_that("error thrown when `measure` is invalid", {
  expect_error(get_smr(count = count, popn = popn, std_r = std_r, measure = "risk"),
               "function currently only supports rate-SMR")
})

test_that("error thrown when `output_type` is invalid", {
  expect_error(get_smr(count = count, popn = popn, std_r = std_r, output_type = "hello"),
               "`output_type` must be either 'ratio' or 'counts'")
})

test_that("error thrown when `percent` is invalid", {
  expect_error(get_smr(count = count, popn = popn, std_r = std_r, percent = "hello"),
               "`percent` must be a boolean")
})

# test rate SMR calculation ---------------------------
test_that("function can correctly calculate rate SMR", {
  smr <- count / sum(popn * std_r)
  expect_equal(smr, get_smr(count = count, popn = popn, std_r = std_r, output_type = "ratio"))
  expect_equal(smr * 100,
               get_smr(count = count, popn = popn, std_r = std_r,
                       output_type = "ratio", percent = TRUE))
})

# test expected counts calculation ---------------------------
test_that("function can correctly calculate expected number of events in each stratum", {
  expect_equal(popn * std_r,
               get_smr(count = count, popn = popn, std_r = std_r, output_type = "counts"))
})

# test confidence interval calculation ---------------------------
test_that("error thrown when `dist` is supplied, but not `interval`, and vice versa", {
  expect_error(get_smr(count = count, popn = popn, std_r = std_r, dist = "normal"),
               "both `dist` and `interval` must be supplied to calculate confidence intervals")
  expect_error(get_smr(count = count, popn = popn, std_r = std_r, interval = 0.9),
               "both `dist` and `interval` must be supplied to calculate confidence intervals")
})

test_that("function throws an error when user provides unsupported distribution name", {
  expect_error(get_smr(count = count, popn = popn, std_r = std_r, dist = "gamma", interval = 0.9),
               "`dist` should be one of 'normal', 'lognormal' or 'poisson'")
})

test_that("function can correctly calculate confidence intervals around rate-SMR estimates", {
  # normal
  expected <- sum(popn * std_r)
  smr <- count / expected
  var <- smr / expected
  interval <- 0.99
  df_norm <- get_ci_norm(interval = interval, estimate = smr, var = var) %>%
    dplyr::mutate(smr = smr, interval = interval) %>%
    dplyr::select(smr, lower, upper, interval)

  expect_equal(get_smr(count = count, popn = popn, std_r = std_r,
                       dist = "normal", interval = interval),
               df_norm)

  # log normal
  var_log <- 1 / count
  df_lnorm <- get_ci_lnorm(interval = interval, estimate = smr, variance_log = var_log) %>%
    dplyr::mutate(smr = smr, interval = interval) %>%
    dplyr::select(smr, lower, upper, interval)

  expect_equal(get_smr(count = count, popn = popn, std_r = std_r,
                       dist = "lognormal", interval = interval),
               df_lnorm)

  # poisson (express SMR as percentage)
  df_pois <- get_ci_pois(interval = interval, x = count, y = expected) %>%
    dplyr::mutate(smr = smr, interval = interval,
                  dplyr::across(-interval, ~ .x * 100)) %>%
    dplyr::select(smr, lower, upper, interval)

  expect_equal(get_smr(count = count, popn = popn, std_r = std_r, percent = TRUE,
                       dist = "poisson", interval = interval),
               df_pois)
})

# TODO: add more checks once risk SMR is implemented
test_that("type and shape of output data are expected based on input parameters", {
  # option 1: rate SMR
  out_01 <- get_smr(count = count, popn = popn, std_r = std_r)
  expect_true(is.double(out_01))
  expect_true(length(out_01) == 1)
  expect_false(is.data.frame(out_01))

  # option 2: expected number of events per stratum
  out_02 <- get_smr(count = count, popn = popn, std_r = std_r, output_type = "counts")
  expect_true(is.double(out_02))
  expect_equal(length(std_r), length(out_02))
  expect_false(is.data.frame(out_01))


  # option 3: rate SMR with CI
  out_03 <- get_smr(count = count, popn = popn, std_r = std_r, dist = "poisson", interval = 0.95)
  expect_true(is.data.frame(out_03))
  expect_equal(ncol(out_03), 4)
  expect_true(setequal(c("smr", "lower", "upper", "interval"), colnames(out_03)))
})
