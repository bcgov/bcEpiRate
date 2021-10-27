# Test ability to calculate (log) normal confidence intervals ---------------------------

test_that("function throws an error when `interval` is invalid", {
  expect_error(
    get_ci_norm(interval = "hello", estimate = 10, variance = 4),
    "`interval` must be numeric"
  )
  expect_error(
    get_ci_norm(interval = c(0.95, 0.975), estimate = 10, variance = 4),
    "`interval` must be a scalar"
  )
  expect_error(
    get_ci_norm(interval = -1, estimate = 10, variance),
    "`interval` must be between 0 and 1"
  )
})

test_that("function throws an error when `estimate` is invalid", {
  expect_error(
    get_ci_norm(interval = 0.9, estimate = "hello", variance = 4),
    "`estimate` must be numeric"
  )
})

test_that("function throws an error `variance` is invalid", {
  expect_error(
    get_ci_norm(interval = 0.9, estimate = 10, variance = "hello"),
    "`variance` must be numeric"
  )
  expect_error(
    get_ci_norm(interval = 0.9, estimate = 10, variance = -1),
    "`variance` must be greater than or equal to 0"
  )
})

test_that("function throws an error when length of `estimate` and `variance` don't match", {
  expect_error(
    get_ci_norm(interval = 0.9, estimate = 10, variance = c(4, 9)),
    "length of `variance` must be equal to length of `estimate`"
  )
})

test_that("function throws an error when `log` is invalid", {
  expect_error(
    get_ci_norm(interval = 0.9, estimate = 10, variance = 4, log = "hello"),
    "`log` must be logical"
  )
})

lower_a <- 10 - qnorm(p = 1 - (0.05 / 2), mean = 0, sd = 1) * sqrt(4)
upper_a <- 10 + qnorm(p = 1 - (0.05 / 2), mean = 0, sd = 1) * sqrt(4)
df_a <- data.frame(lower = lower_a, upper = upper_a)

test_that("function can calculate a single confidence interval using the normal distribution", {
  expect_equal(
    get_ci_norm(interval = 0.95, estimate = 10, variance = 4),
    df_a
  )
})

lower_b <- 20 - qnorm(p = 1 - (0.05 / 2), mean = 0, sd = 1) * sqrt(9)
upper_b <- 20 + qnorm(p = 1 - (0.05 / 2), mean = 0, sd = 1) * sqrt(9)
df_b <- data.frame(lower = lower_b, upper = upper_b)

test_that("function can calculate multiple confidence intervals using the normal distribution", {
  expect_equal(
    get_ci_norm(interval = 0.95, estimate = c(10, 20), variance = c(4, 9)),
    rbind(df_a, df_b)
  )
})

# TODO log normal

test_that("function throws a warning if the normal distribution should be avoided", {
  count <- 1
  popn <- 100000
  rate <- count / popn
  variance <- rate / popn

  expect_warning(
    get_ci_norm(interval = 0.95, estimate = rate, variance = variance),
    "more than 0.15% of the probability mass lies below 0 for at least one of the estimates, consider using a different probability distribution"
  )
})

# Test ability to calculate Poisson confidence intervals ---------------------------

test_that("function throws an error when `interval` is invalid", {
  expect_error(
    get_ci_pois(interval = "hello", x = 97, y = 39570),
    "`interval` must be numeric"
  )
  expect_error(
    get_ci_pois(interval = c(0.9, 0.95), x = 97, y = 39570),
    "`interval` must be a scalar"
  )
  expect_error(
    get_ci_pois(interval = -1, x = 97, y = 39570),
    "`interval` must be between 0 and 1"
  )
})

test_that("function throws an error when `x` is invalid", {
  expect_error(
    get_ci_pois(interval = 0.95, x = "hello", y = 39570),
    "`x` must be numeric"
  )
  expect_error(
    get_ci_pois(interval = 0.95, x = c(0.25, 0.07, 0.55), y = c(18864, 13649, 19479)),
    "`x` must be a vector of integers"
  )
  expect_error(
    get_ci_pois(interval = 0.95, x = c(-78, 12, 3), y = c(18864, 13649, 19479)),
      "`x` must be a vector of positive values"
    )
})

test_that("function throws an error when `x` or `y` is invalid", {
  expect_error(
    get_ci_pois(interval = 0.95, x = 97, y = "hello"),
    "`y` must be numeric"
  )
  expect_error(
    get_ci_pois(interval = 0.95, x = c(78, 12, 3), y = c(-18864, 13649, 19479)),
                "`y` must be a vector of positive values"
  )
})

test_that("function throws an error when length of `x` and `y` are incompatbile", {
  expect_error(
    get_ci_pois(interval = 0.95, x = c(97, 24, 50), y = c(39570, 40282)),
    "length of `y` is not compatible with that of `x`"
  )
  expect_error(
    get_ci_pois(interval = 0.9, x = c(97, 25, 50), y = 5),
    "expected `y = 1` given length of `x` and `y`"
  )
})

lower_c <- (stats::qchisq(p = 0.05 / 2, df = 2 * 97)) / (2 * 39570)
upper_c <- (stats::qchisq(p = 1 - (0.05 / 2), df = 2 * (97 + 1))) / (2 * 39570)
df_c <- data.frame(lower = lower_c, upper = upper_c)

test_that("function can calculate a Poisson confidence interval", {
  expect_equal(
    get_ci_pois(interval = 0.95, x = 97, y = 39570),
    df_c
  )
})

lower_d <- (stats::qchisq(p = 0.05 / 2, df = 2 * 50)) / (2 * 47516)
upper_d <- (stats::qchisq(p = 1 - (0.05 / 2), df = 2 * (50 + 1))) / (2 * 47516)
df_d <- data.frame(lower = lower_d, upper = upper_d)

test_that("function can calculate multiple Poisson confidence intervals", {
  expect_equal(
    get_ci_pois(interval = 0.95, x = c(97, 50), y = c(39570, 47516)),
    rbind(df_c, df_d)
  )
})

# TODO add check on count data

# Test ability to calculate Poisson confidence intervals -----------------------

test_that("function can detect probability distribution names", {
  expect_equal(
    get_dist_name("Normal"),
    get_dist_name("normal"),
    "normal"
  )
  expect_equal(
    get_dist_name("Log Normal"),
    get_dist_name("log normal"),
    get_dist_name("Log-normal"),
    get_dist_name("log-normal"),
    get_dist_name("lognormal"),
    "lognormal"
  )
  expect_equal(
    get_dist_name("Poisson"),
    get_dist_name("poisson"),
    "poisson"
  )
  expect_equal(
    get_dist_name("Gamma"),
    get_dist_name("gamma"),
    "gamma"
  )
})

test_that("function throws an error when user provides unsupported distribution name", {
  expect_error(get_dist_name("weibull"), "string does not match name of probability distribution supported by package")
})
