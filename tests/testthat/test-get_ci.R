normal_a <- qnorm(c(0.025, 0.975), 10, sqrt(4)) %>%
  data.frame() %>%
  data.table::transpose() %>%
  dplyr::rename(lower = 1, upper = 2)

normal_float <- qnorm(c(0.025, 0.975), 7.84, sqrt(1.434)) %>%
  data.frame() %>%
  data.table::transpose() %>%
  dplyr::rename(lower = 1, upper = 2)

mean_log_a <- log(10^2 / sqrt(10^2 + 4))
sd_log_a <- sqrt(log(1 + 4 / 10^2))

log_normal_a <- qlnorm(c(0.025, 0.975), mean_log_a, sd_log_a) %>%
  data.frame() %>%
  data.table::transpose() %>%
  dplyr::rename(lower = 1, upper = 2)

poisson_a <- data.frame(
  lower = qchisq(0.025, 2 * 30) / 2 / 100,
  upper = qchisq(0.975, 2 * (30 + 1)) / 2 / 100
)

test_that("function can use fuzzy matching to identify the probability distribution name", {
  expect_equal(normal_a, get_ci(dist = "Normal", interval = 0.95, estimate = 10, variance = 4))
  expect_equal(normal_a, get_ci(dist = "normal", interval = 0.95, estimate = 10, variance = 4))

  expect_equal(log_normal_a, get_ci(dist = "log normal", interval = 0.95, estimate = 10, variance = 4))
  expect_equal(log_normal_a, get_ci(dist = "lognormal", interval = 0.95, estimate = 10, variance = 4))
  expect_equal(log_normal_a, get_ci(dist = "log-normal", interval = 0.95, estimate = 10, variance = 4))
  expect_equal(log_normal_a, get_ci(dist = "Log-normal", interval = 0.95, estimate = 10, variance = 4))

  expect_equal(poisson_a, get_ci(dist = "Poisson", interval = 0.95, estimate = 30, denominator = 100))
  expect_equal(poisson_a, get_ci(dist = "poisson", interval = 0.95, estimate = 30, denominator = 100))
})

test_that("function can construct a confidence interval when `estimate` is a float for non-Poisson", {
  expect_equal(normal_float, get_ci(dist = "normal", interval = 0.95, estimate = 7.84, variance = 1.434))
})

test_that("function throws an error when an invalid probability distribution name is provided", {
  expect_error(
    get_ci(dist = "Weibull", interval = 0.95, estimate = 10, variance = 4),
    "`dist` must be one of 'normal', 'log normal', or 'poisson'"
  )
})

test_that("function throws an error when `interval` is invalid", {
  expect_error(
    get_ci(dist = "normal", interval = "hello", estimate = 10, variance = 4),
    "`interval` must be numeric"
  )
  expect_error(
    get_ci(dist = "normal", interval = c(0.9, 0.95), estimate = 10, variance = 4),
    "`interval` must be a scalar"
  )
  expect_error(
    get_ci(dist = "normal", interval = 1.05, estimate = 10, variance = 4),
    "`interval` must be between 0 and 1"
  )
  expect_error(
    get_ci(dist = "normal", interval = -0.05, estimate = 10, variance = 4),
    "`interval` must be between 0 and 1"
  )
})

test_that("function throws an error when `estimate` is invalid", {
  expect_error(
    get_ci(dist = "log-normal", interval = 0.9, estimate = "hello", variance = 9),
    "`estimate` must be numeric"
  )
})

test_that("function throws an error when `variance` is provided for Poisson", {
  expect_error(
    get_ci(dist = "poisson", interval = 0.95, estimate = 30, variance = 1, denominator = 100),
    "`variance` must be left empty when `dist` is 'poisson'"
  )
})

test_that("function requires `estimate` to be a whole number for Poisson", {
  expect_error(
    get_ci(dist = "poisson", interval = 0.9, estimate = 10.3, denominator = 1),
    "`estimate must be an integer if `dist` is 'poisson'"
  )
})

test_that("function requires `variance` to calculate interval for non-Poisson", {
  expect_error(
    get_ci(dist = "normal", interval = 0.95, estimate = 10),
    "`variance` is required to construct a confidence interval unless `dist` is 'poisson'"
  )
})

test_that("function throws an error when `variance` is invalid", {
  expect_error(
    get_ci(dist = "normal", interval = 0.95, estimate = 10, variance = "hello"),
    "`variance` must be numeric"
  )
  expect_error(
    get_ci(dist = "normal", interval = 0.95, estimate = 10, variance = -1),
    "`variance` must be greater than or equal to 0"
  )
})

test_that("function throws an error when lengths of `estimate` and `variance` don\'t match", {
  expect_error(
    get_ci(dist = "normal", interval = 0.95, estimate = 10, variance = c(4, 9, 16)),
    "length of `variance` must be equal to length of `estimate`"
  )
})

test_that("function throws an error when `denominator` is supplied for non-Poisson", {
  expect_error(
    get_ci(dist = "normal", interval = 0.95, estimate = 10, variance = 4, denominator = 100),
    "`denominator` must be NULL when `dist` is not 'poisson'"
  )
})

test_that("function recycles `denominator` when it doesn't match the length of `estimate`", {
  a <- get_ci(dist = "poisson", interval = 0.9, estimate = 50, denominator = 1000)
  b <- get_ci(dist = "poisson", interval = 0.9, estimate = 100, denominator = 1000)
  c <- get_ci(dist = "poisson", interval = 0.9, estimate = 200, denominator = 1000)
  result <- rbind(a, b, c)

  expect_equal(get_ci(dist = "poisson", interval = 0.9, estimate = c(50, 100, 200), denominator = 1000),
               result)
})

test_that("`lower` and `upper` are equal to `estimate` when `variance` is 0", {
  normal_var_0 <- get_ci(dist = "normal", interval = 0.95, estimate = 42, variance = 0)
  lognormal_var_0 <- get_ci(dist = "lognormal", interval = 0.95, estimate = 23, variance = 0)

  # using normal
  expect_equal(normal_var_0$lower, 42)
  expect_equal(normal_var_0$upper, 42)

  # using log normal
  expect_equal(lognormal_var_0$lower, 23)
  expect_equal(lognormal_var_0$upper, 23)
})

normal_b <- qnorm(c(0.025, 0.975), 20, sqrt(9)) %>%
  data.frame() %>%
  data.table::transpose() %>%
  dplyr::rename(lower = 1, upper = 2)

mean_log_b <- log(20^2 / sqrt(20^2 + 9))
sd_log_b <- sqrt(log(1 + 9 / 20^2))

log_normal_b <- qlnorm(c(0.025, 0.975), mean_log_b, sd_log_b) %>%
  data.frame() %>%
  data.table::transpose() %>%
  dplyr::rename(lower = 1, upper = 2)

poisson_b <- data.frame(
  lower = qchisq(0.025, 2 * 40) / 2 / 200,
  upper = qchisq(0.975, 2 * (40 + 1)) / 2 / 200
)

test_that("function can calculate multiple confidence intervals", {
  expect_equal(
    get_ci(dist = "normal", interval = 0.95, estimate = c(10, 20), variance = c(4, 9)),
    rbind(normal_a, normal_b)
  )
  expect_equal(
    get_ci(dist = "log normal", interval = 0.95, estimate = c(10, 20), variance = c(4, 9)),
    rbind(log_normal_a, log_normal_b)
  )
  expect_equal(
    get_ci(dist = "poisson", interval = 0.95, estimate = c(30, 40), denominator = c(100, 200)),
    rbind(poisson_a, poisson_b)
  )
})

