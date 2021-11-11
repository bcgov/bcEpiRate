# Test ability to calculate normal confidence intervals ---------------------------

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
    get_ci_norm(interval = -1, estimate = 10, variance = 4),
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

df_norm <- data.frame(
  lower = 10 - qnorm(p = 1 - (0.05 / 2), mean = 0, sd = 1) * sqrt(4),
  upper = 10 + qnorm(p = 1 - (0.05 / 2), mean = 0, sd = 1) * sqrt(4)
)

test_that("function can calculate a single confidence interval using the normal distribution", {
  expect_equal(
    get_ci_norm(interval = 0.95, estimate = 10, variance = 4),
    df_norm
  )
})

df_norm_02 <- data.frame(
  lower = 20 - qnorm(p = 1 - (0.05 / 2), mean = 0, sd = 1) * sqrt(9),
  upper = 20 + qnorm(p = 1 - (0.05 / 2), mean = 0, sd = 1) * sqrt(9)
)

test_that("function can calculate confidence intervals for multiple estimates using normal distribution", {
  expect_equal(
    get_ci_norm(interval = 0.95, estimate = c(10, 20), variance = c(4, 9)),
    rbind(df_norm, df_norm_02)
  )
})

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

# Test ability to calculate log normal confidence intervals --------------------
test_that("function throws an error when `interval` is invalid", {
  expect_error(
    get_ci_lnorm(interval = "hello", estimate = 0.2, variance_log = 0.05),
    "`interval` must be numeric"
  )
  expect_error(
    get_ci_lnorm(interval = c(0.9, 0.95), estimate = 0.2, variance_log = 0.05),
    "`interval` must be a scalar"
  )
  expect_error(
    get_ci_lnorm(interval = -1, estimate = 0.2, variance_log = 0.05),
    "`interval` must be between 0 and 1"
  )
})

test_that("function throws an error when `estimate` is invalid", {
  expect_error(
    get_ci_lnorm(interval = 0.95, estimate = "hello", variance_log = 0.05),
    "`estimate` must be numeric"
  )
})

test_that("function throws an error when `variance_log` is invalid", {
  expect_error(
    get_ci_lnorm(interval = 0.95, estimate = 0.2, variance_log = "hello"),
    "`variance_log` must be numeric"
  )
})

test_that("function throws an error when length of `estimate` and `variance_log` don't match", {
  expect_error(
    get_ci_lnorm(interval = 0.95, estimate = c(0.2, 0.4), variance_log = 0.05),
    "length of `variance_log` must be equal to length of `estimate`"
  )
})

df_lnorm <- data.frame(
  lower = (log(0.4) - qnorm(p = 1 - (0.05 / 2)) * sqrt(0.025)) %>%
    exp(),
  upper = (log(0.4) + qnorm(p = 1 - (0.05 / 2)) * sqrt(0.025)) %>%
    exp()
)

test_that("function can calculate a log normal confidence interval", {
  expect_equal(
    get_ci_lnorm(interval = 0.95, estimate = 0.4, variance_log = 0.025),
    df_lnorm
  )
})

df_lnorm_02 <- data.frame(
  lower = (log(0.25) - qnorm(p = 1 - (0.05 / 2)) * sqrt(0.04)) %>%
    exp(),
  upper = (log(0.25) + qnorm(p = 1 - (0.05 / 2)) * sqrt(0.04)) %>%
    exp()
)

test_that("function can calculate confidence intervals for multiple estimates using log normal distribution", {
  expect_equal(
    get_ci_lnorm(interval = 0.95, estimate = c(0.4, 0.25), variance_log = c(0.025, 0.04)),
    rbind(df_lnorm, df_lnorm_02)
  )
})

# Test ability to calculate Poisson confidence intervals -----------------------

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
    get_ci_pois(interval = 0.95, x = c(0, 12, 3), y = c(18864, 13649, 19479)),
    "`x` must be a vector of positive values"
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
    get_ci_pois(interval = 0.95, x = c(78, 12, 3), y = c(0, 13649, 19479)),
    "`y` must be a vector of positive values"
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
    "length of `y` is not compatible with that of `x`"
  )
})

df_pois <- data.frame(
  lower = (stats::qchisq(p = 0.05 / 2, df = 2 * 97)) / (2 * 39570),
  upper = (stats::qchisq(p = 1 - (0.05 / 2), df = 2 * (97 + 1))) / (2 * 39570)
)

test_that("function can calculate a Poisson confidence interval", {
  expect_equal(
    get_ci_pois(interval = 0.95, x = 97, y = 39570),
    df_pois
  )
})

df_pois_02 <- data.frame(
  lower = (stats::qchisq(p = 0.05 / 2, df = 2 * 50)) / (2 * 47516),
  upper = (stats::qchisq(p = 1 - (0.05 / 2), df = 2 * (50 + 1))) / (2 * 47516)
)

test_that("function can calculate confidence intervals for multiple estimates using Poisson distribution", {
  expect_equal(
    get_ci_pois(interval = 0.95, x = c(97, 50), y = c(39570, 47516)),
    rbind(df_pois, df_pois_02)
  )
})

# TODO add check on count data

# Test ability to calculate gamma confidence intervals -----------------------

w <- c(3.7e-05, 2.2e-05, 2.0e-05, 1.2e-05, 2.3e-05,
       7.0e-05, 3.8e-05, 6.0e-05, 1.7e-05, 2.6e-05)

test_that("function throws an error when `interval` is invalid", {
  expect_error(get_ci_gamma(interval = "hello", estimate = 0.015,
                            weights = list(w), variance = 1e-06),
               "`interval` must be numeric")
  expect_error(get_ci_gamma(interval = c(0.9, 0.95), estimate = 0.015,
                            weights = list(w), variance = 1e-06),
               "`interval` must be a scalar")
  expect_error(get_ci_gamma(interval = -1, estimate = 0.015, weights = list(w), variance = 1e-06),
               "`interval` must be between 0 and 1")
})

test_that("function throws an error when `estimate` is invalid", {
  expect_error(get_ci_gamma(interval = 0.95, estimate = "hello", weights = list(w), variance = 1e-06),
               "`estimate` must be numeric")
})

test_that("function thrown an error when `weights` is invalid", {
  expect_error(get_ci_gamma(interval = 0.95, estimate = 0.015, weights = w, variance = 1e-06),
               "`weights` must be a list")
  expect_error(get_ci_gamma(interval = 0.95, estimate = 0.015,
                            weights = list(c("hello"), c("world")), variance = 1e-06),
               "`weights` must be a list of numeric vectors")
  expect_error(get_ci_gamma(interval = 0.9, estimate = 0.015,
                            weights = list(c(1), c("hello")), variance = 1e-06),
               "`weights` must be a list of numeric vectors")
})

test_that("function throws an error when `variance` is invalid", {
  expect_error(get_ci_gamma(interval = 0.9, estimate = 0.015, weights = list(w), variance = "hello"),
               "`variance` must be numeric")
})

test_that("function throws an error when `method` is invalid", {
  expect_error(get_ci_gamma(interval = 0.9,  estimate = 0.015,
                            weights = list(w), variance = 1e-06, method = "hello"),
               "`method` must be either 'tcz06' or 'ff97'")
})

test_that("function throws an error when input lengths are incompatible", {
  expect_error(get_ci_gamma(interval = 0.9, estimate = c(0.015, 0.004),
                            weights = list(w), variance = c(1e-06, 1e-08)),
               "length of `estimate`, `weights`, and `variance` must be the same")
})

test_that("function can correctly apply the Fay and Feuer method", {
  w_x <- max(w)
  lower_coef <- 1e-06 / (2 * 0.015)
  lower_df <- (2 * 0.015**2) / 1e-06
  lower <- lower_coef * stats::qchisq(p = 0.1 / 2, df = lower_df)
  upper_coef <- (1e-06 + w_x**2) / (2 * (0.015 + w_x))
  upper_df <- (2 * (0.015 + w_x)**2) / (1e-06 + w_x ** 2)
  upper <- upper_coef * stats::qchisq(p = 1 - (0.1 / 2), df= upper_df)
  result <- data.frame(lower, upper)

  expect_equal(get_ci_gamma(interval = 0.9, estimate = 0.015,
                            weights = list(w), variance = 1e-06, method = "ff97"),
               result)

})

w_m <- mean(w)
w_2m <- mean(w**2)
lower_coef <- 1e-06 / (2 * 0.015)
lower_df <- (2 * 0.015**2) / 1e-06
lower <- lower_coef * stats::qchisq(p = 0.1 / 2, df = lower_df)
upper_coef <- (1e-06 + w_2m) / (2 * (0.015 + w_m))
upper_df <- (2 * (0.015 + w_m)**2) / (1e-06 + w_2m)
upper <- upper_coef * stats::qchisq(p = 1 - (0.1 / 2), df = upper_df)
df_gamma <- data.frame(lower, upper)

test_that("function can correctly apply the Tiwari, Clegg, and Zou method (default)", {
  expect_equal(get_ci_gamma(interval = 0.9, estimate = 0.015,
                            weights = list(w), variance = 1e-06),
               df_gamma)
})

w_02 <- c(3.4e-06, 2.0e-06, 1.8e-06, 1.1e-06, 2.1e-06,
          6.3e-06, 3.5e-06, 5.4e-06, 1.5e-06, 2.3e-06)
w_m_02 <- mean(w_02)
w_2m_02 <- mean(w_02**2)
lower_coef_02 <- 1e-08 / (2 * 0.004)
lower_df_02 <- (2 * 0.004**2) / 1e-08
lower_02 <- lower_coef_02 * stats::qchisq(p = 0.1 / 2, df = lower_df_02)
upper_coef_02 <- (1e-08  + w_2m_02) / (2 * (0.004 + w_m_02))
upper_df_02 <- (2 * (0.004 + w_m_02)**2) / (1e-08 + w_2m_02)
upper_02 <- upper_coef_02 * stats::qchisq(p = 1 - (0.1 / 2), df = upper_df_02)
df_gamma_02 <- data.frame(lower = lower_02, upper = upper_02)

test_that("function can calculate confidence intervals for multiple estimates using gamma", {
  expect_equal(get_ci_gamma(interval = 0.9, estimate = c(0.015, 0.004),
                            weights = list(w, w_02), variance = c(1e-06, 1e-08)),
               rbind(df_gamma, df_gamma_02))
})

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
