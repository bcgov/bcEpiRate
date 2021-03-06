# define necessary arguments
counts_a <- c(10, 7, 6, 4, 2) # non-problematic
counts_b <- c(10, 7, 6, 4, NA) # contains NA STRATUM 5

popn_a <- c(300, 595, 492, 43, 422) # non-problematic
popn_b <- c(NA, 595, 492, 43, 422) # contains NA in STRATUM 1

std_popn_a <- c(807, 355, 379, 244, 781) # non-problematic
std_popn_b <- c(0, 355, 379, 244, 781) # contains 0 in STRATUM 1
std_popn_c <- c(NA, 355, 379, 244, 781) # contains NA in STRATUM 1

# Test ability to calculate directly standardized rates ------------------------

test_that("directly standardized rate can be calculated when all arguments are valid", {
  expect_equal(get_ds_rt(counts_a, popn_a, std_popn_a, scale = 1000), 24.20012, tolerance = 1e-6)
  expect_equal(get_ds_rt(counts_a, popn_a, std_popn_a, power = 3), 24.20012, tolerance = 1e-6)
})

test_that("error is thrown when lengths are incompatible", {
  expect_error(
    get_ds_rt(counts_a, popn_a, std_popn_a[1:4]),
    "input vectors must be the same length"
  )
})

test_that("error is thrown when `std_popn` is not numeric", {
  expect_error(
    get_ds_rt(counts_a, popn_a, c("a", "b", "c", "d", "e")),
    "`std_popn` must be a numeric vector"
  )
})

test_that("warning is raised when 0 is found in `std_popn`", {
  expect_warning(
    get_ds_rt(counts_a, popn_a, std_popn_b),
    "at least one element in `std_popn` is 0"
  )
})

test_that("warning is raised when NA is found in `std_popn`", {
  expect_warning(
    get_ds_rt(counts_a, popn_a, std_popn_c),
    "at least one element in `std_popn` is NA"
  )
})

test_that("error is thrown when `output_type` is not valid", {
  expect_error(
    get_ds_rt(counts_a, popn_a, std_popn_a, output_type = "hello"),
    "`output_type` must be either 'rate' or 'counts'"
  )
})

test_that("error is thrown when both `scale` and `power` are provided", {
  expect_error(
    get_ds_rt(counts_a, popn_a, std_popn_a, scale = 1000, power = 3),
    "must supply exactly one of `scale` and `power`"
  )
})

test_that("error is thrown when `scale` is invalid", {
  expect_error(
    get_ds_rt(counts_a, popn_a, std_popn_a, scale = "hello"),
    "`scale` must be a positive integer of length 1"
  )
  expect_error(
    get_ds_rt(counts_a, popn_a, std_popn_a, scale = c(1000, 100000)),
    "`scale` must be a positive integer of length 1"
  )
  expect_error(
    get_ds_rt(counts_a, popn_a, std_popn_a, scale = 0),
    "`scale` must be a positive integer of length 1"
  )
  expect_error(
    get_ds_rt(counts_a, popn_a, std_popn_a, scale = 9999.99),
    "`scale` must be a positive integer of length 1"
  )
})

test_that("error is thrown when `power` is invalid", {
  expect_error(
    get_ds_rt(counts_a, popn_a, std_popn_a, power = "hello"),
    "`power` must be a non-negative integer of length 1"
  )
  expect_error(
    get_ds_rt(counts_a, popn_a, std_popn_a, power = c(3, 5)),
    "`power` must be a non-negative integer of length 1"
  )
  expect_error(
    get_ds_rt(counts_a, popn_a, std_popn_a, power = -1),
    "`power` must be a non-negative integer of length 1"
  )
  expect_error(
    get_ds_rt(counts_a, popn_a, std_popn_a, power = 3.5),
    "`power` must be a non-negative integer of length 1"
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

# define more necessary arguments to test edge cases
counts_c <- c(10, NA, 6, NA, 2) # contains NA in STRATUMS 2 AND 4
popn_c <- c(NA, 595, NA, 43, NA) # contains NA in STRATUMS 2 AND 4

counts_d <- c(NA, NA, NA, 4, 2) # contains NA in STRATUMS 1-3
popn_d <- c(300, 595, 492, 43, 422) # non-problematic
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

# Test ability to construct confidence intervals ---------------------------

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

test_that("function throws an error when user provides unsupported distribution name", {
  expect_error(
    get_ds_rt(counts_a, popn_a, std_popn_a, dist = "poisson", interval = 0.95),
    "for a directly standardized rate, `dist` should be one of 'normal', 'log normal', or 'gamma'"
  )
})

test_that("function throws an error when `method` is provided for (log) normal", {
  expect_error(
    get_ds_rt(counts_a, popn_a, std_popn_a, dist = "normal", interval = 0.9, method = "ff97"),
    "`method` must be NULL when `dist` is 'normal' or 'lognormal'"
  )
})

test_that("function cannot construct confidence intervals when `clean_strata = 'zero'", {
  expect_error(
    get_ds_rt(counts_a, popn_a, std_popn_a, clean_strata = "zero", dist = "normal", interval = 0.95),
    "construction of confidence interval with clean_strata = 'zero' not yet implemented"
  )
})

# define another set of non-problematic inputs to test compatibility with `dplyr::group_by()`
counts_e <- c(44, 29, 19, 50, 35)
popn_e <- c(2276, 1975, 2031, 4615, 3773)
std_popn_e <- c(13811, 11797, 10798, 17141, 19999)

# create a data frame to call `dplyr::group_by()` on
df_comb <- data.frame(
  grp = c(rep("a", 5), rep("e", 5)),
  counts = c(counts_a, counts_e),
  popn = c(popn_a, popn_e),
  std_popn = c(std_popn_a, std_popn_e)
)

# define variables for calculating confidence intervals

df <- data.frame(counts = counts_a, popn = popn_a, std_popn = std_popn_a) %>%
  dplyr::mutate(spec_rt = get_spec_rt(counts, popn))
dsr <- df %>%
  dplyr::summarise(estimate = get_ds_rt(counts, popn, std_popn)) %>%
  dplyr::pull(estimate)
var <- get_ds_rt_var(df)

df_02 <- data.frame(counts = counts_e, popn = popn_e, std_popn = std_popn_e) %>%
  dplyr::mutate(spec_rt = get_spec_rt(counts, popn))
dsr_02 <- df_02 %>%
  dplyr::summarise(estimate = get_ds_rt(counts, popn, std_popn)) %>%
  dplyr::pull(estimate)
var_02 <- get_ds_rt_var(df_02)

test_that("function can calculate normal confidence intervals", {
  df_norm <- data.frame(dsr = dsr) %>%
    dplyr::mutate(get_ci_norm(0.95, estimate = dsr, variance = var),
      interval = 0.95
    )

  # single confidence interval
  expect_equal(
    get_ds_rt(counts_a, popn_a, std_popn_a, dist = "normal", interval = 0.95),
    df_norm
  )

  # perform standardization again
  df_norm_02 <- data.frame(dsr = dsr_02) %>%
    dplyr::mutate(get_ci_norm(0.95, estimate = dsr_02, variance = var_02),
      interval = 0.95
    )

  # multiple confidence intervals
  expect_equal(
    df_comb %>%
      dplyr::group_by(grp) %>%
      dplyr::summarise(get_ds_rt(counts, popn, std_popn, dist = "normal", interval = 0.95),
        .groups = "drop"
      ),
    rbind(df_norm, df_norm_02) %>%
      dplyr::mutate(grp = c("a", "e")) %>%
      dplyr::relocate(grp) %>%
      tibble::as_tibble() # since summarise() returns a tibble
  )
})

test_that("function can calculate log normal confidence intervals", {
  var_log <- (1 / (dsr**2)) * var

  df_lnorm <- data.frame(dsr = dsr) %>%
    dplyr::mutate(get_ci_lnorm(0.95, estimate = dsr, variance = var_log),
      interval = 0.95
    )

  # single confidence interval
  expect_equal(
    get_ds_rt(counts_a, popn_a, std_popn_a, dist = "lognormal", interval = 0.95),
    df_lnorm
  )

  # perform standardization again
  var_log_02 <- (1 / (dsr_02**2)) * var_02
  df_lnorm_02 <- data.frame(dsr = dsr_02) %>%
    dplyr::mutate(get_ci_lnorm(0.95, estimate = dsr_02, variance = var_log_02),
      interval = 0.95
    )

  # multiple confidence intervals
  expect_equal(
    df_comb %>%
      dplyr::group_by(grp) %>%
      dplyr::summarise(get_ds_rt(counts, popn, std_popn,
        dist = "lognormal", interval = 0.95
      ),
      .groups = "drop"
      ),
    rbind(df_lnorm, df_lnorm_02) %>%
      dplyr::mutate(grp = c("a", "e")) %>%
      dplyr::relocate(grp) %>%
      tibble::as_tibble()
  )
})

# define `weights` for testing calculation of gamma confidence intervals
w <- df %>%
  dplyr::mutate(w_j = (std_popn / sum(std_popn)) * (1 / popn)) %>%
  dplyr::pull(w_j)

w_02 <- df_02 %>%
  dplyr::mutate(w_j = (std_popn / sum(std_popn)) * (1 / popn)) %>%
  dplyr::pull(w_j)

test_that("function can calculate gamma confidence intervals (Fey and Feuer)", {
  df_gamma <- data.frame(dsr = dsr) %>%
    dplyr::mutate(get_ci_gamma(0.9,
      estimate = dsr,
      weights = list(w), variance = var, method = "ff97"
    ),
    interval = 0.9
    )

  # single confidence interval
  expect_equal(
    get_ds_rt(counts_a, popn_a, std_popn_a, dist = "gamma", interval = 0.9, method = "ff97"),
    df_gamma
  )

  # perform standardization again
  df_gamma_02 <- data.frame(dsr = dsr_02) %>%
    dplyr::mutate(get_ci_gamma(0.9,
      estimate = dsr_02,
      weights = list(w_02), variance = var_02, method = "ff97"
    ),
    interval = 0.9
    )

  # multiple confidence intervals
  expect_equal(
    df_comb %>%
      dplyr::group_by(grp) %>%
      dplyr::summarise(get_ds_rt(counts, popn, std_popn,
        dist = "gamma", interval = 0.9, method = "ff97"
      ),
      .groups = "drop"
      ),
    rbind(df_gamma, df_gamma_02) %>%
      dplyr::mutate(grp = c("a", "e")) %>%
      dplyr::relocate(grp) %>%
      tibble::as_tibble()
  )
})

test_that("function can calculate gamma confidence intervals (Tiwari, Clegg, and Zou)", {
  df_gamma <- data.frame(dsr = dsr) %>%
    dplyr::mutate(get_ci_gamma(0.95,
      estimate = dsr,
      weights = list(w), variance = var
    ), # default `method` is 'tcz06'
    interval = 0.95
    )

  # single confidence interval
  expect_equal(
    get_ds_rt(counts_a, popn_a, std_popn_a, dist = "gamma", interval = 0.95),
    df_gamma
  )

  # perform standardization again
  df_gamma_02 <- data.frame(dsr = dsr_02) %>%
    dplyr::mutate(get_ci_gamma(0.95,
      estimate = dsr_02,
      weights = list(w_02), variance = var_02
    ),
    interval = 0.95
    )

  # multiple confidence intervals
  expect_equal(
    df_comb %>%
      dplyr::group_by(grp) %>%
      dplyr::summarise(get_ds_rt(counts, popn, std_popn,
        dist = "gamma", interval = 0.95
      ),
      .groups = "drop"
      ),
    rbind(df_gamma, df_gamma_02) %>%
      dplyr::mutate(grp = c("a", "e")) %>%
      dplyr::relocate(grp) %>%
      tibble::as_tibble()
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
