#' Calculate a (log) normal distribution confidence interval
#'
#' @description Calculate a (log) normal distribution confidence interval.
#'
#' @param interval A scalar between 0 and 1, indicating the width of the interval.
#' For example, use 0.95 to calculate a 95% confidence interval.
#' @param estimate A numeric vector containing the point estimates.
#' @param variance A numeric vector containing the variances.
#' @param log A Boolean to indicate whether to use the log normal distribution.
#' The default value is `FALSE`.
#'
#' @details This function doesn't need to know which type of epidemiological measure has
#' been passed to `estimate` (e.g., rate, risk). It also assumes that the values
#' of the input arguments `estimate` and `variance` are aligned and that the vectors
#' are the same length.
#'
#' This function is used to add confidence intervals in [get_spec_rt()] and [get_ds_rt()].
#'
#' @return A data frame with columns `lower` and `upper` which contain the lower
#' and upper limits of a confidence interval respectively.
#'
#' @references \href{http://documentation.sas.com/doc/en/pgmsascdc/9.4_3.4/statug/statug_stdrate_details.htm}{The STDRATE Procedure}
#'
#' @examples
#' \dontrun{
#' # calculate a single confidence interval
#' get_ci_norm(interval = 0.95, estimate = 10, variance = 4)
#' get_ci_norm(interval = 0.9, estimate = 20, variance = 9, log = TRUE)
#'
#' # calculate multiple confidence intervals
#' get_ci_norm(interval = 0.95, estimate = c(10, 20), variance = c(4, 9))
#'
#' # use columns of a data frame as inputs
#' df <- data.frame(estimate = c(10, 20, 30), variance = c(4, 9, 16))
#'
#' # using dplyr
#' df %>%
#'   dplyr::mutate(get_ci_norm(0.975, estimate, variance))
#'
#' # using base
#' get_ci_norm(interval = 0.975, estimate = df$estimate, variance = df$variance)
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
get_ci_norm <- function(interval, estimate, variance, log = FALSE) {
  # TODO integrate normal CI into functions that calculate risk, standardized risk, SMR, rate difference, and risk difference
  # TODO integrate log normal CI into functions that calculate risk, standardized risk, SMR, rate ratio, and risk ratio

  # check validity of inputs

  if (!is.numeric(interval)) {
    stop("`interval` must be numeric")
  }

  if (length(interval) != 1) {
    stop("`interval` must be a scalar")
  }

  if (interval < 0 | 1 < interval) {
    stop("`interval` must be between 0 and 1")
  }

  if (!is.numeric(estimate)) {
    stop("`estimate` must be numeric")
  }

  if (!is.numeric(variance)) {
    stop("`variance` must be numeric")
  }

  if (!all(variance >= 0, na.rm = TRUE)) {
    stop("`variance` must be greater than or equal to 0")
  }

  if (length(estimate) != length(variance)) {
    stop("length of `variance` must be equal to length of `estimate`")
  }

  if (!is.logical(log)) {
    stop("`log` must be logical")
  }

  # determine quantiles of interest
  alpha <- 1 - interval
  q_upper <- 1 - (alpha / 2)
  q_lower <- alpha / 2

  if (log) {
    # user chose log normal distribution
    # transformation based on https://msalganik.wordpress.com/2017/01/21/making-sense-of-the-rlnorm-function-in-r/comment-page-1/
    esimate_log <- log(estimate**2 / sqrt(estimate**2 + variance))
    sd_log <- sqrt(log(1 + (variance / estimate**2)))
    ci <- purrr::map2(esimate_log, sd_log, ~ stats::qlnorm(c(q_lower, q_upper), .x, .y))
  } else {
    # user chose normal distribution
    sd <- sqrt(variance)

    # check if distribution should be avoided
    threshold <- (1 - 0.997) / 2
    mass_below_0 <- stats::pnorm(0, mean = estimate, sd = sd)
    if (any(mass_below_0 > threshold, na.rm = TRUE)) {
      warning("more than 0.15% of the probability mass lies below 0 for at least one of the estimates, consider using a different probability distribution")
    }

    ci <- purrr::map2(estimate, sd, ~ stats::qnorm(c(q_lower, q_upper), .x, .y))
  }

  # wrangle output
  result <- ci %>%
    data.frame() %>%
    data.table::transpose() %>%
    dplyr::rename(lower = 1, upper = 2)

  return(result)
}


#' Calculate a Poisson distribution confidence interval
#'
#' @description Calculate a Poisson distribution confidence interval. This function
#' imitates [stats::qpois()] using [stats::qchisq()] to allow for the continuous
#' extension of the estimate.
#'
#' @param interval A scalar between 0 and 1, indicating the width of the interval.
#' For example, use 0.95 to calculate a 95% confidence interval.
#' @param x A numeric vector containing the stratum-specific number of events or
#' the observed number of events in the study population.
#' @param y A numeric vector containing the population-time for each stratum or
#' the expected number of events.
#'
#' @details When working with rates, pass the stratum-specific
#' number of events into `x` and the population-time for each stratum into `y`.
#' When working with standardized morbidity/mortality ratios, pass the observed
#' number of events into `x` and the expected number of events into `y`. Ensure
#' that the values of `x` and `y` are aligned and that the vectors are the same
#' length.
#'
#' Alternatively, to calculate a confidence interval for a rate, pass a vector
#' of pre-calculated rates into `x` and set `y = 1`. The shorter vector `y` will
#' be repeated so that its length matches that of `x`.
#'
#' This function is used to add confidence intervals in [get_spec_rt()].
#'
#' @return A data frame with columns `lower` and `upper` which contain the lower
#' and upper limits of a confidence interval respectively.
#'
#' @examples
#' \dontrun{
#' # calculate a single confidence interval
#' get_ci_pois(interval = 0.95, x = 49, y = 52)
#' get_ci_pois(interval = 0.95, x = 11, y = 1)
#'
#' # use columns of a data frame as inputs
#' df <- data.frame(x = c(24, 49), y = c(94, 52))
#'
#' # using dplyr
#' df %>%
#'   dplyr::mutate(get_ci_pois(0.95, x, y))
#'
#' # using base
#' get_ci_pois(0.95, df$x, df$y)
#' }
#'
#' @references \href{http://documentation.sas.com/doc/en/pgmsascdc/9.4_3.4/statug/statug_stdrate_details.htm}{The STDRATE Procedure}
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
get_ci_pois <- function(interval, x, y) {
  # TODO integrate Poisson CI into function that calculates SMR

  # check validity of inputs

  if (!is.numeric(interval)) {
    stop("`interval` must be numeric")
  }

  if (length(interval) != 1) {
    stop("`interval` must be a scalar")
  }

  if (interval < 0 | 1 < interval) {
    stop("`interval` must be between 0 and 1")
  }

  if (!is.numeric(x)) {
    stop("`x` must be numeric")
  }

  if (!is.numeric(y)) {
    stop("`y` must be numeric")
  }

  if (length(y) != 1 & length(y) != length(x)) {
    stop("length of `y` is not compatible with that of `x`")
  }

  # determine quantiles of interest
  alpha <- 1 - interval
  q_upper <- 1 - (alpha / 2)
  q_lower <- alpha / 2

  result <- data.frame(x = x, y = y) %>%
    dplyr::mutate(
      lower = stats::qchisq(q_lower, df = 2 * x) / (2 * y),
      upper = stats::qchisq(q_upper, df = 2 * (x + 1)) / (2 * y)
    ) %>%
    dplyr::select(lower, upper)

  return(result)
}


#' Calculate a gamma distribution confidence interval
#'
#' @description Derive an approximate gamma distribution confidence interval
#' using [stats::qchisq()].
#'
#' @param interval A scalar between 0 and 1, indicating the width of the interval.
#' For example, use 0.95 to calculate a 95% confidence interval.
#' @param estimate A numeric vector containing the point estimates.
#' @param weights A list of numeric vectors containing the weights.
#' @param variance A numeric vector containing the variances.
#' @param method A string to indicate which method to use to calculate the
#' confidence interval. The default is `"tcz06"` which refers to the method proposed
#' by Tiwari, Clegg, and Zou (2006). Use `"ff97"` for a more conservative confidence
#' interval proposed by Fay and Feuer (1997).
#'
#' @details This function assumes that the values of the input arguments `estimate`,
#' `weights`, and `variance` are aligned and that they are the same length.
#'
#' This function is used to add confidence intervals in [get_ds_rt()].
#'
#' @return A data frame with columns `lower` and `upper` which contain the lower
#' and upper limits of a confidence interval respectively.
#'
#' @references \href{http://documentation.sas.com/doc/en/pgmsascdc/9.4_3.4/statug/statug_stdrate_details.htm}{The STDRATE Procedure}
#'
#' @examples
#' \dontrun{
#' # calculate a single confidence interval
#' e_1 <- 0.015
#' w_1 <- c(
#'   3.7e-05, 2.2e-05, 2.0e-05, 1.2e-05, 2.3e-05,
#'   7.0e-05, 3.8e-05, 6.0e-05, 1.7e-05, 2.6e-05
#' )
#' v_1 <- c(1e-06)
#'
#' get_ci_gamma(interval = 0.95, estimate = e_1, weights = list(w_1), variance = v_1)
#' get_ci_gamma(interval = 0.9, estimate = e_1, weights = list(w_1), variance = v_1, method = "ff97")
#'
#' # calculate multiple confidence intervals
#' e_2 <- 0.004
#' w_2 <- c(
#'   3.4e-06, 2.0e-06, 1.8e-06, 1.1e-06, 2.1e-06,
#'   6.3e-06, 3.5e-06, 5.4e-06, 1.5e-06, 2.3e-06
#' )
#' v_2 <- 1e-08
#'
#' get_ci_gamma(
#'   interval = 0.9, estimate = c(e_1, e_2),
#'   weights = list(w_1, w_2), variance = c(v_1, v_2)
#' )
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
get_ci_gamma <- function(interval, estimate, weights, variance, method = "tcz06") {
  # check validity of inputs

  if (!is.numeric(interval)) {
    stop("`interval` must be numeric")
  }

  if (length(interval) != 1) {
    stop("`interval` must be a scalar")
  }

  if (interval < 0 | 1 < interval) {
    stop("`interval` must be between 0 and 1")
  }

  if (!is.numeric(estimate)) {
    stop("`estimate` must be numeric")
  }

  if (!is.list(weights)) {
    stop("`weights` must be a list")
  }

  if (!all(sapply(weights, is.numeric))) {
    stop("`weights` must be a list of numeric vectors")
  }

  if (!is.numeric(variance)) {
    stop("`variance` must be numeric")
  }

  if (!method %in% c("tcz06", "ff97")) {
    stop("`method` must be either 'tcz06' or 'ff97'")
  }

  # determine quantiles of interest
  alpha <- 1 - interval
  q_lower <- alpha / 2
  q_upper <- 1 - (alpha / 2)

  dt <- data.table::data.table(estimate = estimate, variance = variance, weights = weights)

  # define variables specific to method chosen by user
  if (method == "tcz06") { # default
    dt <- dt %>%
      dplyr::mutate(
        w = purrr::map_dbl(weights, mean), # w_j
        w_sq = purrr::map(weights, ~ .x**2) %>%
          purrr::map_dbl(mean)
      ) # w_j**2
  } else if (method == "ff97") { # less conservative method
    dt <- dt %>%
      dplyr::mutate(
        w = purrr::map_dbl(weights, max), # w_x
        w_sq = w**2
      ) # w_x**2
  }

  # construct confidence interval
  dt <- dt %>%
    dplyr::mutate(
      df_lower = (2 * (estimate**2)) / variance,
      df_upper = (2 * ((estimate + w)**2)) / (variance + w_sq),
      lower = (variance / (2 * estimate)) * stats::qchisq(q_lower, df_lower),
      upper = ((variance + w_sq) / (2 * (estimate + w))) * stats::qchisq(q_upper, df_upper)
    ) %>%
    dplyr::select(lower, upper)

  return(dt)
}


#' Use regex to detect a probability distribution name
#'
#' @param dist A string provided by the user
#'
#' @return One of "normal", "lognormal", "poisson", or "gamma". If there is no
#' match, an error is thrown.
#'
#' @keywords internal
get_dist_name  <- function(dist) {
  if (stringr::str_detect(dist, stringr::regex("^normal$", ignore_case = TRUE))) {
    name <- "normal"
  } else if (stringr::str_detect(dist, stringr::regex("^log(-|\\s)?normal$", ignore_case = TRUE))) {
    name <- "lognormal"
  } else if (stringr::str_detect(dist, stringr::regex("^poisson$", ignore_case = TRUE))) {
    name <- "poisson"
  } else if (stringr::str_detect(dist, stringr::regex("^gamma$", ignore_case = TRUE))) {
    name <- "gamma"
  } else {
    stop("string does not match name of probability distribution supported by package")
  }

  return(name)
}
