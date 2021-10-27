#' #' Calculate a confidence interval
#' #'
#' #' @description Calculate a confidence interval given the estimate(s),
#' #' the variance(s), the width of the interval, and the type of probability
#' #' distribution to follow.
#' #'
#' #' @param dist A string to indicate the type of probability distribution
#' #' to follow. Can be one of the following: `"normal"`, `"log normal"`, or `"poisson"`.
#' #' This parameter is case insensitive.
#' #' @param interval A scalar, between 0 and 1, indicating the width of the interval.
#' #' For example, for a 95% confidence interval, use `interval = 0.95`.
#' #' @param estimate A numeric vector containing the point estimates. When constructing
#' #' a confidence interval using the Poisson distribution, `estimate` must be a (vector of)
#' #' whole number(s). When the Poisson confidence interval is on a *rate*, `estimate` is the numerator.
#' #' @param variance A numeric vector containing the variances. This parameter must
#' #' be left empty when constructing a confidence interval for a Poisson distribution.
#' #' @param denominator A numeric vector containing the denominator when constructing
#' #' a Poisson confidence interval on a *rate*. Default value is
#' #' `1` when `dist = "poisson"`. For other distributions, this parameter must be left empty.
#' #'
#' #' @details This low-level function doesn't need to know what the point estimates
#' #' represent (e.g. rate, ratio, etc.). It also assumes that the estimates and variances
#' #' are provided in the same order.
#' #'
#' #' `estimate` and `variance`, when provided, must have the same length. If `denominator` is
#' #' provided, its length must be 1 or equal to the length of `estimate`. If the length of
#' #' `denominator` is 1, R's [recycling rules](http://www.r-tutor.com/r-introduction/vector/vector-arithmetics) apply.
#' #'
#' #' The Poisson distribution is implemented using the chi-squared distribution to allow for
#' #' the continuous extension of integer variable (`estimate`).
#' #'
#' #' @return A data frame with 2 columns `upper` and `lower` which contain the upper
#' #' and lower bounds of a confidence interval respectively.
#' #'
#' #' @examples
#' #' \dontrun{
#' #' # calculate a single confidence interval
#' #' get_ci(dist = "normal", interval = 0.95, estimate = 10, variance = 4)
#' #' get_ci(dist = "log normal", interval = 0.9, estimate = 10, variance = 4)
#' #' get_ci(dist = "poisson", interval = 0.99, estimate = 30, denominator = 100)
#' #'
#' #' # calculate multiple confidence intervals
#' #' get_ci(dist = "normal", interval = 0.95, estimate = c(10, 20), variance = c(4, 9))
#' #'
#' #' # use a data frame to calculate confidence intervals
#' #' estimate <- c(10, 20, 30)
#' #' variance <- c(4, 9, 16)
#' #' df <- data.frame(estimate, variance)
#' #'
#' #' get_ci(dist = "normal", interval = 0.95, df$estimate, df$variance)
#' #' }
#' #'
#' #' @importFrom magrittr %>%
#' #' @importFrom rlang .data
#' #' @export
#' get_ci <- function(dist, interval, estimate, variance = NULL, denominator = NULL) {
#'
#'   # check validity of inputs
#'
#'   if (stringr::str_detect(dist, stringr::regex("^normal$", ignore_case = TRUE))) {
#'     dist <- "normal"
#'   } else if (stringr::str_detect(dist, stringr::regex("^log(-|\\s)?normal$", ignore_case = TRUE))) {
#'     dist <- "log normal"
#'   } else if (stringr::str_detect(dist, stringr::regex("^poisson$", ignore_case = TRUE))) {
#'     dist <- "poisson"
#'   } else {
#'     stop("`dist` must be one of 'normal', 'log normal', or 'poisson'")
#'   }
#'
#'   if (!is.numeric(interval)) {
#'     stop("`interval` must be numeric")
#'   }
#'
#'   if (length(interval) != 1) {
#'     stop("`interval` must be a scalar")
#'   }
#'
#'   if (interval < 0 | 1 < interval) {
#'     stop("`interval` must be between 0 and 1")
#'   }
#'
#'   if (!is.numeric(estimate)) {
#'     stop("`estimate` must be numeric")
#'   }
#'
#'   if (dist == "poisson") {
#'     if (!all(estimate %% 1 == 0, na.rm = TRUE)) {
#'       stop("`estimate must be an integer if `dist` is 'poisson'")
#'     }
#'
#'     if (!is.null(variance)) {
#'       stop("`variance` must be left empty when `dist` is 'poisson'")
#'     }
#'
#'     if (is.null(denominator)) {
#'       denominator <- 1
#'     }
#'   } else { # normal, log normal
#'
#'     if (is.null(variance)) {
#'       stop("`variance` is required to construct a confidence interval unless `dist` is 'poisson'")
#'     }
#'
#'     if (!is.numeric(variance)) {
#'       stop("`variance` must be numeric")
#'     }
#'
#'     if (!all(variance >= 0, na.rm = TRUE)) {
#'       stop("`variance` must be greater than or equal to 0")
#'     }
#'
#'     if (length(estimate) != length(variance)) {
#'       stop("length of `variance` must be equal to length of `estimate`")
#'     }
#'
#'     if (!is.null(denominator)) {
#'       stop("`denominator` must be NULL when `dist` is not 'poisson'")
#'     }
#'   }
#'
#'   # determine limit quantiles
#'
#'   alpha <- 1 - interval
#'   q_low <- alpha / 2
#'   q_high <- 1 - (alpha / 2)
#'
#'   # construct confidence intervals
#'
#'   if (dist == "normal") {
#'     sd <- sqrt(variance)
#'     ci <- purrr::map2(estimate, sd, ~ stats::qnorm(c(q_low, q_high), .x, .y))
#'   } else if (dist == "log normal") {
#'     # transformation based on:
#'     # https://en.wikipedia.org/wiki/Log-normal_distribution#Generation_and_parameters
#'     # https://msalganik.wordpress.com/2017/01/21/making-sense-of-the-rlnorm-function-in-r/comment-page-1/
#'     mean_log <- log(estimate^2 / sqrt(estimate^2 + variance))
#'     sd_log <- sqrt(log(1 + (variance / estimate^2)))
#'     ci <- purrr::map2(mean_log, sd_log, ~ stats::qlnorm(c(q_low, q_high), .x, .y))
#'   } else if (dist == "poisson") {
#'     ci <- purrr::map2(estimate, denominator, ~ get_ci_pois(q_low, q_high, .x, .y))
#'   }
#'
#'   # wrangle output
#'   result <- ci %>%
#'     data.frame() %>%
#'     data.table::transpose() %>%
#'     dplyr::rename(lower = 1, upper = 2)
#'
#'   return(result)
#' }
#'
#'
#' #' Replicate `stats::qpois` using `stats::qchisq`
#' #'
#' #' @description This function implements the Poisson distribution using the
#' #' chi-squared distribution to allow for the continuous extension of integer
#' #' variable (`estimate`). Read p.9385 of the [STDRATE Procedure](https://support.sas.com/documentation/onlinedoc/stat/151/stdrate.pdf)
#' #' for more information.
#' #'
#' #' @param estimate A scalar, must be a whole number (e.g. number of events)
#' #' @param denominator A scalar (e.g. population-time)
#' #' @param q_low A scalar, indicating the lower limit
#' #' @param q_high A scalar, indicating the upper limit
#' #'
#' #' @return A numeric vector containing the lower and upper limits
#' #'
#' #' @examples
#' #' \dontrun{
#' #' get_ci_pois(q_low = 0.025, q_high = 0.975, estimate = 30, denominator = 1000)
#' #' }
#' #'
#' #' @keywords internal
#' # get_ci_pois <- function(q_low, q_high, estimate, denominator) {
#' #   result <- stats::qchisq(c(q_low, q_high), df = c(2 * estimate, 2 * (estimate + 1))) / 2 / denominator
#' #
#' #   return(result)
#' # }
