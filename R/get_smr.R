#' Calculate a standardized morbidity/mortality ratio
#'
#' @description Calculate a standardized morbidity/mortality ratio (SMR) or the
#' expected number of events in each stratum. The SMR is the number of observed
#' events divided by the expected events.
#'
#' @param count A scalar, the number of observed events.
#' @param popn A numeric vector containing the size of each stratum in the observed
#' population.
#' @param std_r A numeric vector containing the rate or risk of each stratum in
#' the standard population.
#' @param std_measure A string to indicate whether the values in `std_r` are rates
#' or risks, which is used to calculate the expected number of events in each stratum.
#' Currently, this function only supports the calculation of rate SMRs (default is `"rate"`).
#' The calculation of risk SMRs will be implemented in the future.
#' @param output_type If `"ratio"` (default), the SMR is returned. If `"counts"`,
#' the expected number of events in each stratum is returned.
#' @param percent If `FALSE` (default), the SMR is returned as is (e.g. 0.5). If
#' `TRUE`, the SMR is expressed as a percentage (e.g. 50).
#' @param dist A string to indicate the type of probability distribution for the
#' confidence interval to follow. Can be one of the following: `"normal"`,
#' `"log normal"` or `"poisson"`. This parameter is case insensitive.
#' @param interval A scalar, between 0 and 1, indicating the width of the
#' confidence interval. For example, for a 95% confidence interval, use
#' `interval = 0.95`.
#'
#' @details This low-level function assumes that the size of each stratum in the
#' observed population and the rate or risk of each stratum in the standard
#' population are provided in the same order.
#'
#' To construct confidence intervals for the ratio estimates, arguments must be
#' supplied to both `dist` and `interval`.
#'
#' @return If `output_type == "ratio"` (default) and neither `dist` nor `interval`
#' is supplied, the SMR is returned as a numeric vector.
#'
#' If `output_type == "ratio"` and both `dist` and `interval` are provided, then a
#' data frame with the columns `smr`, `lower`, `upper` and `interval` is returned.
#'
#' If `output_type == "counts"`, the expected number of events in each stratum
#' is returned as a numeric vector.
#'
#' @references \href{http://documentation.sas.com/doc/en/pgmsascdc/9.4_3.4/statug/statug_stdrate_details.htm}{The STDRATE Procedure}
#'
#' @examples
#' \dontrun{
#' count <- 1500
#' popn <- c(13358, 13029, 34378, 39649, 11671)
#' std_r <- c(0.004, 0.003, 0.007, 0.011, 0.013)
#'
#' get_smr(count = count, popn = popn, std_r = std_r, output_type = "ratio")
#' get_smr(count = count, popn = popn, std_r = std_r, output_type = "counts")
#' get_smr(count = count, popn = popn, std_r = std_r, percent = TRUE)
#' get_smr(
#'   count = count, popn = popn, std_r = std_r,
#'   dist = "normal", interval = 0.95
#' )
#' get_smr(
#'   count = count, popn = popn, std_r = std_r,
#'   percent = TRUE, dist = "normal", interval = 0.95
#' )
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @seealso [get_ci_norm()], [get_ci_lnorm()], [get_ci_pois()]
#' @export
get_smr <- function(count, popn, std_r, std_measure = "rate",
                    output_type = "ratio", percent = FALSE,
                    dist = NULL, interval = NULL) {

  # check validity of inputs

  if (!is.numeric(count)) {
    stop("`count` must be a non-negative integer of length 1")
  } else if (length(count) != 1) {
    stop("`count` must be a non-negative integer of length 1")
  } else if (count %% 1 != 0 | count < 0) {
    stop("`count` must be a non-negative integer of length 1")
  }

  if (length(popn) != length(std_r)) {
    stop("lengths of `popn` and `std_r` must be the same")
  }

  if (!is.numeric(popn)) {
    stop("`popn` must be a numeric vector of non-negative integers")
  } else if (any(popn < 0, na.rm = TRUE)) {
    stop("`popn` must be a numeric vector of non-negative integers")
  }

  # TODO: check if rates/risk are valid separately when risk SMR is added
  if (!is.numeric(std_r)) {
    stop("`std_r` must be a numeric vector")
  }

  # TODO: relax check when risk SMR is added
  if (std_measure != "rate") {
    stop("function currently only supports rates from the standard population")
  }

  if (!output_type %in% c("ratio", "counts")) {
    stop("`output_type` must be either 'ratio' or 'counts'")
  }

  if (!is.logical(percent)) {
    stop("`percent` must be a boolean")
  }

  # calculate expected number of events in each stratum
  expected <- popn * std_r

  # if desired, return expected number of events in each stratum
  if (output_type == "counts") {
    return(expected)
  }

  # proceed with calculation of SMR
  total_expected <- sum(expected)
  smr <- count / total_expected

  if (xor(is.null(dist), is.null(interval))) {
    stop("both `dist` and `interval` must be supplied to calculate confidence intervals")
  }

  if (!is.null(dist) & !is.null(interval)) {
    # check validity of distribution name, calculate confidence intervals
    dist_name <- get_dist_name(dist)
    if (dist_name == "normal") {
      if (std_measure == "rate") {
        var <- smr / total_expected
        ci <- get_ci_norm(interval = interval, estimate = smr, variance = var)
      }
      # TODO: add another block for calculating variance of risk SMR
    } else if (dist_name == "lognormal") {
      if (std_measure == "rate") {
        var_log <- 1 / count
        ci <- get_ci_lnorm(interval = interval, estimate = smr, variance_log = var_log)
      }
      # TODO: add another block for calculating variance of log risk SMR
    } else if (dist_name == "poisson") {
      ci <- get_ci_pois(interval = interval, x = count, y = total_expected)
    } else {
      stop("`dist` should be one of 'normal', 'lognormal' or 'poisson'")
    }

    # add confidence interval to output
    df <- data.frame(smr = smr) %>%
      dplyr::mutate(lower = ci$lower, upper = ci$upper, interval = interval)

    # if desired, express SMR as percentage
    if (percent) {
      df <- df %>%
        dplyr::mutate(dplyr::across(-interval, ~ .x * 100))
    }
    return(df)
  } else {
    # no confidence interval
    # if desired, express SMR as percentage
    if (percent) {
      smr <- smr * 100
    }
    return(smr)
  }
}
