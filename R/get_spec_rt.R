#' Calculate specific rates
#'
#' @description
#' Calculate specific rates given a vector containing the frequencies of events
#' and a vector containing the sizes of the defined populations.
#'
#' @param counts A numeric vector containing the frequency of events.
#' @param popn A numeric vector containing the size of each defined population.
#' @param scale,power Provide either `scale`, a positive integer, to specify the
#' scale, or `power`, a non-negative integer, to specify the power of 10 to use
#' when expressing the result. If neither are supplied, `scale = 1` will be
#' applied.
#' @param output_status If `FALSE` (default) the calculated rates are returned
#' as a numeric vector. If `TRUE` a data frame containing both the rates and
#' the validity of the corresponding elements in the input vectors is returned.
#' @param dist A string to indicate the type of probability distribution for the
#' confidence interval to follow. Can be one of the following: `"normal"`,
#' `"log normal"` or `"poisson"`. This parameter is case insensitive.
#' @param interval A scalar, between 0 and 1, indicating the width of the
#' confidence interval. For example, for a 95% confidence interval, use
#' `interval = 0.95`.
#'
#' @details This low-level function assumes that counts and sizes of respective
#' subpopulations are provided in the same order. It can also be used to
#' calculate a crude rate if `counts` and `popn` are scalars and represent the
#' sum of all counts and the size of the entire population respectively.
#'
#' To construct confidence intervals for the rate estimates, arguments must be
#' explicitly supplied to both `dist` and `interval`.
#'
#' @return
#' If `output_status = FALSE` and neither `dist` or `interval` is supplied, a
#' numeric vector is returned.
#'
#' Otherwise, a data frame is returned with a column for `rate` and up to 4
#' additional columns:
#'  * If `output_status = TRUE`, then the column `status` is included.
#'  * If both `dist` and `interval` are provided, then the columns `interval`,
#'  `lower`, and `upper` are also included.
#'
#' The `status` column of the output can be interpreted as follows:
#' * `"has_NA"` indicates that the numerator and/or denominator of the fraction
#' was NA.
#' * `"denom_0"` indicates that the denominator of the fraction was 0.
#' * `"success"` indicates that both the numerator and denominator of the
#' fraction were valid numbers.
#'
#' @references \href{http://documentation.sas.com/doc/en/pgmsascdc/9.4_3.4/statug/statug_stdrate_details.htm}{The STDRATE Procedure}
#'
#' @examples
#' \dontrun{
#' # using vectors
#' counts <- c(100, 200, 300, 400, 500)
#' popn <- c(1000, 5000, 10000, 50000, 100000)
#'
#' get_spec_rt(counts, popn)
#' get_spec_rt(counts, popn, scale = 1000)
#' get_spec_rt(counts, popn, power = 3)
#'
#' # get output status
#' get_spec_rt(counts, popn, output_status = TRUE)
#'
#' # construct confidence intervals for rate estimates
#' get_spec_rt(counts, popn, dist = "normal", interval = 0.95)
#'
#' # using a data frame
#' df <- data.frame(counts, popn)
#'
#' get_spec_rt(df$counts, df$popn, scale = 100000)
#' get_spec_rt(df$counts, df$popn, power = 5)
#'
#' # using dplyr to create a column
#' df <- df %>%
#'   dplyr::mutate(spec_rate = get_spec_rt(counts, popn, scale = 1000))
#' }
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @seealso [get_ci_norm()], [get_ci_lnorm()], [get_ci_pois()]
#' @export
get_spec_rt <- function(counts, popn, scale = NULL, power = NULL, output_status = FALSE,
                        dist = NULL, interval = NULL) {

  # check for valid inputs
  if (!is.numeric(counts)) {
    stop("`counts` must be a numeric vector of non-negative integers")
  } else if (any(counts < 0, na.rm = TRUE)) {
    stop("`counts` must be a numeric vector of non-negative integers")
  }

  if (!is.numeric(popn)) {
    stop("`popn` must be a numeric vector of non-negative integers")
  } else if (any(popn < 0, na.rm = TRUE)) {
    stop("`popn` must be a numeric vector of non-negative integers")
  }

  if (length(counts) != length(popn)) {
    stop("`counts` and `popn` must be the same length")
  }

  if (!is.null(scale) & !is.null(power)) {
    stop("must supply exactly one of `scale` and `power`")
  }

  # set multiplier
  if (!is.null(scale)) { # if only `scale` is supplied
    if (!is.numeric(scale)) {
      stop("`scale` must be a positive integer of length 1")
    }
    if (length(scale) != 1) {
      stop("`scale` must be a positive integer of length 1")
    }

    if (scale %% 1 != 0 | scale < 1) {
      stop("`scale` must be a positive integer of length 1")
    }

    multiplier <- scale
  } else if (!is.null(power)) { # if only `power` is supplied
    if(!is.numeric(power)) {
      stop("`power` must be a non-negative integer of length 1")
    }

    if (length(power) != 1) {
      stop("`power` must be a non-negative integer of length 1")
    }

    if (power %% 1 != 0 | power < 0) {
      stop("`power` must be a non-negative integer of length 1")
    }

    multiplier <- 10**power
  } else { # if neither `scale` nor `power` is supplied, return the rates as is (i.e. `scale` = 1)
    multiplier <- 1
  }

  # calculate rates
  result <- purrr::map2_dfr(counts, popn, get_single_rt) %>%
    dplyr::mutate(status = as.character(.data$status))

  if (xor(is.null(dist), is.null(interval))) {
    stop("both `dist` and `interval` must be supplied to construct confidence intervals")
  }

  if (!is.null(dist) & !is.null(interval)) {
    # check validity of distribution name and calculate confidence interval
    dist_name <- get_dist_name(dist)
    if (dist_name == "normal") {
      var <- result$rate / popn
      ci <- get_ci_norm(interval = interval, estimate = result$rate, variance = var)
    } else if (dist_name == "lognormal") {
      var_log <- 1 / counts
      ci <- get_ci_lnorm(interval = interval, estimate = result$rate, variance_log = var_log)
    } else if (dist_name == "poisson") {
      ci <- get_ci_pois(interval = interval, x = counts, y = popn)
    } else {
      stop("`dist` should be one of 'normal', 'log normal', or 'poisson'")
    }

    # add confidence interval to output
    result <- result %>%
      dplyr::mutate(lower = ci$lower, upper = ci$upper, interval = interval) %>%
      dplyr::mutate( # overwrite limits with NA when `rate` is 0 (not applicable to Poisson)
        lower = dplyr::if_else(.data$rate == 0, NA_real_, .data$lower),
        upper = dplyr::if_else(.data$rate == 0, NA_real_, .data$upper)
      ) %>%
      dplyr::mutate( # apply multiplier to limits
        lower = .data$lower * multiplier,
        upper = .data$upper * multiplier
      ) %>%
      dplyr::select(.data$rate, .data$interval, .data$lower, .data$upper, .data$status)
  }

  # apply multiplier to rate
  result <- result %>%
    dplyr::mutate(rate = .data$rate * multiplier)

  # return specified outputs
  if (output_status) {
    return(result)
  } else { # don't output status
    # raise warnings to get user to inspect elements, if necessary
    if ("has_NA" %in% result$status) {
      warning("one or more elements in `counts` and/or `popn` are NA, pass `output_status = TRUE` to check")
    }

    if ("denom_0" %in% result$status) {
      warning("one or more elements in `popn` are 0, pass `output_status = TRUE` to check")
    }

    if ("interval" %in% colnames(result)) {
      result <- result %>%
        dplyr::select(-.data$status)
      return(result)
    } else { # confidence intervals not constructed
      return(result$rate)
    }
  }
}

#' Calculate a single rate and evaluate the validity of the output
#'
#' @param x The numerator of the rate
#' @param y The denominator of the rate
#'
#' @return A data frame containing the rate and validity of the output.
#'
#' @examples
#' \dontrun{
#' a <- 10
#' b <- 100
#'
#' get_single_rt(a, b)
#' }
#'
#' @keywords internal
get_single_rt <- function(x, y) {
  # either input is NA
  if (is.na(x) | is.na(y)) {
    result <- data.frame(
      rate = NA_real_,
      status = "has_NA"
    )
    return(result)

    # numerator is 0
  } else if (y == 0) {
    result <- data.frame(
      rate = NA_real_,
      status = "denom_0"
    )
    return(result)

    # both inputs are finite numbers
  } else if (is.finite(x) & is.finite(y)) {
    result <- data.frame(
      rate = x / y,
      status = "success"
    )
    return(result)
  }
}
