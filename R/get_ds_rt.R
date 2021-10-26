#' Perform direct standardization
#'
#' @description Compute a directly standardized rate given the counts of events,
#' the size of each defined population and the size of each group of the
#' standard population.
#'
#' @param counts A numeric vector containing the frequency of events.
#' @param popn A numeric vector containing the size of each defined population.
#' @param std_popn A numeric vector containing the size of each group of the
#' standard population.
#' @param scale,power Provide either `scale`, a positive integer, to specify the
#' scale, or `power`, a non-negative integer, to specify the power of 10 to use
#' when expressing the result. If neither argument is supplied, `scale = 1` will
#' be applied.
#' @param output_type If `"rate"` (default), the directly standardized rate is
#' returned. If `"counts"`, the expected number of events for each stratum is
#' returned.
#' @param clean_strata
#'
#'  * `"none"` returns a directly standardized rate if and only if the evaluated
#'  specific rates are free of `NA` and `std_popn` is free of `NA` and 0.
#'  * `"exclude"` removes strata with specific rates of `NA` or `std_popn`
#'  values of `NA` or `0` from the calculation of the directly standardized rate.
#'  `NA` is returned if all strata are excluded from the calculation.
#'  * `"zero"` sets specific rates evaluated to `NA` as 0. In addition, when
#'  `std_popn` contains 0, the respective stratum is excluded from the
#'  calculation of the directly standardized rate. However, when `std_popn`
#'  contains `NA`, the directly standardized rate is evaluated to `NA`. When
#'  `output_type = "counts"`, the expected counts are calculated *after*
#'  specific rates of `NA` have been set to 0. *Confidence intervals cannot be
#'  constructed when this argument is used.*
#' @param dist A string to indicate the type of probability distribution for the
#' confidence interval to follow. Can be either `"normal"`, `"log normal"`, or
#' `"gamma"`. This parameter is case insensitive.
#' @param interval A scalar, between 0 and 1, indicating the width of the
#' confidence interval. For example, for a 95% confidence interval, use
#' `interval = 0.95`.
#' @param method A string specifying the method to use when calculating
#' confidence intervals based on the gamma distribution. Use `"tcz06"` (default)
#' for the method proposed by Tiwari, Clegg, and Zou (2006) and `"ff97"` for the
#' more conservative method proposed by Fay and Feuer (1997). This parameter
#' must be left empty when calculating confidence intervals using other
#' distributions.
#'
#' @details This low-level function assumes that the counts in each stratum,
#' the size of each stratum in the observed population and the size of each
#' stratum in the standard population are provided in the same order.
#'
#' To construct confidence intervals for the rate estimates, arguments must be
#' supplied to both `dist` and `interval`. In general, *the normal distribution
#' should not be used to construct confidence intervals when the standard
#' deviation is greater than a third of the mean*. A warning is raised if this
#' is the case.
#'
#' @return
#' If `output_type = "rate"` (default) and neither `dist` or `interval` is
#' supplied, the directly standardized rate is returned as a numeric vector.
#'
#' If `output_type = "rate"` and both `dist` and `interval` are provided, then a
#' data frame is returned with the columns `rate`, `lower`, `upper` and
#' `interval`.
#'
#' If `output_type = "counts"`, the expected counts are returned as a numeric
#' vector.
#'
#' @references \href{http://documentation.sas.com/doc/en/pgmsascdc/9.4_3.4/statug/statug_stdrate_details.htm}{The STDRATE Procedure}
#'
#' @examples
#' \dontrun{
#' # using vectors
#' counts <- c(10, 7, 6, 4, 2)
#' popn <- c(300, 595, 492, 43, 422)
#' std_popn <- c(807, 355, 379, 244, 781)
#'
#' get_ds_rt(counts, popn, std_popn)
#' get_ds_rt(counts, popn, std_popn, scale = 1000)
#' get_ds_rt(counts, popn, std_popn, power = 3)
#' get_ds_rt(counts, popn, std_popn, output_type = "counts")
#' get_ds_rt(counts, popn, std_popn, clean_strata = "exclude")
#' get_ds_rt(
#'   counts, popn, std_popn, scale = 1000,
#'   dist = "normal", interval = 0.95
#' )
#' get_ds_rt(
#'   counts, popn, std_popn, scale = 100000,
#'   dist = "gamma", interval = 0.9, method = "ff97"
#' )
#'
#' # using a data frame
#' df <- data.frame(counts, popn, std_popn)
#'
#' get_ds_rt(df$counts, df$popn, df$std_popn, scale = 1000)
#' get_ds_rt(df$counts, df$popn, df$std_popn, power = 3)
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
get_ds_rt <- function(counts, popn, std_popn, scale = NULL, power = NULL,
                      output_type = "rate", clean_strata = "none",
                      dist = NULL, interval = NULL, method = NULL) {

  # check validity of inputs

  if (length(unique(purrr::map(list(counts, popn, std_popn), length))) != 1) {
    stop("input vectors must be the same length")
  }

  if (!is.numeric(std_popn)) {
    stop("`std_popn` must be a numeric vector")
  }

  if (any(is.na(std_popn))) {
    warning("at least one element in `std_popn` is NA")
  }

  if (any(std_popn == 0, na.rm = TRUE)) {
    warning("at least one element in `std_popn` is 0")
  }

  if (!output_type %in% c("rate", "counts")) {
    stop("`output_type` must be either 'rate' or 'counts'")
  }

  # set multiplier
  if (!is.null(scale)) {
    multiplier <- scale
  } else if (!is.null(power)) {
    multiplier <- 10**power
  } else {
    multiplier <- 1
  }

  # calculate stratum-specific rates
  spec_rt <- get_spec_rt(counts, popn) %>%
    as.numeric()

  df <- tibble::tibble(counts, popn, std_popn, spec_rt)

  # calculate directly standardized rate based on specified method
  if (clean_strata == "none") {

    # calculate expected counts for each stratum
    df <- df %>%
      dplyr::mutate(exp_counts = .data$spec_rt * .data$std_popn)

    # check if true rate can be calculated
    n_valid_rows <- df %>%
      dplyr::filter(!is.na(spec_rt), std_popn > 0) %>%
      nrow()

    # only calculate rate if all strata are valid
    if (n_valid_rows == nrow(df)) {
      df_dsr <- df %>%
        dplyr::summarise(
          dsr_n = sum(.data$exp_counts),
          dsr_d = sum(.data$std_popn),
          dsr = .data$dsr_n / .data$dsr_d
        ) %>%
        dplyr::select(.data$dsr)
    } else {
      df_dsr <- tibble::tibble(dsr = NA_real_)
    }
  } else if (clean_strata == "exclude") {

    # calculate expected counts for each stratum
    df <- df %>%
      dplyr::mutate(exp_counts = .data$spec_rt * .data$std_popn)

    # exclude rows where specific rate is NA or standard population is 0 or NA
    df_excl <- df %>%
      dplyr::filter(!is.na(spec_rt), std_popn > 0)

    # calculate rate
    if (!plyr::empty(df_excl)) {
      df_dsr <- df_excl %>%
        dplyr::summarise(
          dsr_n = sum(.data$exp_counts),
          dsr_d = sum(std_popn),
          dsr = .data$dsr_n / .data$dsr_d
        ) %>%
        dplyr::select(.data$dsr)
    } else {
      (df_dsr <- tibble::tibble(dsr = NA_real_))
    }
  } else if (clean_strata == "zero") {
    # set specific rate of NA to 0 then calculate expected counts
    df <- df %>%
      dplyr::mutate(spec_rt = dplyr::if_else(is.na(spec_rt), 0, spec_rt)) %>%
      dplyr::mutate(exp_counts = .data$spec_rt * .data$std_popn)

    df_dsr <- df %>%
      dplyr::summarise(
        dsr_n = sum(.data$exp_counts),
        dsr_d = sum(.data$std_popn),
        dsr = .data$dsr_n / .data$dsr_d
      ) %>%
      dplyr::select(.data$dsr)
  } else {
    stop("`clean_strata` must be one of 'none', 'exclude' or 'zero'")
  }

  if (xor(is.null(dist), is.null(interval))) {
    stop("both `dist` and `interval` must be supplied to construct confidence intervals")
  }

  if (!is.null(dist) & !is.null(interval)) {
    if (clean_strata == "zero") {
      stop("construction of confidence interval with clean_strata = 'zero' not yet implemented")
    }

    # check validity of distribution name calculate confidence interval
    dist_name <- get_dist_name(dist)
    variance <- get_ds_rt_var(df)
    if (dist_name == "normal") {
      df_dsr <- df_dsr %>%
        dplyr::mutate(
          get_ci_norm(interval = interval, estimate = .data$dsr, variance = variance)
        )
    } else if (dist_name == "lognormal") {
      df_dsr <- df_dsr %>%
        dplyr::mutate(
          get_ci_norm(interval = interval, estimate = .data$dsr, variance = variance, log = TRUE)
        )
    } else if (dist_name == "gamma") {
      weights <- df %>%
        dplyr::mutate(w_j = (std_popn / sum(std_popn)) * (1 / popn)) %>%
        dplyr::pull(w_j) %>%
        list()

      df_dsr <- df_dsr %>%
        dplyr::mutate(get_ci_gamma(interval = interval, estimate = .data$dsr, weights = weights,
                                   variance = variance, method = method))
    } else {
      stop("`dist` should be one of 'normal', 'log normal', or 'gamma'")
    }

    # clean output
    df_dsr <- df_dsr %>%
      dplyr::mutate(interval = interval) %>%
      dplyr::select(.data$dsr, .data$lower, .data$upper, .data$interval) %>%
      dplyr::mutate(dplyr::across(.cols = c(.data$lower, .data$upper), ~ .x * multiplier)) # apply multiplier to lower and upper limits
  }

  # apply multiplier to rate
  df_dsr <- df_dsr %>%
    dplyr::mutate(dsr = .data$dsr * multiplier)

  # return specified outputs
  if (output_type == "rate") {
    if ("interval" %in% colnames(df_dsr)) {
      result <- df_dsr
    } else {
      result <- df_dsr$dsr # confidence interval not calculated
    }
  } else {
    result <- df$exp_counts
  }

  return(result)
}

#' Calculate the variance for a directly standardized rate
#'
#' @param df A data frame with the columns `popn`, `std_popn`, `spec_rt`
#'
#' @return A variance as a double
#'
#' @examples
#' \dontrun{
#' df <- tibble::tibble(counts, popn, std_popn) %>%
#'   dplyr::mutate(
#'     spec_rt = get_spec_rt(counts, popn),
#'     exp_counts = spec_rt * std_popn
#'   )
#'
#' get_ds_rt_var(df)
#' }
#'
#' @keywords internal
get_ds_rt_var <- function(df) {
  var_n <- df %>%
    dplyr::mutate(
      std_pop_sq = .data$std_popn**2,
      var_spec_rt = .data$spec_rt / .data$popn
    ) %>%
    dplyr::summarise(numerator = sum(.data$std_pop_sq * .data$var_spec_rt)) %>%
    dplyr::pull(.data$numerator)

  var_d <- sum(df$std_popn)**2

  var <- var_n / var_d

  return(var)
}
