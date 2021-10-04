#' Perform indirect rate standardization
#'
#' @description Compute a standardized mortality ratio (SMR) or expected number
#' of deaths given the number of observed deaths, the size of each stratum in the
#' observed population, and the specific rate for each stratum in the standard
#' population.
#'
#' @param count A scalar, the number of observed deaths.
#' @param popn A numeric vector containing the size of each defined population.
#' @param std_rt A numeric vector containing the specific rate (expressed as
#' the number of cases per unit of population) for each stratum in the standard
#' population.
#' @param output_type If `"ratio"` (default), only the SMR is returned. If `"counts"`,
#' the expected number of deaths for each stratum in the observed population is
#' returned as a vector.
#' @param percent If `FALSE` (default), the SMR is returned as is (e.g. 0.5). If `TRUE`, the
#' SMR is expressed as a percentage (e.g. 50).
#'
#' @details This low-level function assumes that the size of each stratum in the
#' observed population and the specific rate for the respective stratum in the
#' standard population are provided in the same order.
#'
#' @return Either the SMR as a scalar or an array of the expected counts for
#' each stratum.
#'
#' @examples
#' \dontrun{
#' count <- 1500
#' popn <- c(150000, 55000, 12000)
#' std_rt <- c(0.001, 0.003, 0.05)
#'
#' get_smr(count, popn, std_rt)
#' get_smr(count, popn, std_rt, output_type = "ratio")
#' get_smr(count, popn, std_rt, output_type = "counts")
#' get_smr(count, popn, std_rt, percent = FALSE)
#' get_smr(count, popn, std_rt, percent = TRUE)
#' }
#'
#' @importFrom magrittr %>%
#' @export
get_smr <- function(count, popn, std_rt, output_type = "ratio", percent = FALSE){

  # validate inputs

  if (!is.numeric(count)) {
    stop("`count` must be numeric")
  } else if (length(count) != 1) {
    stop("`count` must be a scalar")
  }

  if (length(popn) != length(std_rt)) {
    stop("lengths of `popn` and `std_rt` must match")
  }

  if (!is.numeric(popn)) {
    stop("`popn` must be a numeric vector")
  } else if (!is.numeric(std_rt)) {
    stop("`std_rt` must be a numeric vector")
  }

  if (!(output_type == "ratio" | output_type == "counts")) {
    stop('`output_type` must be either "ratio" or "counts"')
  }

  if (!percent %in% c(TRUE, FALSE)) {
    stop("`percent` must be one of TRUE or FALSE")
  }

  if (output_type == "ratio") {
    if (anyNA(popn)) {
      stop("when calculating the SMR, `popn` should not have any NA values")
    } else if (anyNA(std_rt)) {
      stop("when calculating the SMR, `std_rt` should not have any NA values")
    }
  } else if (output_type == "counts") {
    if (anyNA(popn)) {
      warning("one or more elements in `popn` are NA")
    } else if (anyNA(std_rt)) {
      warning("one or more elements in `std_rt` are NA")
    }
  }

  exp_counts <- popn * std_rt

  # if output type is counts, then exist the function here
  if (output_type == "counts") {
    return(exp_counts)
  } else if (output_type == "ratio") {
    smr <- count / sum(exp_counts)
    if (percent) {
      return(smr * 100)
    } else {
      return(smr)
    }
  }
}
