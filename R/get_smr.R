get_smr <- function(x, y, z, measure = "rate",
                    output_type = "ratio", percent = FALSE,
                    dist = NULL, interval = NULL){

  # check validity of inputs

  if (length(x) != length(y)) {
    stop("lengths of `x` and `y` must be the same")
  }

  if (!measure %in% c("rate", "risk")) {
    stop("`measure` must be either 'rate' or 'risk'")
  }

  if (!is.numeric(x)) {
    stop("`x` must be a numeric vector of non-negative integers")
  } else if (any(x < 0, na.rm = TRUE)) {
    stop("`x` must be a numeric vector of non-negative integers")
  }

  # check y

  if (!is.numeric(z)) {
    stop("`z` must be a non-negative integer of length 1")
  } else if (length(z) != 1) {
    stop("`z` must be a non-negative integer of length 1")
  } else if (z %% 1 != 0 | z < 0) {
    stop("`z` must be a non-negative integer of length 1")
  }

  if (!output_type %in% c("ratio", "counts")) {
    stop("`output_type` must be either 'ratio' or 'counts'")
  }

  # check percent
  if (!is.logical(percent)) {
    stop("`percent` must be a boolean")
  }

  # check dist and interval (like in other functions)
}


#
#   if (output_type == "ratio") {
#     if (anyNA(popn)) {
#       stop("when calculating the SMR, `popn` should not have any NA values")
#     } else if (anyNA(std_rt)) {
#       stop("when calculating the SMR, `std_rt` should not have any NA values")
#     }
#   } else if (output_type == "counts") {
#     if (anyNA(popn)) {
#       warning("one or more elements in `popn` are NA")
#     } else if (anyNA(std_rt)) {
#       warning("one or more elements in `std_rt` are NA")
#     }
#   }
#
#   exp_counts <- popn * std_rt
#
#   # if output type is counts, then exist the function here
#   if (output_type == "counts") {
#     return(exp_counts)
#   } else if (output_type == "ratio") {
#     smr <- count / sum(exp_counts)
#     if (percent) {
#       return(smr * 100)
#     } else {
#       return(smr)
#     }
#   }
# }
