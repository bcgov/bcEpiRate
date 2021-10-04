#' Number of deaths
#'
#' A data set containing *simulated* number of deaths by disease, year,
#' and demographic characteristics.
#'
#' @format A data frame with 950 rows and 5 variables:
#' \describe{
#'   \item{year}{Year of death}
#'   \item{disease}{Cause of death}
#'   \item{age_group}{5-year age group}
#'   \item{sex}{Sex}
#'   \item{counts}{Number of deaths}
#' }
"counts"

#' Study population size
#'
#' A data set containing the number of people in each subgroup of the *simulated*
#' study population.
#'
#' @format A data frame with 190 rows and 4 variables:
#' \describe{
#'  \item{year}{Year}
#'  \item{age_group}{5-year age group}
#'  \item{sex}{Sex}
#'  \item{popn}{Number of people}
#' }
"population"

#' Standard population size
#'
#' A data set containing the number of people in each subgroup of the *simulated*
#' standard population.
#'
#' @format A data frame with 38 rows and 3 variables:
#' \describe{
#'   \item{age_group}{5-year age group}
#'   \item{sex}{Sex}
#'   \item{std_popn}{Number of people}
#' }
"std_population"

#' Age-specific death rates
#'
#' A data set containing *simulated* age-specific death rates.
#'
#' @format A data frame with 95 rows and 3 variables:
#' \describe{
#'   \item{disease}{Cause of death}
#'   \item{age_group}{5-year age group}
#'   \item{std_rt}{Death rate expressed as number of death per unit of population}
#' }
"death_rates"
