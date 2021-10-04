library(tidyverse)

# simulates a study population

# define demographic variables
year <- c(2016:2020)
age_group <- c(
  paste(seq(0, 85, by = 5), seq(4, 89, by = 5), sep = "-"),
  paste0(90, "+")
)
sex <- c("F", "M")

# generate all combinations of demographic variables
population <- expand.grid(
  year = year,
  age_group = age_group,
  sex = sex
)

# randomly generate population size for each set of demographic variables
# run code with provided seed and reset it afterwards
population <- population %>%
  mutate(popn = with_seed(123, sample(10000:50000, nrow(population), replace = TRUE)))

usethis::use_data(population, overwrite = TRUE)
