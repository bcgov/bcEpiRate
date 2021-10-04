library(tidyverse)

# simulates a standard population

# define demographic variables
age_group <- c(
  paste(seq(0, 85, by = 5), seq(4, 89, by = 5), sep = "-"),
  paste0(90, "+")
)
sex <- c("F", "M")

# generate all combinations of demographic variables
std_population <- expand.grid(age_group = age_group, sex = sex)

# randomly generate size of each subgroup of the standard population
# run code with provided seed and reset it afterwards
std_population <- std_population %>%
  mutate(std_popn = with_seed(123, sample(50000:200000, nrow(std_population), replace = TRUE)))

usethis::use_data(std_population, overwrite = TRUE)
