library(tidyverse)
library(withr)

# simulates age-specific death rates for a standard population

# define variables
disease <- paste("Disease", LETTERS[1:5])
age_group <- c(
  paste(seq(0, 85, by = 5), seq(4, 89, by = 5), sep = "-"),
  paste0(90, "+")
)

# generate all combinations of variables
death_rates <- expand.grid(
  disease = disease,
  age_group = age_group
)

# randomly generate death rates for each age group
# run code with provided seed and reset it afterwards
death_rates <- death_rates %>%
  mutate(std_rt = with_seed(123, sample(50, nrow(death_rates), replace = TRUE)),
         std_rt = std_rt * 0.00001)

usethis::use_data(death_rates, overwrite = TRUE)
