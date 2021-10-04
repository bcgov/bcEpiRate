library(tidyverse)
library(withr)

# simulates number of deaths from a specific cause

# define demographic variables
year <- c(2016:2020)
disease <- paste("Disease", LETTERS[1:5])
age_group <- c(
  paste(seq(0, 85, by = 5), seq(4, 89, by = 5), sep = "-"),
  paste0(90, "+")
)
sex <- c("F", "M")

# generate all combinations of demographic variables
counts <- expand.grid(
  year = year,
  disease = disease,
  age_group = age_group,
  sex = sex
)

# randomly generate death counts for each set of demographic variables
# run code with provided seed and reset it afterwards
counts <- counts %>%
  mutate(counts = with_seed(1234, sample(100, nrow(counts), replace = TRUE)))

usethis::use_data(counts, overwrite = TRUE)
